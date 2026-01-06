test_that("orchestrated bundle can advance through multiple events with gating", {
  # Helper: retrieve current time in a Core-compatible way
  get_time <- function(patient) {
    # Time is engine-owned; always use patient$last_time (not part of state()).
    if (is.null(patient$last_time)) return(NA_real_)
    patient$last_time
  }

  hosp <- hospital_toy_bundle(hosp_params = list(admit_wait_mean = 0.5, los_mean = 0.1))

  # Chronic model proposes clinic visits when outpatient; it should be muted inpatient by policy gating
  chronic <- list(
    name = "chronic",
    schema = list(
      next_clinic = list(type = "continuous", default = NA_real_)
    ),
    propose_events = function(patient, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      st <- patient$as_list(c("care_mode", "next_clinic"))
      t_now <- get_time(patient)
      if (st$care_mode == "inpatient") return(list())
      t_next <- st$next_clinic
      if (!is.finite(t_next) || t_next <= t_now) t_next <- t_now + 0.2
      list(clinic = list(event_type = "clinic", time_next = t_next))
    },
    transition = function(patient, event, ctx = NULL) {
      if (event$event_type != "clinic") return(list())
      t_now <- get_time(patient)
      list(next_clinic = t_now + 0.2)
    },
    stop = function(patient, event = NULL, ctx = NULL) FALSE
  )

  b <- orchestrated_bundle(
    models = list(hosp = hosp, chronic = chronic),
    policy = list(
      eligible_models = function(patient, ctx = NULL) {
        mode <- patient$as_list("care_mode")$care_mode
        if (mode == "inpatient") return("hosp")
        c("hosp", "chronic")
      },
      event_priority = function(proposal, patient, ctx = NULL) {
        if (proposal$event_type %in% c("admit", "discharge")) return(10L)
        200L
      }
    )
  )

  p <- patientSimCore::new_patient(init = list(care_mode = "outpatient"), schema = b$schema, time0 = 0)

  times <- c()
  modes <- c()

  for (i in 1:12) {
    props <- b$propose_events(p, current_proposals = NULL)
    expect_true(length(props) > 0)

    tvec <- vapply(props, function(x) {
      if (is.null(x$time_next) || length(x$time_next) != 1) return(NA_real_)
      as.numeric(x$time_next)
    }, numeric(1))

    expect_true(all(is.finite(tvec)), info = "All proposals must include scalar time_next.")

    df <- data.frame(pid = names(props), t = tvec, stringsAsFactors = FALSE)
    df <- df[order(df$t, df$pid), , drop = FALSE]
    pid_next <- df$pid[1]
    ev <- props[[pid_next]]
    ev$process_id <- pid_next

    upd <- b$transition(p, ev)

    # patientSimCore advances time + applies changes via update()
    p$update(time = ev$time_next, event_type = ev$event_type, changes = upd)

    times <- c(times, get_time(p))
    modes <- c(modes, p$as_list("care_mode")$care_mode)
  }

  expect_true(all(diff(times) >= -1e-12))
  expect_true(any(modes == "inpatient"))
})
