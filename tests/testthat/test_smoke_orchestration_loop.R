test_that("orchestrated delivery bundle can advance through multiple events with gating", {
  # Helper: retrieve current time in a Core-compatible way
  get_time <- function(entity) {
    if (is.null(entity$last_time)) return(NA_real_)
    entity$last_time
  }

  # Primary model: pickup/dropoff delivery route.
  route <- route_toy_bundle(route_params = list(pickup_wait_mean = 0.5, delivery_duration_mean = 0.3))

  # Secondary model: charge battery at depot. Suppressed when on_delivery by policy.
  charge <- list(
    name = "charge",
    schema = list(
      battery_pct = list(type = "percent", default = 80, coerce = as.numeric)
    ),
    propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      t_now <- if (is.null(entity$last_time)) 0 else as.numeric(entity$last_time)
      list(charge = list(event_type = "charge", time_next = t_now + 0.4))
    },
    transition = function(entity, event, ctx = NULL) {
      if (!identical(event$event_type, "charge")) return(list())
      batt <- as.numeric(entity$as_list("battery_pct")$battery_pct)
      list(battery_pct = min(100, batt + 10))
    },
    stop = function(entity, event = NULL, ctx = NULL) FALSE
  )

  b <- orchestrated_bundle(
    models = list(route = route, charge = charge),
    policy = list(
      eligible_models = function(entity, ctx = NULL) {
        status <- entity$as_list("status")$status
        if (identical(status, "on_delivery")) return("route")
        c("route", "charge")
      },
      event_priority = function(proposal, entity, ctx = NULL) {
        if (proposal$event_type %in% c("pickup", "dropoff")) return(10L)
        200L
      }
    )
  )

  p <- fluxCore::Entity$new(
    init   = list(status = "at_depot", payload_kg = 0, deliveries_completed = 0L, battery_pct = 80),
    schema = b$schema,
    time0  = 0
  )

  times    <- c()
  statuses <- c()

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
    p$update(time = ev$time_next, event_type = ev$event_type, changes = upd)

    times    <- c(times, get_time(p))
    statuses <- c(statuses, p$as_list("status")$status)
  }

  expect_true(all(diff(times) >= -1e-12))
  expect_true(any(statuses == "on_delivery"))
})
