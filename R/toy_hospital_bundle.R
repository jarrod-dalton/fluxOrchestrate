# ------------------------------------------------------------------------------
# hospital_toy_bundle()
#
# Purpose:
#   Minimal "hospital episode" model bundle used for orchestration demos/tests.
#   The bundle alternates between outpatient and inpatient states by proposing
#   admission and discharge events.
#
# Time semantics (LOCKED):
#   fluxCore treats time as engine metadata, not a schema state var.
#   The canonical time reference is `entity$last_time`.
# ------------------------------------------------------------------------------

#' Create a toy hospital episode model bundle
#'
#' Returns a minimal demonstration bundle that alternates between outpatient and
#' inpatient care modes using admission/discharge events.
#'
#' @param hosp_params Optional list of toy parameters. Defaults include
#'   `admit_wait_mean` and `los_mean`.
#'
#' @return A model bundle list with schema, propose/transition/stop functions.
#' @export
hospital_toy_bundle <- function(hosp_params = NULL) {
  if (is.null(hosp_params)) hosp_params <- list(admit_wait_mean = 1.0, los_mean = 0.05)

  schema <- list(
    care_mode = list(
      type = "categorical",
      levels = c("outpatient", "inpatient"),
      default = "outpatient"
    ),
    in_hospital = list(
      type = "binary",
      levels = c("0", "1"),
      default = "0"
    ),
    next_admit_time = list(
      type = "numeric",
      default = NA_real_,
      allow_na = TRUE
    ),
    next_discharge_time = list(
      type = "numeric",
      default = NA_real_,
      allow_na = TRUE
    )
  )

  propose_events <- function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    st <- entity$as_list(c("care_mode", "next_admit_time", "next_discharge_time"))
    t_now <- if (is.null(entity$last_time)) 0 else as.numeric(entity$last_time)
    mode <- st$care_mode

    want_pid <- function(pid) is.null(process_ids) || pid %in% process_ids

    # Option A: reuse cached proposal if still valid.
    if (!is.null(current_proposals) && length(current_proposals)) {
      if (identical(mode, "outpatient") && !is.null(current_proposals[["admission"]]) && want_pid("admission")) {
        p <- current_proposals[["admission"]]
        if (!is.null(p$time_next) && is.finite(p$time_next) && p$time_next > t_now) return(list(admission = p))
      }
      if (identical(mode, "inpatient") && !is.null(current_proposals[["discharge"]]) && want_pid("discharge")) {
        p <- current_proposals[["discharge"]]
        if (!is.null(p$time_next) && is.finite(p$time_next) && p$time_next > t_now) return(list(discharge = p))
      }
    }

    params <- hosp_params
    if (!is.null(ctx) && is.list(ctx) && !is.null(ctx$hosp_params)) {
      params <- utils::modifyList(params, ctx$hosp_params)
    }

    if (identical(mode, "outpatient")) {
      if (!want_pid("admission")) return(list())
      t_admit <- st$next_admit_time
      if (!is.finite(t_admit) || t_admit <= t_now) {
        t_admit <- t_now + stats::rexp(1, rate = 1 / params$admit_wait_mean)
      }
      list(admission = list(event_type = "admit", time_next = t_admit))
    } else {
      if (!want_pid("discharge")) return(list())
      t_disc <- st$next_discharge_time
      if (!is.finite(t_disc) || t_disc <= t_now) {
        t_disc <- t_now + stats::rexp(1, rate = 1 / params$los_mean)
      }
      list(discharge = list(event_type = "discharge", time_next = t_disc))
    }
  }

  transition <- function(entity, event, ctx = NULL) {
    t_now <- if (is.null(entity$last_time)) 0 else as.numeric(entity$last_time)

    if (identical(event$event_type, "admit")) {
      list(
        care_mode = "inpatient",
        in_hospital = "1",
        next_admit_time = NA_real_,
        next_discharge_time = t_now + stats::rexp(1, rate = 1 / hosp_params$los_mean)
      )
    } else if (identical(event$event_type, "discharge")) {
      list(
        care_mode = "outpatient",
        in_hospital = "0",
        next_discharge_time = NA_real_,
        next_admit_time = t_now + stats::rexp(1, rate = 1 / hosp_params$admit_wait_mean)
      )
    } else {
      list()
    }
  }

  stop <- function(entity, event = NULL, ctx = NULL) FALSE

  list(
    name = "toy_hospital",
    schema = schema,
    propose_events = propose_events,
    transition = transition,
    stop = stop
  )
}
