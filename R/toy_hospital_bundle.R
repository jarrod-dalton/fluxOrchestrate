#' Toy hospitalization episode bundle (for orchestration demos and tests)
#'
#' This intentionally minimal model alternates between an outpatient state and an inpatient
#' state by proposing either an admission event or a discharge event.
#'
#' The bundle is designed to demonstrate orchestration patterns, not clinical realism.
#' It also provides a lightweight second model for patientSimOrchestrate unit tests.
#'
#' Default behavior (if `ctx$hosp_params` is not provided):
#' - Admit time is generated as current_time + rexp(1, rate = 1 / admit_wait_mean)
#' - Length of stay is generated as rexp(1, rate = 1 / los_mean)
#'
#' @param hosp_params Optional list with `admit_wait_mean` and `los_mean` (in model time units).
#' @return A ModelBundle-compatible list.
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
    type = "continuous",
    default = NA_real_
  ),
  next_discharge_time = list(
    type = "continuous",
    default = NA_real_
  )
)

  Toy hospitalization episode bundle (for orchestration demos and tests)
#'
#' This intentionally minimal model alternates between an outpatient state and an inpatient
#' state by proposing either an admission event or a discharge event.
#'
#' The bundle is designed to demonstrate orchestration patterns, not clinical realism.
#' It also provides a lightweight second model for patientSimOrchestrate unit tests.
#'
#' Default behavior (if `ctx$hosp_params` is not provided):
#' - Admit time is generated as current_time + rexp(1, rate = 1 / admit_wait_mean)
#' - Length of stay is generated as rexp(1, rate = 1 / los_mean)
#'
#' @param hosp_params Optional list with `admit_wait_mean` and `los_mean` (in model time units).
#' @return A ModelBundle-compatible list.
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
    type = "continuous",
    default = NA_real_
  ),
  next_discharge_time = list(
    type = "continuous",
    default = NA_real_
  )
)

  .get_time <- function(patient) {
    # patientSimCore does not treat 'time' as a schema var. Time is managed separately.
    # Prefer patient$state() if available, else fall back to patient$as_list() without 'time'.
    if (!is.null(patient$state)) {
      st <- patient$state()
      if (!is.null(st) && "time" %in% names(st)) return(st$time)
      if (!is.null(st) && !is.null(st$current) && "time" %in% names(st$current)) return(st$current$time)
    }
    # last resort: try snapshot_at_time? not appropriate; default to 0
    0
  }

  propose_events <- function(patient, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    st <- patient$as_list(c("care_mode", "next_admit_time", "next_discharge_time"))
    t_now <- (if (is.null(patient$last_time)) 0 else as.numeric(patient$last_time))
    mode  <- st$care_mode

    want_pid <- function(pid) is.null(process_ids) || pid %in% process_ids

    # Option A: reuse cached proposal if still valid
    if (!is.null(current_proposals) && length(current_proposals)) {
      if (mode == "outpatient" && !is.null(current_proposals[["admission"]]) && want_pid("admission")) {
        p <- current_proposals[["admission"]]
        if (!is.null(p$time_next) && is.finite(p$time_next) && p$time_next > t_now) return(list(admission = p))
      }
      if (mode == "inpatient" && !is.null(current_proposals[["discharge"]]) && want_pid("discharge")) {
        p <- current_proposals[["discharge"]]
        if (!is.null(p$time_next) && is.finite(p$time_next) && p$time_next > t_now) return(list(discharge = p))
      }
    }

    params <- hosp_params
    if (!is.null(ctx) && is.list(ctx) && !is.null(ctx$hosp_params)) {
      params <- modifyList(params, ctx$hosp_params)
    }

    if (identical(mode, "outpatient")) {
      if (!want_pid("admission")) return(list())
      t_admit <- st$next_admit_time
      if (!is.finite(t_admit)) t_admit <- t_now + stats::rexp(1, rate = 1 / params$admit_wait_mean)
      list(admission = list(event_type = "admit", time_next = t_admit))
    } else {
      if (!want_pid("discharge")) return(list())
      t_disc <- st$next_discharge_time
      if (!is.finite(t_disc)) t_disc <- t_now + stats::rexp(1, rate = 1 / params$los_mean)
      list(discharge = list(event_type = "discharge", time_next = t_disc))
    }
  }

  transition <- function(patient, event, ctx = NULL) {
    t_now <- (if (is.null(patient$last_time)) 0 else as.numeric(patient$last_time))

    params <- hosp_params
    if (!is.null(ctx) && is.list(ctx) && !is.null(ctx$hosp_params)) {
      params <- modifyList(params, ctx$hosp_params)
    }

    if (!is.list(event) || is.null(event$event_type)) stop("Toy hospital event must have event_type.")
    et <- event$event_type

    if (identical(et, "admit")) {
      los <- stats::rexp(1, rate = 1 / params$los_mean)
      t_disc <- max(t_now, event$time_next) + los
      list(
        care_mode = "inpatient",
        in_hospital = "1",
        next_discharge_time = t_disc,
        next_admit_time = NA_real_
      )
    } else if (identical(et, "discharge")) {
      wait <- stats::rexp(1, rate = 1 / params$admit_wait_mean)
      t_admit <- max(t_now, event$time_next) + wait
      list(
        care_mode = "outpatient",
        in_hospital = "0",
        next_admit_time = t_admit,
        next_discharge_time = NA_real_
      )
    } else {
      stop(sprintf("Unknown toy hospital event_type '%s'.", et))
    }
  }

  stop <- function(patient, event = NULL, ctx = NULL) FALSE

  list(
    name = "hospital_toy_bundle",
    schema = schema,
    propose_events = propose_events,
    transition = transition,
    stop = stop
  )
}
