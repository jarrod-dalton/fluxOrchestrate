# ------------------------------------------------------------------------------
# route_toy_bundle()
#
# Purpose:
#   Minimal urban food delivery model bundle used for orchestration demos/tests.
#   The bundle alternates between at-depot and on-delivery states by proposing
#   pickup and dropoff events.
#
# Domain:
#   A courier starts at the depot, picks up an order, delivers it, and returns.
#   A companion "charge" model (defined inline in tests) can fire charge events
#   at the depot and is suppressed by orchestration policy when on delivery.
#
# Time semantics (LOCKED):
#   fluxCore treats time as engine metadata, not a schema state var.
#   The canonical time reference is `entity$last_time`.
# ------------------------------------------------------------------------------

#' Create a toy courier route model bundle
#'
#' Returns a minimal demonstration bundle for an urban food courier that
#' alternates between depot and on-delivery states via pickup and dropoff events.
#' Designed to pair with an inline charging model in orchestration tests.
#'
#' @param route_params Optional list of parameters:
#'   - `pickup_wait_mean`: mean hours waiting at depot before next pickup (default `1.0`)
#'   - `delivery_duration_mean`: mean delivery duration in hours (default `0.5`)
#'
#' @return A model bundle list with `schema`, `propose_events`, `transition`,
#'   and `stop` functions, compatible with `fluxCore::Engine`.
#' @export
route_toy_bundle <- function(route_params = NULL) {
  if (is.null(route_params)) {
    route_params <- list(pickup_wait_mean = 1.0, delivery_duration_mean = 0.5)
  }

  schema <- list(
    status = list(
      type = "categorical",
      levels = c("at_depot", "on_delivery"),
      default = "at_depot",
      coerce = as.character
    ),
    payload_kg = list(
      type = "nonnegative_numeric",
      default = 0,
      coerce = as.numeric
    ),
    deliveries_completed = list(
      type = "count",
      default = 0L,
      coerce = as.integer
    )
  )

  propose_events <- function(entity, sim_ctx = NULL, param_ctx = NULL, process_ids = NULL, current_proposals = NULL) {
    st <- entity$as_list("status")
    t_now <- if (is.null(entity$last_time)) 0 else as.numeric(entity$last_time)
    status <- st$status

    want_pid <- function(pid) is.null(process_ids) || pid %in% process_ids

    # Use param_ctx if available; param_ctx should contain route_params.
    params <- route_params
    if (!is.null(param_ctx) && is.list(param_ctx) && !is.null(param_ctx$route_params)) {
      params <- utils::modifyList(params, param_ctx$route_params)
    }

    if (identical(status, "at_depot")) {
      if (!want_pid("pickup")) return(list())
      t_pickup <- t_now + stats::rexp(1, rate = 1 / params$pickup_wait_mean)
      list(pickup = list(event_type = "pickup", time_next = t_pickup))
    } else {
      if (!want_pid("dropoff")) return(list())
      t_dropoff <- t_now + stats::rexp(1, rate = 1 / params$delivery_duration_mean)
      list(dropoff = list(event_type = "dropoff", time_next = t_dropoff))
    }
  }

  transition <- function(entity, event, sim_ctx = NULL, param_ctx = NULL) {
    if (identical(event$event_type, "pickup")) {
      list(
        status     = "on_delivery",
        payload_kg = 5
      )
    } else if (identical(event$event_type, "dropoff")) {
      n <- as.integer(entity$as_list("deliveries_completed")$deliveries_completed)
      list(
        status               = "at_depot",
        payload_kg           = 0,
        deliveries_completed = n + 1L
      )
    } else {
      list()
    }
  }

  stop <- function(entity, event = NULL, sim_ctx = NULL, param_ctx = NULL) FALSE

  list(
    name          = "route",
    schema        = schema,
    propose_events = propose_events,
    transition    = transition,
    stop          = stop
  )
}
