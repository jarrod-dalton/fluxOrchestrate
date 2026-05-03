test_that("eligible_models suppresses charge model when courier is on_delivery", {
  # A model that must never be called when status == 'on_delivery'.
  bad_model <- list(
    name = "bad_model",
    schema = list(
      status = list(type = "categorical", levels = c("at_depot", "on_delivery"),
                    default = "at_depot", coerce = as.character)
    ),
    propose_events = function(entity, sim_ctx = NULL, param_ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      stop("bad_model should not be called when on_delivery")
    },
    transition = function(entity, event, sim_ctx = NULL, param_ctx = NULL) list(),
    stop      = function(entity, event = NULL, sim_ctx = NULL, param_ctx = NULL) FALSE
  )

  route <- route_toy_bundle(route_params = list(pickup_wait_mean = 1, delivery_duration_mean = 0.5))

  b <- orchestrated_bundle(
    models = list(route = route, bad = bad_model),
    policy = list(
      eligible_models = function(entity, sim_ctx = NULL, param_ctx = NULL) {
        status <- entity$as_list("status")$status
        if (identical(status, "on_delivery")) return("route")
        c("route", "bad")
      }
    )
  )

  # Start on_delivery: bad_model must not fire.
  p <- fluxCore::Entity$new(
    init   = list(status = "on_delivery", payload_kg = 5, deliveries_completed = 0L),
    schema = b$schema,
    time0  = 0
  )

  props <- b$propose_events(p)
  expect_true(length(props) > 0)
  expect_true(all(grepl("|route|", names(props), fixed = TRUE)))
})
