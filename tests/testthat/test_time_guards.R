library(testthat)
library(fluxOrchestrate)

test_that("orchestrated_bundle rejects calendar time proposals", {
  bad_model <- list(
    name = "bad",
    schema = list(
      status = list(type = "categorical", levels = c("at_depot", "on_delivery"),
                    default = "at_depot", coerce = as.character)
    ),
    propose_events = function(entity, sim_ctx = NULL, param_ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      list(p = list(event_type = "x", time_next = as.Date("2000-01-01")))
    },
    transition = function(entity, event, sim_ctx = NULL, param_ctx = NULL) list(),
    stop = function(entity, event = NULL, sim_ctx = NULL, param_ctx = NULL) FALSE
  )

  b <- orchestrated_bundle(models = list(bad = bad_model))

  p <- fluxCore::Entity$new(init = list(), schema = b$schema, time0 = 0)

  expect_error(
    b$propose_events(p, sim_ctx = list(time = list(unit = "days"))),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )
})
