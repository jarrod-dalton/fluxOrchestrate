library(testthat)
library(fluxOrchestrate)

test_that("orchestrated_bundle rejects calendar time proposals", {
  bad_model <- list(
    name = "bad",
    schema = list(),
    propose_events = function(entity, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      list(p = list(event_type = "x", time_next = as.Date("2000-01-01")))
    },
    transition = function(entity, event, ctx = NULL) list(),
    stop = function(entity, event = NULL, ctx = NULL) FALSE
  )

  b <- orchestrated_bundle(models = list(bad = bad_model))

  p <- fluxCore::new_entity(init = list(), schema = b$schema, time0 = 0)

  expect_error(
    b$propose_events(p, ctx = list(time = list(unit = "days"))),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )
})
