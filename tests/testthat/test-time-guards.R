library(testthat)
library(patientSimOrchestrate)

test_that("orchestrated_bundle rejects calendar time proposals", {
  bad_model <- list(
    name = "bad",
    schema = list(),
    propose_events = function(patient, ctx = NULL, process_ids = NULL, current_proposals = NULL) {
      list(p = list(event_type = "x", time_next = as.Date("2000-01-01")))
    },
    transition = function(patient, event, ctx = NULL) list(),
    stop = function(patient, event = NULL, ctx = NULL) FALSE
  )

  b <- orchestrated_bundle(models = list(bad = bad_model))

  p <- patientSimCore::new_patient(init = list(), schema = b$schema, time0 = 0)

  expect_error(
    b$propose_events(p, ctx = list(time = list(unit = "days"))),
    "Calendar time inputs are out of scope",
    fixed = TRUE
  )
})
