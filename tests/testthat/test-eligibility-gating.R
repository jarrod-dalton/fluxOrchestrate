test_that("eligible_models can lock out other models (no propose_events call)", {
  bad_model <- list(
    name = "bad_model",
    schema = list(),
    propose_events = function(patient, ctx=NULL, process_ids=NULL, current_proposals=NULL) {
      stop("bad_model should not be called")
    },
    transition = function(patient, event, ctx=NULL) list(),
    stop = function(patient, event=NULL, ctx=NULL) FALSE
  )

  hosp <- hospital_toy_bundle(hosp_params = list(admit_wait_mean = 1, los_mean = 0.1))

  b <- orchestrated_bundle(
    models = list(hosp = hosp, bad = bad_model),
    policy = list(
      eligible_models = function(patient, ctx=NULL) {
        st <- patient$as_list("care_mode")
        if (st$care_mode == "inpatient") return("hosp")
        c("hosp","bad")
      }
    )
  )

  p <- patientSimCore::new_patient(init = list(care_mode = "inpatient"), schema = b$schema, time0 = 0)

  props <- b$propose_events(p, ctx = list())
  expect_true(length(props) > 0)
  expect_true(all(grepl("|hosp|", names(props), fixed = TRUE)))
})
