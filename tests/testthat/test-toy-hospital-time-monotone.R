test_that("toy hospital bundle proposals are not before patient$last_time", {
  # Construct a patient and advance time
  schema <- patientSimCore::default_patient_schema()
  p <- patientSimCore::new_patient(init = list(age = 55), schema = schema)

  # Advance to a nonzero time
  p$update(time = 0.2, event_type = "INIT", changes = list())

  bundle <- hospital_toy_bundle()

  evs <- bundle$propose_events(p, ctx = list(), process_ids = NULL, current_proposals = NULL)
  expect_true(length(evs) >= 1)

  for (ev in evs) {
    expect_true(ev$time_next >= p$last_time)
  }
})
