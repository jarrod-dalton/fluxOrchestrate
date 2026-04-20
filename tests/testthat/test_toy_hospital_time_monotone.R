test_that("toy hospital bundle proposals are not before entity$last_time", {
  bundle <- hospital_toy_bundle()

  # Construct an entity under the bundle schema and advance time
  p <- fluxCore::new_entity(init = list(care_mode = "outpatient"), schema = bundle$schema)
  p$update(time = 0.2, event_type = "INIT", changes = list())

  evs <- bundle$propose_events(p, ctx = list(), process_ids = NULL, current_proposals = NULL)
  expect_true(length(evs) >= 1)

  for (ev in evs) {
    expect_true(ev$time_next >= p$last_time)
  }
})
