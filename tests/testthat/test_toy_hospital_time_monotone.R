test_that("route toy bundle proposals are not before entity$last_time", {
  bundle <- route_toy_bundle()

  # Construct an entity under the bundle schema and advance time
  p <- fluxCore::Entity$new(
    init   = list(status = "at_depot", payload_kg = 0, deliveries_completed = 0L),
    schema = bundle$schema
  )
  p$update(time = 0.2, event_type = "INIT", changes = list())

  evs <- bundle$propose_events(p, ctx = list(), process_ids = NULL, current_proposals = NULL)
  expect_true(length(evs) >= 1)

  for (ev in evs) {
    expect_true(ev$time_next >= p$last_time)
  }
})
