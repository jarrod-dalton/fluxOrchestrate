test_that("toy and orchestrated bundles carry one canonical model clock", {
  route <- route_toy_bundle()

  expect_s3_class(route$time_spec, "time_spec")
  expect_identical(route$time_spec$unit, "hours")
  expect_no_error(fluxCore::Engine$new(bundle = route))

  bundle <- orchestrated_bundle(models = list(route = route))

  expect_identical(bundle$time_spec, route$time_spec)
  engine <- fluxCore::Engine$new(bundle = bundle)
  entity <- fluxCore::Entity$new(
    init = list(
      status = "at_depot",
      payload_kg = 0,
      deliveries_completed = 0L
    ),
    schema = bundle$schema,
    time0 = 0
  )

  set.seed(2021)
  out <- engine$run(entity, max_events = 2)

  expect_identical(out$events$event_type, c("init", "pickup", "dropoff"))
  expect_identical(out$entity$current$deliveries_completed, 1L)
})

test_that("orchestration rejects missing or conflicting constituent clocks", {
  route <- route_toy_bundle()
  missing_clock <- route
  missing_clock$time_spec <- NULL

  expect_error(
    orchestrated_bundle(models = list(route = route, missing = missing_clock)),
    "missing or invalid for: missing",
    fixed = TRUE
  )

  daily_route <- route
  daily_route$time_spec <- fluxCore::time_spec(unit = "days")

  expect_error(
    orchestrated_bundle(models = list(route = route, daily = daily_route)),
    "mismatch for: daily",
    fixed = TRUE
  )

  with_clock <- function(bundle, clock) {
    bundle$time_spec <- clock
    bundle
  }
  date_clock <- fluxCore::time_spec(
    unit = "hours",
    origin = as.Date("2020-01-01"),
    zone = "UTC"
  )
  matching_date_clock <- fluxCore::time_spec(
    unit = "hours",
    origin = as.Date("2020-01-01"),
    zone = "UTC"
  )
  date_route <- with_clock(route, date_clock)

  expect_no_error(orchestrated_bundle(models = list(
    first = date_route,
    second = with_clock(route, matching_date_clock)
  )))

  mismatches <- list(
    origin_instant = fluxCore::time_spec(
      unit = "hours",
      origin = as.Date("2020-01-02"),
      zone = "UTC"
    ),
    origin_class = fluxCore::time_spec(
      unit = "hours",
      origin = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
      zone = "UTC"
    ),
    zone = fluxCore::time_spec(
      unit = "hours",
      origin = as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
      zone = "America/New_York"
    )
  )
  for (mismatch_name in names(mismatches)) {
    expect_error(
      orchestrated_bundle(models = list(
        first = date_route,
        conflicting = with_clock(route, mismatches[[mismatch_name]])
      )),
      "mismatch for: conflicting",
      fixed = TRUE,
      info = mismatch_name
    )
  }
})

test_that("route toy parameters are overridden through typed ParamContext params", {
  route <- route_toy_bundle(route_params = list(
    pickup_wait_mean = 1,
    delivery_duration_mean = 0.5
  ))
  entity <- fluxCore::Entity$new(
    init = list(
      status = "at_depot",
      payload_kg = 0,
      deliveries_completed = 0L
    ),
    schema = route$schema,
    time0 = 0
  )
  param_ctx <- fluxCore::ParamContext(
    draw_id = 7L,
    params = list(pickup_wait_mean = 4)
  )

  set.seed(2021)
  expected_time <- stats::rexp(1, rate = 1 / 4)
  set.seed(2021)
  proposal <- route$propose_events(entity, param_ctx = param_ctx)$pickup

  expect_equal(proposal$time_next, expected_time)
})
