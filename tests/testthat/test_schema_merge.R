test_that("merge_schemas_strict errors on conflicts", {
  s1 <- list(x = list(type="continuous", default=0))
  s2 <- list(x = list(type="continuous", default=1))
  expect_error(merge_schemas_strict(s1, s2), "Schema conflict")
})

test_that("merge_schemas_strict defers schema validation to Core", {
  # Missing required schema metadata (type) should error via patientSimCore::schema_validate
  bad <- list(x = list(default = 0))
  expect_error(merge_schemas_strict(bad), "must define \\$type")
})
