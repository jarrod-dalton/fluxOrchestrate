test_that("merge_schemas_strict errors on conflicts", {
  s1 <- list(x = list(type="numeric", default=0))
  s2 <- list(x = list(type="numeric", default=1))
  expect_error(merge_schemas_strict(s1, s2), "Schema conflict")
})
