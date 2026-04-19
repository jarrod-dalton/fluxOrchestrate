test_that("priority_pid encodes and parse_priority_pid decodes", {
  pid <- priority_pid(7, "ascvd", "main")
  parts <- parse_priority_pid(pid)
  expect_equal(parts$priority, 7L)
  expect_equal(parts$model_id, "ascvd")
  expect_equal(parts$sub_pid, "main")
})
