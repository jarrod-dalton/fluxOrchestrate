test_that("priority encoding yields stable lexical ordering", {
  pid1 <- priority_pid(1, "m", "a")
  pid2 <- priority_pid(10, "m", "a")
  pid3 <- priority_pid(2, "m", "a")
  expect_true(pid1 < pid3)
  expect_true(pid3 < pid2)
})
