context("dl_read_d1_data()")

test_that("correct inputs are given", {
  expect_error(dl_read_d1_data(7, 7))
  expect_error(dl_read_d1_data("", 7))
  expect_error(dl_read_d1_data("", ""))
  expect_error(dl_read_d1_data("test", ""))
  expect_error(dl_read_d1_data("test", "test"))
})
