context("download_d1_data_pkg()")

test_that("accepts correct inputs", {
  expect_error(download_d1_data_pkg(7))
  expect_error(download_d1_data_pkg(c("test", "test2")))
  expect_error(download_d1_data_pkg(""))
  expect_error(download_d1_data_pkg("test", 7))
  expect_error(download_d1_data_pkg("test", c("test", "test2")))
  expect_error(download_d1_data_pkg("test", ""))
  expect_error(download_d1_data_pkg("test", "test"))
})
