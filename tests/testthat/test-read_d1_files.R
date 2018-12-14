context("read_d1_files()")

test_that("accepts correct inputs", {
  expect_error(read_d1_files(7))
  expect_error(read_d1_files(c("test", "test2")))
  expect_error(read_d1_files(""))
  expect_error(read_d1_files("test"))
  expect_error(read_d1_files("inst/extdata", 7))
  expect_error(read_d1_files("inst/extdata", c("test", "test2")))
  expect_error(read_d1_files("inst/extdata", ""))
})

test_that("test read in csv files", {
  pkg <- read_d1_files(system.file("extdata/test_data", package = "metajam"))
  names <- names(pkg)

  expect_true("attribute_metadata" %in% names)
  expect_true("summary_metadata" %in% names)
  expect_true("data" %in% names)

  expect_true(is.data.frame(pkg$attribute_metadata))
  expect_true(is.data.frame(pkg$summary_metadata))
  expect_true(is.data.frame(pkg$data))
})
