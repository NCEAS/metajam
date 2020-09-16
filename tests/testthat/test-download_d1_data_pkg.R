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

test_that("accepts EDI data package", {
  # Takes too much time and add load on servers
  skip_on_cran()
  output <- download_d1_data_pkg(
    meta_obj = 'doi:10.6073/pasta/9f2f89e48f9e943f7125d1a335d96eb0',
    path = tempdir()
    )
  expect_true(dir.exists(output[[1]]))
  unlink(output, recursive = TRUE)
})

test_that("accepts Arctic Data Center data package", {
  # Takes too much time and add load on servers
  skip_on_cran()
  output <- download_d1_data_pkg(
    meta_obj = 'doi:10.18739/A2DP3X',
    path = tempdir()
    )
  expect_true(dir.exists(output[[1]]))
  unlink(output, recursive = TRUE)
})
