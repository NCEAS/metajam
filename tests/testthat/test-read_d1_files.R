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

test_that("read Arctic Data Center data package", {
  # Takes too much time and add load on servers
  skip_on_cran()

  # Call download_di_data_pkg() for data package
  paths <- suppressMessages(
    download_d1_data_pkg(
      meta_obj = 'doi:10.18739/A2DP3X',
      path = tempdir()
    )
  )
  # Read data and metadata
  output <- suppressMessages(
    read_d1_files(
      folder_path = paths[[2]]
    )
  )
  # Test output class
  expect_true(class(output) == 'list')
  # Test output list object names
  expect_true(
    all(
      names(output) %in% c('attribute_metadata', 'summary_metadata', 'data', 'factor_metadata')
    )
  )
  # Clean up
  unlink(paths, recursive = TRUE)
})


test_that("read EDI data package", {
  # Takes too much time and add load on servers
  skip_on_cran()

  # Call download_di_data_pkg() for data package
  paths <- suppressMessages(
    download_d1_data_pkg(
      meta_obj = 'doi:10.6073/pasta/9f2f89e48f9e943f7125d1a335d96eb0',
      path = tempdir()
    )
  )
  # Read data and metadata
  output <- suppressMessages(
    read_d1_files(
      folder_path = paths[[1]]
    )
  )
  # Test output class
  expect_true(class(output) == 'list')
  # Test output list object names
  expect_true(
    all(
      names(output) %in% c('attribute_metadata', 'summary_metadata', 'data')
    )
  )
  # Clean up
  unlink(paths, recursive = TRUE)
})
