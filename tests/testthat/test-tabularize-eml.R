context("tabularize_eml()")

test_that("parse summary table correctly", {
  eml <- system.file("extdata", "test_data", "SoilMois2012_2017__full_metadata.xml",
                     package = "metajam")
  metadata <- tabularize_eml(eml)
  expect_equal(dim(metadata), c(16,2))
})

test_that("parse full table correctly", {
  eml <- system.file("extdata", "test_data", "SoilMois2012_2017__full_metadata.xml",
                     package = "metajam")
  metadata <- tabularize_eml(eml, full = TRUE)
  expect_equal(dim(metadata), c(189,2))
})

test_that("test fails on non xml file", {
  eml <- system.file("extdata", "test_data", "SoilMois2012_2017.csv",
                     package = "metajam")
  expect_error(tabularize_eml(eml, full = TRUE))
})
