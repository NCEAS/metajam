context("check_version()")

test_that("accepts correct inputs", {
  expect_error(check_version(7))
  expect_error(check_version(""))
  expect_error(check_version("test", 7))
  expect_error(check_version("test", c("test", "test2")))
  expect_error(check_version("test", "test"))
})

test_that("warnings or errors are issued", {
  # Takes too much time and add load on servers
  skip_on_cran()

  out <- check_version("https://cn.dataone.org/cn/v2/resolve/urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570")
  expect_equal(out$formatType, "DATA")

  # Returns a warning if the identifier has been obsoleted
  expect_warning(check_version("doi:10.18739/A2HF7Z", formatType = "metadata"))

  # Returns an error if no matching identifiers are found
  expect_error(check_version("a_test_pid"))
  expect_error(check_version("doi:10.18739/A2ZF6M", formatType = "data"))

  # Returns a warning if several identifiers are returned
  expect_warning(check_version("10.18739/A2057CR99"))
})
