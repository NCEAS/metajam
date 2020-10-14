context("get_pkg_pids()")

test_that('data package report is not "data"', {
  # Takes too much time and add load on servers
  skip_on_cran()

  output <- suppressMessages(
    get_pkg_pids(
      pkg_doi = 'https://doi.org/10.6073/pasta/08abf45e6ef1585ad7ee1e00fb9d7dc1'
    )
  )
  expect_true(
    !any(stringr::str_detect(output$data, 'report'))
  )
})

