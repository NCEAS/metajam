context("get_pkg_pids()")

test_that('data package report is not "data"', {
  # Takes too much time and add load on servers
  skip_if_offline()

  output <- suppressMessages(
    get_pkg_pids(
      pkg_doi = 'doi:10.6073/pasta/25b39ca1bb9853a9ed3148ef64a2c1d5'
    )
  )
  expect_true(
    !any(stringr::str_detect(output$data, 'report'))
  )
})

