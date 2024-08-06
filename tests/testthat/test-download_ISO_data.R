context("download_ISO_data()")

#adding a test for a dataset that uses ISO metadata from Research Workspace member node
test_that("test data URL with ISO metadata from Research Workspace member node and with multiple data tables", {
  # Takes too much time and add load on servers
  skip_if_offline()

  temp_dir <- tempdir()
  data_id <- "8181dc0f-25fe-45c2-9bcd-0af54276aa62"
  meta_id <- "10.24431_rw1k5ab_20210709T212300Z"
  data_nodes <- dataone::resolve(dataone::CNode("PROD"), data_id)
  d1c <- dataone::D1Client("PROD", data_nodes$data$nodeIdentifier[[1]])
  cn <- dataone::CNode()
  metadata_nodes <- dataone::resolve(cn, meta_id)
  meta_obj <- dataone::getObject(d1c@mn, meta_id)
  meta_raw <- rawToChar(meta_obj)
  out <- download_ISO_data(meta_raw, meta_obj, meta_id, data_id, metadata_nodes, temp_dir)

  files <- list.files(out)

  expect_equal(length(files), 3)
  expect_true(any(stringr::str_detect(files, "full_metadata.xml")))
  expect_true(any(stringr::str_detect(files, "summary_metadata.csv")))

  folder_name <- stringr::str_extract(out, "[^/]*$")
  # expect_true(stringr::str_detect(folder_name, "^doi")) #starts with doi

  # remove files
  unlink(temp_dir, recursive = TRUE)
})
