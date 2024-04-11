context("download_EML_data()")

#adding a test for a dataset that uses EML metadata from Arctic Data Center member node
test_that("test data URL with eml metadata from the Arctic Data Center member node and with multiple data tables", {

  temp_dir <- tempdir()
  data_id <- "urn:uuid:4b5b35cf-7a39-449f-90f1-93fb3e6fb242"
  meta_id <- "doi:10.18739/A2S17ST50"
  data_nodes <- dataone::resolve(dataone::CNode("PROD"), data_id)
  d1c <- dataone::D1Client("PROD", data_nodes$data$nodeIdentifier[[1]])
  cn <- dataone::CNode()
  metadata_nodes <- dataone::resolve(cn, meta_id)
  meta_obj <- dataone::getObject(d1c@mn, meta_id)
  meta_raw <- rawToChar(meta_obj)
  out <- download_ISO_data(meta_raw, meta_obj, meta_id, data_id, metadata_nodes, temp_dir)

  files <- list.files(out)

  expect_equal(length(files), 3)
  # expect_true(any(stringr::str_detect(files, "attribute_metadata.csv")))
  expect_true(any(stringr::str_detect(files, "full_metadata.xml")))
  expect_true(any(stringr::str_detect(files, "summary_metadata.csv")))

  folder_name <- stringr::str_extract(out, "[^/]*$")
  # expect_true(stringr::str_detect(folder_name, "^doi")) #starts with doi

  # remove files
  unlink(temp_dir, recursive = TRUE)
})
