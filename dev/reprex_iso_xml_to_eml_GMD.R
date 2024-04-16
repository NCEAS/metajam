
## Try to get DataONE data_id from data_url for file named 	nga_TGX201809_seabird_processed_densities_L1.csv---------
data_url <- utils::URLdecode("https://cn.dataone.org/cn/v2/resolve/a0b7cf1a-bdbf-407e-be30-4c4ebd7d2dfc")
data_versions <- metajam::check_version("https://cn.dataone.org/cn/v2/resolve/a0b7cf1a-bdbf-407e-be30-4c4ebd7d2dfc", formatType = "data")

if (nrow(data_versions) == 1) {
  data_id <- data_versions$identifier
} else if (nrow(data_versions) > 1) {
  #get most recent version
  data_versions$dateUploaded <- lubridate::ymd_hms(data_versions$dateUploaded)
  data_id <- data_versions$identifier[data_versions$dateUploaded == max(data_versions$dateUploaded)]
} else {
  stop("The DataONE ID could not be found for ", data_url)
}

## Set Nodes ------------
data_nodes <- dataone::resolve(dataone::CNode("PROD"), data_id)
d1c <- dataone::D1Client("PROD", data_nodes$data$nodeIdentifier[[1]])
cn <- dataone::CNode()

## Download Metadata ------------
meta_id2 <- dataone::query(
  cn,
  list(q = sprintf('documents:"%s" AND formatType:"METADATA" AND -obsoletedBy:*', data_id),
       fl = "identifier")) %>%
  unlist()


meta_raw <- rawToChar(dataone::getObject(d1c@mn, meta_id2))

#Produces a List of 16, incomplete metadata
eml_from_xml_raw <- emld::as_emld(meta_raw, from = "xml")

#getObject returns a raw format for we can use the XML package to make it look like XML again
meta_iso_xml <- XML::xmlTreeParse(meta_raw)

#flaxml will squish the metadata into a dataframe...which seems to be the preferred input for conver_metadata
xml.dataframe <- flatxml::fxml_importXMLFlat("https://cn.dataone.org/cn/v2/resolve/10.24431_rw1k45w_2020_7_2_23391")

geometa_eml <- geometa::convert_metadata(xml.dataframe, from = "geometa|iso-19115-1", to = "eml",
                                     mappings = geometa::getMappings(), verbose = FALSE)
