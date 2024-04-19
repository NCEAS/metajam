#' Download data and metadata from a dataset that uses ISO metadata.
#'
#' This is an internal function called by the download_d1_data.R function. Not to be exported
#' @import dataone
#' @import EML
#' @import purrr
#' @import readr
#' @importFrom emld as_emld
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_extract
#' @importFrom tidyr spread
#' @importFrom utils URLdecode
#'
#' @param meta_raw (character) A raw metadata object produced by download_d1_data
#' @param meta_obj (character) A metadata object produced by download_d1_data
#' @param meta_id (character) A metadata identifier produced by download_d1_data
#' @param data_id (character) A data identifier produced by download_d1_data
#' @param metadata_nodes (character) The member nodes where this metadata is stored, produced by download_d1_data
#' @param path (character) Path to a directory to download data to.
#'
#' @keywords internal
#'


download_ISO_data <- function(meta_raw, meta_obj, meta_id, data_id, metadata_nodes, path) {
  # Silence 'visible bindings' note
  name <- value <- entity_data <- NULL

  meta_iso_xml <- XML::xmlTreeParse(meta_raw)

  eml <- tryCatch({emld::as_emld(meta_obj, from = "xml")},  # If eml make EML object
                  error = function(e) {NULL})

  metadata <- eml %>%
    unlist() %>%
    tibble::enframe()

  metadata2 <- meta_iso_xml %>%
    unlist() %>%
    tibble::enframe()


ISO_type <- metadata2 %>% filter(name == "doc.children.MD_Metadata.children.metadataStandardName.children.CharacterString.children.text.value")

 metadata <- metadata %>%
   mutate(value = ifelse(name == "@type", ISO_type$value ,value ))


  metadata <- metadata %>%
    dplyr::mutate(name = dplyr::case_when(
      grepl("@type", name) ~ "xml.version",
      grepl("title", name) ~ "title",
      grepl("individualName", name) ~ "people",
      grepl("abstract", name) ~ "abstract",
      grepl("identificationInfo.MD_DataIdentification.descriptiveKeywords.MD_Keywords.keyword.CharacterString", name) ~ "keyword",
      grepl("doc.children.MD_Metadata.children.metadataStandardName.children.CharacterString.children.text.value", name) ~ "Metadata_ISO_Version",
      grepl("geographicDescription", name) ~ "geographicCoverage.geographicDescription",
      grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.geographicElement.EX_GeographicBoundingBox.westBoundLongitude.Decimal", name) ~ "geographicCoverage.westBoundingCoordinate",
      grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.geographicElement.EX_GeographicBoundingBox.eastBoundLongitude.Decimal", name) ~ "geographicCoverage.eastBoundingCoordinate",
      grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.geographicElement.EX_GeographicBoundingBox.northBoundLatitude.Decimal", name) ~ "geographicCoverage.northBoundingCoordinate",
      grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.geographicElement.EX_GeographicBoundingBox.southBoundLatitude.Decimal", name) ~ "geographicCoverage.southBoundingCoordinate",
      grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.temporalElement.EX_TemporalExtent.extent.TimePeriod.beginPosition", name) ~ "temporalCoverage.beginDate",
      grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.temporalElement.EX_TemporalExtent.extent.TimePeriod.endPosition", name) ~ "temporalCoverage.endDate",
      grepl("dataQualityInfo.DQ_DataQuality.report.DQ_ConceptualConsistency.evaluationMethodDescription.CharacterString", name) ~ "methods",
      grepl("objectName", name) ~ "objectName",
      grepl("online.url", name) ~ "url",
      grepl("dataQualityInfo.DQ_DataQuality.lineage.LI_Lineage.statement.CharacterString", name) ~ "methods"
    )) %>%
    dplyr::filter(!is.na(name)) %>%
    dplyr::mutate(value = stringr::str_trim(value)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(value = paste(value, collapse = "; "), .groups = "drop") %>%
    dplyr::mutate(value = gsub("\n", "", value))  #without this, fields get truncated in Excel


  meta_tabular <- tidyr::pivot_wider(data = metadata,
                                     names_from = name,
                                     values_from = value)
  metadata_url <- metadata_nodes$data$baseURL[[1]]

  ## Summary metadata from EML (combine with general metadata later)
  entity_meta <- suppressWarnings(list(
    Metadata_ID = meta_id,
    Metadata_URL =  metadata_url,
    Metadata_Version = meta_tabular$xml.version,
    File_Description = NA,
    File_Label = NA,
    Dataset_URL = paste0("https://search.dataone.org/#view/", meta_id),
    Dataset_Title = meta_tabular$title,
    Dataset_StartDate = meta_tabular$temporalCoverage.beginDate,
    Dataset_EndDate = meta_tabular$temporalCoverage.endDate,
    Dataset_Location = meta_tabular$geographicCoverage.geographicDescription,
    Dataset_WestBoundingCoordinate = meta_tabular$geographicCoverage.westBoundingCoordinate,
    Dataset_EastBoundingCoordinate = meta_tabular$geographicCoverage.eastBoundingCoordinate,
    Dataset_NorthBoundingCoordinate = meta_tabular$geographicCoverage.northBoundingCoordinate,
    Dataset_SouthBoundingCoordinate = meta_tabular$geographicCoverage.southBoundingCoordinate,
    Dataset_Taxonomy = meta_tabular$taxonomicCoverage,
    Dataset_Abstract = meta_tabular$abstract,
    Dataset_Methods = meta_tabular$methods,
    Dataset_People = meta_tabular$people
  ))


  # Write files & download data--------
  message("\nDownloading data ", data_id, " ...")
  cn <- dataone::CNode()
  data_nodes <- dataone::resolve(dataone::CNode("PROD"), data_id)
  d1c <- dataone::D1Client("PROD", data_nodes$data$nodeIdentifier[[1]])
  pid <- data_id
  data_sys <- suppressMessages(dataone::getSystemMetadata(d1c@mn, pid))

  data_name <- data_sys@fileName %|||% ifelse(exists("entity_data"), entity_data$physical$objectName %|||% entity_data$entityName, NA) %|||% data_id
  data_name <- gsub("[^a-zA-Z0-9. -]+", "_", data_name) #remove special characters & replace with _
  data_extension <- gsub("(.*\\.)([^.]*$)", "\\2", data_name)
  data_name <- gsub("\\.[^.]*$", "", data_name) #remove extension
  data_name <- gsub(" ", "", data_name) # remove spaces
  meta_name <- gsub("[^a-zA-Z0-9. -]+", "_", meta_id) #remove special characters & replace with _

  # Also remove periods from various objects
  meta_name <- gsub(pattern = "\\.", replacement = "_", x = meta_name)
  data_name <- gsub(pattern = "\\.", replacement = "_", x = data_name)
  data_extension <- gsub(pattern = "\\.", replacement = "_", x = data_extension)

  # Assemble a new folder name
  new_dir <- file.path(path, paste(meta_name, data_name, data_extension,
                                   sep = "__"))

  # Create a counter
  k <- 1

  # If this folder already exists, make a new folder
  while(dir.exists(new_dir)){

    # Make a *new* new folder
    new_dir <- file.path(path, paste(meta_name, data_name, data_extension,
                                     paste0("copy_", k), sep = "__"))

    # Increment counter
    k <- k + 1 }

  # Make the folder (if it doesn't exist already)
  dir.create(new_dir, showWarnings = FALSE)

  ## download Data
  data_nodes <- dataone::resolve(dataone::CNode("PROD"), data_id)
  d1c <- dataone::D1Client("PROD", data_nodes$data$nodeIdentifier[[1]])
  out <- dataone::downloadObject(d1c, data_id, path = new_dir)
  message("Download complete")

  # change downloaded data object name to data_name
  data_files <- list.files(new_dir, full.names = TRUE)
  data_files_ext <- stringr::str_extract(data_files, ".[^.]{1,4}$")
  file.rename(data_files, file.path(new_dir, paste0(data_name, data_files_ext)))

  entity_meta_general <- list(File_Name = data_name,
                              Date_Downloaded = paste0(Sys.time()),
                              Data_ID = data_id,
                              Data_URL = data_nodes$data$url[[1]]
  )

  ## write metadata xml/tabular form if exists
  if (exists("eml")) {
    EML::write_eml(eml, file.path(new_dir, paste0(data_name, "__full_metadata.xml")))

    entity_meta_combined <- c(entity_meta_general, entity_meta) %>% unlist() %>% enframe()
    readr::write_csv(entity_meta_combined,
                     file.path(new_dir, paste0(data_name, "__summary_metadata.csv")))
  } else {entity_meta_general <- entity_meta_general %>% unlist() %>% enframe()
  readr::write_csv(entity_meta_general,
                   file.path(new_dir, paste0(data_name, "__summary_metadata.csv")))
  }

  ## Output folder name
  return(new_dir)
}
