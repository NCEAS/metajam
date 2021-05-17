#' Download data and metadata from DataONE
#'
#' Downloads a data object from DataONE along with metadata.
#'
#' @param data_url (character) An identifier or URL for a DataONE object to download.
#' @param path (character) Path to a directory to download data to.
#'
#' @return (character) Path where data is downloaded to.
#'
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
#' @export
#'
#' @seealso [read_d1_files()] [download_d1_data_pkg()]
#'
#' @examples
#' \dontrun{
#' download_d1_data("urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570", path = "./Data")
#' download_d1_data(
#'    "https://cn.dataone.org/cn/v2/resolve/urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570",
#'     path = "."
#'     )
#' }

download_ISO_data <- function(data_url, path) {
  # TODO: add meta_doi to explicitly specify doi

  stopifnot(is.character(data_url), length(data_url) == 1, nchar(data_url) > 0)
  stopifnot(is.character(path), length(path) == 1, nchar(path) > 0, dir.exists(path))

  ## Try to get DataONE data_id from data_url ---------
  data_url <- utils::URLdecode(data_url)
  data_versions <- check_version(data_url, formatType = "data")

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
  meta_id <- dataone::query(
    cn,
    list(q = sprintf('documents:"%s" AND formatType:"METADATA" AND -obsoletedBy:*', data_id),
         fl = "identifier")) %>%
    unlist()

  # if no results are returned, try without -obsoletedBy
  if (length(meta_id) == 0) {
    meta_id <- dataone::query(
      cn,
      list(q = sprintf('documents:"%s" AND formatType:"METADATA"', data_id),
           fl = "identifier")) %>%
      unlist()
  }

  # depending on results, return warnings
  if (length(meta_id) == 0) {
    warning("no metadata records found")
    meta_id <- NULL
  } else if (length(meta_id) > 1) {
    warning("multiple metadata records found:\n",
            paste(meta_id, collapse = "\n"),
            "\nThe first record was used")
    meta_id <- meta_id[1]
  }

  ## Get package level metadata -----------
  if (!is.null(meta_id)) {
    message("\nDownloading metadata ", meta_id, " ...")
    meta_obj <- dataone::getObject(d1c@mn, meta_id)
    message("Download metadata complete")
    metadata_nodes <- dataone::resolve(cn, meta_id)

    meta_raw <- rawToChar(dataone::getObject(d1c@mn, meta_id))


    eml <- tryCatch({emld::as_emld(meta_obj, from = "xml")},  # If eml make EML object
                    error = function(e) {NULL})


    metadata <- eml %>%
      unlist() %>%
      tibble::enframe()

    metadata <- metadata %>%
      dplyr::mutate(name = dplyr::case_when(
        grepl("schemaLocation", name) ~ "eml.version",
        grepl("title", name) ~ "title",
        grepl("individualName", name) ~ "people",
        grepl("abstract", name) ~ "abstract",
        grepl("identificationInfo.MD_DataIdentification.descriptiveKeywords.MD_Keywords.keyword.CharacterString", name) ~ "keyword",
        grepl("geographicDescription", name) ~ "geographicCoverage.geographicDescription",
        grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.geographicElement.EX_GeographicBoundingBox.westBoundLongitude.Decimal", name) ~ "geographicCoverage.westBoundingCoordinate",
        grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.geographicElement.EX_GeographicBoundingBox.eastBoundLongitude.Decimal", name) ~ "geographicCoverage.eastBoundingCoordinate",
        grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.geographicElement.EX_GeographicBoundingBox.northBoundLatitude.Decimal", name) ~ "geographicCoverage.northBoundingCoordinate",
        grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.geographicElement.EX_GeographicBoundingBox.southBoundLatitude.Decimal", name) ~ "geographicCoverage.southBoundingCoordinate",
        grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.temporalElement.EX_TemporalExtent.extent.TimePeriod.beginPosition", name) ~ "temporalCoverage.beginDate",
        grepl("identificationInfo.MD_DataIdentification.extent.EX_Extent.temporalElement.EX_TemporalExtent.extent.TimePeriod.endPosition", name) ~ "temporalCoverage.endDate",
        grepl("identificationInfo.MD_DataIdentification.descriptiveKeywords.MD_Keywords.keyword.CharacterString", name) ~ "taxonomicCoverage",
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


    meta_tabular <- metadata  %>% tidyr::spread(name, value)

    ## Summary metadata from EML (combine with general metadata later)
    entity_meta <- suppressWarnings(list(
      Metadata_ID = meta_id[[1]],
      Metadata_URL = metadata_nodes$data$url[1],
      Metadata_EML_Version = stringr::str_extract(meta_tabular$eml.version, "\\d\\.\\d\\.\\d"),
      File_Description = NA,
      File_Label = NA,
      Dataset_URL = paste0("https://search.dataone.org/#view/", meta_id[[1]]),
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

  }

  # Write files & download data--------
  message("\nDownloading data ", data_id, " ...")
  data_sys <- suppressMessages(dataone::getSystemMetadata(d1c@cn, data_id))

  data_name <- data_sys@fileName %|||% ifelse(exists("entity_data"), entity_data$physical$objectName %|||% entity_data$entityName, NA) %|||% data_id
  data_name <- gsub("[^a-zA-Z0-9. -]+", "_", data_name) #remove special characters & replace with _
  data_extension <- gsub("(.*\\.)([^.]*$)", "\\2", data_name)
  data_name <- gsub("\\.[^.]*$", "", data_name) #remove extension
  meta_name <- gsub("[^a-zA-Z0-9. -]+", "_", meta_id) #remove special characters & replace with _

  new_dir <- file.path(path, paste0(meta_name, "__", data_name, "__", data_extension))

  # Check if the dataset has already been downloaded at this location. If so, exit the function
  if (dir.exists(new_dir)) {
    warning("This dataset has already been downloaded. Please delete or move the folder to download the dataset again.")
    return(new_dir)
  }

  dir.create(new_dir)

  ## download Data
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

  # write attribute tables if data metadata exists
  if (exists("attributeList")) {
    if (nrow(attributeList$attributes) > 0) {
      atts <- attributeList$attributes %>% mutate(metadata_pid = meta_id)
      readr::write_csv(atts,
                       file.path(new_dir, paste0(data_name, "__attribute_metadata.csv")))
    }

    if (!is.null(attributeList$factors)) {
      facts <- attributeList$factors %>% mutate(metadata_pid = meta_id)
      readr::write_csv(facts,
                       file.path(new_dir, paste0(data_name, "__attribute_factor_metadata.csv")))
    }
  }

  ## Output folder name
  return(new_dir)
}
