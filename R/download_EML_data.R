#' Download data and metadata from a dataset that uses EML metadata.
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
#' @param meta_obj (character) A metadata object produced by download_d1_data. This is a different format than the metadata object required for the analogous ISO function
#' @param meta_id (character) A metadata identifier produced by download_d1_data
#' @param data_id (character) A data identifier produced by download_d1_data
#' @param metadata_nodes (character) The member nodes where this metadata is stored, produced by download_d1_data
#' @param mn (character) The member node carried over from the download_d1_function
#' @param path (character) Path to a directory to download data to.

download_EML_data <- function(meta_obj, meta_id, data_id, metadata_nodes, mn, path) {


    eml <- tryCatch({emld::as_emld(meta_obj, from = "xml")},  # If eml make EML object
                    error = function(e) {NULL})

    # Get attributes ----------
    ## get entity that contains the metadata for the data object
    entities <- c("dataTable", "spatialRaster", "spatialVector", "storedProcedure", "view", "otherEntity")
    entities <- entities[entities %in% names(eml$dataset)]

    entity_objs <- purrr::map(entities, ~EML::eml_get(eml, .x)) %>%
      # restructure so that all entities are at the same level
      purrr::map_if(~!is.null(.x$entityName), list) %>%
      unlist(recursive = FALSE)

    #sometimes url is stored in ...online$url instead of ...online$url$url
    #sometimes url needs to be decoded
    entity_data <- entity_objs %>%
      purrr::keep(~any(grepl(data_id,
                             purrr::map_chr(.x$physical$distribution$online$url, utils::URLdecode))))

    if (length(entity_data) == 0) {
      warning("No data metadata could be found for ", data_url)

    } else {

      if (length(entity_data) > 1) {
        warning("Multiple data metadata records found:\n",
                data_url,
                "\nThe first record was used")
      }

      entity_data <- entity_data[[1]]
    }

    attributeList <- suppressWarnings(EML::get_attributes(entity_data$attributeList, eml))

    meta_tabular <- tabularize_eml(eml) %>% tidyr::spread(name, value)
    metadata_url <- metadata_nodes$data$baseURL[[1]]

    ## Summary metadata from EML (combine with general metadata later)
    entity_meta <- suppressWarnings(list(
      Metadata_ID = meta_id,
      Metadata_URL =  metadata_url,
      Metadata_Version = stringr::str_extract(meta_tabular$eml.version, "\\d\\.\\d\\.\\d"), #removed the word EML from this feature name
      File_Description = entity_data$entityDescription,
      File_Label = entity_data$entityLabel,
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



  # Write files & download data--------
  message("\nDownloading data ", data_id, " ...")
  cn <- dataone::CNode()
  mn <- dataone::getMNode(cn, metadata_nodes$data$nodeIdentifier[[1]])
  pid <- data_id
  data_sys <- suppressMessages(dataone::getSystemMetadata(mn, pid))

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
  } else {
    entity_meta_general <- entity_meta_general %>% unlist() %>% enframe()
    readr::write_csv(entity_meta_general,
                     file.path(new_dir, paste0(data_name, "__summary_metadata.csv")))
  }

  # write attribute tables if data metadata exists
  if (exists("attributeList")) {
    if (length(attributeList$attributes) > 0) {
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

