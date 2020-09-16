#' Download data and metadata from DataONE
#'
#' Downloads a data object from DataONE along with metadata.
#'
#' @param data_url (character) An identifier or URL for a DataONE object to download.
#' @param path (character) Path to a directory to download data to.
#' @param dir_name (character) (Optional) Desired name for the folder containing the downloaded data. Defaults to the data file name.
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
#' @importFrom tidyr pivot_wider
#' @importFrom utils URLdecode
#' @importFrom rlang .data
#'
#' @export
#'
#' @seealso [read_d1_files()] [download_d1_data_pkg()]
#'
#' @examples
#' \dontrun{
#' soi_moist_path <- download_d1_data(
#'                      data_url = "urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570",
#'                      path = tempdir())
#' download_d1_data(
#'     data_url = "https://cn.dataone.org/cn/v2/resolve/urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570",
#'     path = tempdir(),
#'     dir_name = "test"
#'     )
#' }
#'

download_d1_data <- function(data_url, path, dir_name = NULL) {
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
    warning("no metadata records found", immediate. = T)
    meta_id <- NULL
  } else if (length(meta_id) > 1) {
    warning("multiple metadata records found:\n",
            paste(meta_id, collapse = "\n"),
            "\nThe first record was used", immediate. = T)
    meta_id <- meta_id[1]
  }

  ## Get package level metadata -----------
  if (!is.null(meta_id)) {
    message("\nDownloading metadata ", meta_id, " ...")
    meta_obj <- dataone::getObject(d1c@mn, meta_id)
    message("Download metadata complete")
    metadata_nodes <- dataone::resolve(cn, meta_id)

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
      warning("No data metadata could be found for ", data_url, immediate. = T)

    } else {

      if (length(entity_data) > 1) {
      warning("Multiple data metadata records found:\n",
              data_url,
              "\nThe first record was used", immediate. = T)
      }

      entity_data <- entity_data[[1]]
    }

    # Test for the case a dataTable entity does not have attribute level metadata
    if (!is.null(entity_data$attributeList)){
      attributeList <- suppressWarnings(EML::get_attributes(entity_data$attributeList, eml))
    }

    meta_tabular <- tabularize_eml(eml) %>% tidyr::pivot_wider(names_from = name, values_from = value)

    ## Summary metadata from EML (combine with general metadata later)
    entity_meta <- suppressWarnings(list(
      Metadata_ID = meta_id[[1]],
      Metadata_URL = metadata_nodes$data$url[1],
      Metadata_EML_Version = stringr::str_extract(meta_tabular$eml.version, "\\d\\.\\d\\.\\d"),
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

  }

  # Write files & download data--------
  message("\nDownloading data ", data_id, " ...")
  data_sys <- suppressMessages(dataone::getSystemMetadata(d1c@cn, data_id))

  data_name <- data_sys@fileName %|||% ifelse(exists("entity_data"), entity_data$physical$objectName %|||% entity_data$entityName, NA) %|||% data_id
  data_name <- gsub("[^a-zA-Z0-9. -]+", "_", data_name) #remove special characters & replace with _
  data_extension <- gsub("(.*\\.)([^.]*$)", "\\2", data_name)
  data_name <- gsub("\\.[^.]*$", "", data_name) #remove extension
  meta_name <- gsub("[^a-zA-Z0-9. -]+", "_", meta_id) #remove special characters & replace with _

  # Check for backwards compatibility.
  old_dir_path <- file.path(path, paste0(meta_name, "__", data_name, "__", data_extension))

  log_path <- file.path(path, "metajam.log")
  old_dir_path_log <- ""

  # Check to see if log exists (ie some data has been download at this location).
    # If it does, then get the version number downloaded, and save it into old_dataid
  if(file.exists(log_path)){
    old_log <- suppressMessages(readr::read_csv(log_path))
    if(data_id %in% old_log$Data_ID){
      old_dir_path_log <- old_log %>%
        dplyr::filter(.data$Data_ID == data_id) %>%
        utils::tail(1) %>%
        dplyr::pull(.data$Location)
    }
  }

  # If the latest version is already downloaded, then exit the function
    # first condition checks old naming scheme, second condition checks log (so folder can have any name)
  if(dir.exists(old_dir_path)){
    stop(paste("This dataset has already been downloaded here:", old_dir_path, "Please delete or move the folder to download the dataset again.", sep = "\n"))
  } else if(dir.exists(old_dir_path_log)) {
    stop(paste("This dataset has already been downloaded here:", old_dir_path_log, "Please delete or move the folder to download the dataset again.", sep = "\n"))
  }

  # Name the new folder, checking for optional parameter dir_name
  if(!is.null(dir_name)){
    new_dir <- file.path(path, dir_name)
  } else {
    new_dir <- old_dir_path
  }

  # Check to make sure we don't overwrite old versions of the data.
    # If the directory they're trying to create already exists, then exit the function.
  if(dir.exists(new_dir)){
    warning(paste("An old version of the data exists at the specified path:", new_dir, "Please change dir_name or delete/move the folder to download a new version of the data", sep = "\n"))
    return(new_dir)
  } else{
    dir.create(new_dir)
  }


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

    entity_meta_combined <- c(entity_meta_general, entity_meta) %>% unlist() %>% tibble::enframe()
    readr::write_csv(entity_meta_combined,
                     file.path(new_dir, paste0(data_name, "__summary_metadata.csv")))
  } else {
    entity_meta_general <- entity_meta_general %>% unlist() %>% tibble::enframe()
    readr::write_csv(entity_meta_general,
                     file.path(new_dir, paste0(data_name, "__summary_metadata.csv")))
  }

  # write attribute tables if data metadata exists
  if (exists("attributeList")) {
    if (nrow(attributeList$attributes) > 0) {
      atts <- attributeList$attributes %>% dplyr::mutate(metadata_pid = meta_id)
      readr::write_csv(atts,
                       file.path(new_dir, paste0(data_name, "__attribute_metadata.csv")))
    }

    if (!is.null(attributeList$factors)) {
      facts <- attributeList$factors %>% dplyr::mutate(metadata_pid = meta_id)
      readr::write_csv(facts,
                       file.path(new_dir, paste0(data_name, "__attribute_factor_metadata.csv")))
    }
  }


  # Write to log file (log file's name and path is log_path)
  data_to_log <- data.frame(Date_Run = paste0(Sys.time()),
                            Data_ID = data_id,
                            Dataset_URL = data_nodes$data$url,
                            Location = new_dir)

  # If the log file doesn't exist yet, then create it.
  if(file.exists(log_path)){
    readr::write_csv(data_to_log, path = log_path, append = TRUE)
  } else {
    readr::write_csv(data_to_log, path = log_path, append = FALSE, col_names = TRUE)
  }

  ## Output folder name
  return(new_dir)
}

