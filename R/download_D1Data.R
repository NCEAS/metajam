## TODO:: create check_versions function Irene

#' Downloads data from DataOne along with metadata
#'
#' @param data_obj (character) An identifier or url for a DataONE object to download.
#' @param path (character) Path to a directory to download data to
#'
#' @return (character) Path where data is downloaded to
#'
#' @export
download_D1Data <- function(data_obj, path) {
  
  ## TODO:: refine summary_metadata Irene
  
  stopifnot(is.character(data_obj))
  stopifnot(dir.exists(path))
  
  ## Try to get DataONE data_id from data_obj
  data_obj <- utils::URLdecode(data_obj)
  
  # use internal check_version option
  data_versions <- check_version(data_obj, formatType = "data")
  
  if(nrow(data_versions) == 1){
    data_id <- data_versions$identifier
  } else if(nrow(data_versions) > 1){
    data_versions$dateUploaded <- lubridate::ymd_hms(data_versions$dateUploaded)
    data_id <- data_versions$identifier[data_versions$dateUploaded == max(data_versions$dateUploaded)]
  } else {
    stop("The DataOne ID could not be found for ", data_obj)
  }
  #only returns formatType = "data"
  
  ## Set Nodes
  data_nodes <- dataone::resolve(dataone::CNode("PROD"), data_id)
  d1c <- dataone::D1Client("PROD", data_nodes$data$nodeIdentifier[[1]])
  cn <- dataone::CNode()
  
  ## Download Metadata
  meta_id <- dataone::query(
    cn,
    list(q = sprintf('documents:"%s" AND formatType:"METADATA"', data_id),
         #removed -obsoletedBy:* because this should still work for older versions of metadata
         fl = "identifier")) %>% 
    unlist()
  
  if (length(meta_id) == 0) {
    warning("no metadata records found")
    
  } else if (length(meta_id) > 1) {
    warning("multiple metadata records found:\n",
            meta_id,
            "\nThe first record was used")
    meta_id <- meta_id[1]
  }
  
  message("\nDownloading metadata ", meta_id, " ...")
  meta_obj <- tryCatch({dataone::getObject(d1c@mn, meta_id)},
                       error = function(e){NULL})
  message("Download complete")
  metadata_nodes <- dataone::resolve(cn, meta_id)
  
  ## Download Data
  message("\nDownloading data ", data_id, " ...")
  data_sys <- suppressMessages(dataone::getSystemMetadata(d1c@cn, data_id))
  data_path <- gsub("[^a-zA-Z0-9\\.\\-]+", "_", data_id) ## safe name for files using data_id
  new_dir <- file.path(path, data_path)
  dir.create(new_dir)
  out <- dataone::downloadObject(d1c, data_id, path = new_dir)
  message("Download complete")
  
  ## Rename folder based on filename from downloadObject 
  ## Could originally name folder based on sysmeta, but to ensure consistency this may be the better option although not ideal
  ## e.g. want to make sure same behavior when sysmeta filename is null
  filename <- gsub(".*\\/+","", out) ## remove beginning of string
  filename <- gsub("\\.+[^\\.]*$","", filename) ## remove end of string
  file.rename(new_dir, file.path(path, filename))
  new_dir <- file.path(path, filename)
  
  ## Get package level metadata
  if (!is.null(meta_obj)) {
    #workaround since eml2::read_eml currently can't take raw
    xml <- xml2::read_xml(meta_obj)
    xml2::write_xml(xml, file.path(new_dir, paste0(filename, "_full_metadata.xml")))
  }
  
  eml <- tryCatch({emld::as_emld(xml)},  # If eml make EML object
                  error = function(e) {NULL})
  
  ## Get data object metadata
  if (!is.null(eml)) {
    
    ## get entity that contains the metadata for the data object
    entities <- c("dataTable", "spatialRaster", "spatialVector", "storedProcedure", "view", "otherEntity")
    entities <- entities[entities %in% names(eml$dataset)]
    entity_objs <- lapply(entities, function (x) eml$dataset[[x]])
    
    which_entity <- unlist(lapply(entity_objs, function(x) grepl(data_id, x$physical$distribution$online$url$url)))
    
    ## if here used because output is different for metadata with 1 vs multiple entities
    if (length(which_entity) == 0) {
      which_entity <-  unlist(lapply(unlist(entity_objs , recursive = FALSE),
                                     function(x) grepl(data_id, x$physical$distribution$online$url$url)))
    }
    entity_data <- entity_objs[which_entity]
    
    if (length(entity_data) == 0) {
      warning("No data metadata could not be found for ", data_obj)
      
    } else if (length(entity_data) > 1) {
      warning("multiple data metadata records found:\n",
              data_obj,
              "\nThe first record was used")
    }
    
    entity_data <- entity_data[[1]]
    attributeList <- suppressWarnings(eml2::get_attributes(entity_data$attributeList, eml))
    
    ## write attributes
    ## TODO:: fix file names
    if (nrow(attributeList$attributes) > 0) {
      utils::write.csv(x = attributeList$attributes,
                       file = file.path(new_dir, paste0(filename, "_attribute_metadata.csv")))
    }
    
    if (is.null(attributeList$factors)) {
      utils::write.csv(x = attributeList$factors,
                       file = file.path(new_dir, paste0(filename, "_attribute_factor_metadata.csv")))
    }
    
    ## TODO:: Collect fields more selectively
    entity_meta <- list(
      Date_Downloaded = paste0(Sys.time()),
      Name = entity_data$entityName,
      Data_ID = data_id,
      Data_URL = data_nodes$data$url,
      Metadata_ID = meta_id,
      Metadata_URL = metadata_nodes$data$url[1],
      Description = entity_data$entityDescription,
      Label = entity_data$entityLabel,
      Project_Location = eml$dataset$coverage$geographicCoverage$geographicDescription,
      Project_WestBoundingCoordinate = eml$dataset$coverage$geographicCoverage$boundingCoordinates$westBoundingCoordinate,
      Project_EastBoundingCoordinate = eml$dataset$coverage$geographicCoverage$boundingCoordinates$eastBoundingCoordinate,
      Project_NorthBoundingCoordinate = eml$dataset$coverage$geographicCoverage$boundingCoordinates$northBoundingCoordinate,
      Project_SouthBoundingCoordinate = eml$dataset$coverage$geographicCoverage$boundingCoordinates$southBoundingCoordinate,
      Project_Title = eml$dataset$title,
      Project_Abstract = paste0(eml$dataset$abstract)
    )
    entity_meta <- t(as.data.frame(entity_meta[!sapply(entity_meta, is.null)]))
    utils::write.csv(x = entity_meta,
                     file = file.path(new_dir, paste0(filename, "_summary_metadata.csv")))
    
  }
  
  ## Output full xml file
  return(new_dir)
}