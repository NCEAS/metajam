## TODO:: create check_versions function Irene

#' Downloads data from DataOne along with metadata
#'
#' @param data_obj (character) An identifier or url for a DataONE object to download.
#' If the object is a metadata or resource map object, all data associated with that object will downloaded.
#' @param path (character) Path to a directory to download data to
#'
#' @return (character) Path where data is downloaded to
#'
#' @export
download_D1Data <- function(data_obj, path) {
  
  ## TODO:: illegal charcaters in filenames? check how filenames are made Mitchell
  ## TODO:: refine naming, folder = DOI_filename, all else = filename_* Mitchell
  ## TODO:: refine summary_metadata Irene
  
  stopifnot(is.character(data_obj))
  stopifnot(dir.exists(path))
  
  ## Try to get DataONE data_id from data_obj
  data_id <- utils::URLdecode(data_obj)
  cn <- dataone::CNode("PROD")
  q_fmt <- '%s:"%s"' ## query format
  fl <- "identifier, formatType, obsoletedBy, entityName"  ## query fields
  
  while(nchar(data_id) > 0) {
    results <- suppressMessages(
      dataone::query(cn,
                     list(q = sprintf(q_fmt, "identifier", data_id),
                          fl = fl))
    )
    
    if (length(results) == 0) {
      data_id <- gsub("^[^\\/=]+[\\/=]*", "", data_id)
      
    } else {
      if (length(results) > 1) {
        stop("A unique DataOne ID could not be found for ", data_obj)
      }
      
      results <- unlist(results, recursive = FALSE)
      
      ## TODO:: use check_versions here maybe
      if (length(results$obsoletedBy) > 0) {
        warning(data, "has been obsoletedBy ", results$obsoletedBy)
      }
      
      formatType <- results$formatType
      data_id <- results$identifier
      break
    }
    
  }
  
  if (nchar(data_id) == 0) {
    stop("The DataOne ID could not be found for ", data_obj)
  }
  
  ## Set Node
  data_nodes <- dataone::resolve(cn, data_id)
  d1c <- dataone::D1Client("PROD", data_nodes$data$nodeIdentifier[[1]])
  
  ## If data_obj is not data, return data associated with either the resource map or metadata.
  ## Else get metadata associated with data
  if (formatType %in% c("RESOURCE", "METADATA")) {
    test <- ifelse(formatType == "RESOURCE", "resourceMap", "isDocumentedBy")
    results <- dataone::query(cn,
                              list(q = paste(sprintf(q_fmt, test, data_id),
                                             'AND formatType:"DATA"'),
                                   fl = "identifier, fileName"))
    
    results <- unlist(sapply(seq_along(results), function(i){
      if (i > 5) {
        NULL
      } else {
        x <- results[[i]]
        paste0("\n", x$fileName, "\n", x$identifier, "\n")
      }
    }))
    
    stop("\n", data_obj, "\nis not a data object.\n",
         "Use one of the following data objects associated with \n", data_obj, "\n",
         results,
         "\n(only the first five shown)")
    
    ## Get resource, metadata, and data information for obj
  } else if (formatType == "DATA") {
    results <- dataone::query(cn,
                              list(q = paste(sprintf(q_fmt, "documents", data_id),
                                             'AND formatType:"METADATA" AND -obsoletedBy:*'),
                                   fl = "identifier"))
    
  } else {
    stop(data_obj, " is not a data object, metadata object, or data package")
  }
  
  ## Download Metadata
  meta_id <- unlist(results)
  message("\nDownloading metadata ", meta_id, " ...")
  if (length(meta_id) == 0 ) {
    warning("no metadata records found")
    
  } else if (length(meta_id) > 1) {
    warning("multiple metadata records found:\n",
            meta_id,
            "\nThe first record was used")
    meta_obj <- dataone::getObject(d1c@mn, meta_id[1])
    
  } else{
    meta_obj <- dataone::getObject(d1c@mn, meta_id)
  }
  message("Download complete")
  metadata_nodes <- dataone::resolve(cn, meta_id)
  
  ## Download Data
  message("\nDownloading data ", data_id, " ...")
  data_sys <- suppressMessages(dataone::getSystemMetadata(d1c@cn, data_id))
  new_dir <- file.path(path, data_id)
  dir.create(new_dir)
  dataone::downloadObject(d1c, data_id, path = new_dir)
  message("Download complete")
  
  ## Get package level metadata
  xml <- xml2::read_xml(rawToChar(meta_obj))
  eml = tryCatch({
    emld::as_emld(xml) # If eml make EML object
    #TODO: update to eml2 when eml2 is stable
  }, error = function(e) {
    NULL
  })
  
  ## Get data object metadata
  if (!is.null(eml)) {
    
    ## get entity that contains the metadata for the data object
    entities <- c("dataTable", "spatialRaster", "spatialVector", "storedProcedure", "view", "otherEntity")
    entities <- entities[entities %in% names(eml$dataset)]
    entity_objs <- sapply(entities, function (x) eml$dataset[[x]])
    which_entity <- sapply(entity_objs, function(x) grepl(data_id, x$physical$distribution$online$url$url))
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
                       file = file.path(new_dir, "attributes.csv"))
    }
    
    if (nrow(attributeList$factors) > 0) {
      utils::write.csv(x = attributeList$factors,
                       file = file.path(new_dir, "factors.csv"))
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
                     file = file.path(new_dir, "select_metadata.csv"))
    
  }
  
  ## Output full xml file
  xml2::write_xml(xml, file.path(new_dir, "full_metadata.xml"))
  return(new_dir)
}