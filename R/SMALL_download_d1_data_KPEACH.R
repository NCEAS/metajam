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

download_d1_data <- function(data_url, path) {
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

    meta_raw <- rawToChar(meta_obj)

    #Here we assume that these are the only two types of possible metadata..that's probably not smart
    #"eml://ecoinformatics.org/eml"
    #"http://www.isotc211.org/"

    if (grepl("eml://ecoinformatics.org/eml-", meta_raw) == FALSE) {
      warning("Metadata is in ISO format")
      new_dir <- mini_download_ISO_data(meta_raw, path) # add iso function here
    } else if (grepl("eml://ecoinformatics.org/eml-", meta_raw) == TRUE) {
      warning("Metadata is in EML format")
      new_dir <- mini_download_EML_data(meta_obj, path)
    }
  }
  ## Output folder name
  return(new_dir)
}
