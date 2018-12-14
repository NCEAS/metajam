#' Download all data and metadata of a data package from DataONE
#'
#' Downloads all the data objects of a data package from DataONE along with metadata.
#'
#' @param meta_obj (character) A DOI or metadata object PID for a DataONE package to download.
#' @param path (character) Path to a directory to download data to.
#'
#' @return (list) Paths where data are downloaded to.
#'
#' @import purrr
#' @importFrom utils URLdecode
#'
#' @export
#'
#' @seealso [read_d1_files()] [download_d1_data()]
#'
#' @examples
#' \dontrun{
#' download_d1_data_pkg("doi:10.18739/A2028W", ".")
#' download_d1_data_pkg("https://doi.org/10.18739/A2028W", ".")
#' }

download_d1_data_pkg <- function(meta_obj, path) {

  stopifnot(is.character(meta_obj), length(meta_obj) == 1, nchar(meta_obj) > 0)
  stopifnot(is.character(path), length(path) == 1, nchar(path) > 0, dir.exists(path))

  meta_obj <- utils::URLdecode(meta_obj)
  # check to see if there's a new version of the package and throw warning
  pids <- get_pkg_pids(meta_obj)
  dir_list <- purrr::map(pids$data, ~download_d1_data(.x, path))

  return(dir_list)
}
