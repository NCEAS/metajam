
#' Downloads all the datasets of a data package from DataONE
#'
#' @param meta_obj (character) A DOI for a DataONE package to download
#' @param path (character) Path to a directory to download data to
#'
#' @import purrr
#'
#' @return (character) Path where data is downloaded to
#' @export
#'
#' @examples 
#' \dontrun{
#' download_d1_data_pkg("doi:10.18739/A2028W")
#' download_d1_data_pkg("https://doi.org/10.18739/A2028W")
#' }
#' 

download_d1_data_pkg <- function(meta_obj, path) {
  stopifnot(is.character(meta_obj))
  meta_obj <- utils::URLdecode(meta_obj)
  # check to see if there's a new version of the package and throw warning
  pids <- get_pkg_pids(meta_obj)
  dir_list <- purrr::map(pids$data, ~download_d1_data(.x, path))
  return(dir_list)
}




