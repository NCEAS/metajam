#' Download and read data along with metadata into your R environment
#' 
#' This is a convenience function to both download and read data and metadata
#' into your R environment.
#'
#' @param data_url (character) An identifier or URL for a DataONE object to download.
#' @param path (character) Path to a directory to download data to.
#' @param fnc (character) Function to be used to read the data (default is `readr::read_csv()`).
#' @param ... Parameters to pass onto the function specified in `fnc`.
#'
#' @return (list) A named list containing data and metadata as data.frames.
#' 
#' @export
#' 
#' @seealso [download_d1_data()] [read_d1_files()]
#'
#' @examples
#' \dontrun{
#' dl_read_d1_data("urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570", path = "./Data")
#' }

dl_read_d1_data <- function(data_url, path, fnc = "read_csv", ...) {
  stopifnot(is.character(data_url) & nchar(data_url) > 0)
  stopifnot(is.character(path) & nchar(path) > 0 & dir.exists(path))

  dir <- download_d1_data(data_url, path)
  
  read_d1_files(dir, fnc = "read_csv", ...)
}
