#' Read data along with metadata into your R environment based on dowload_d1_data file structure
#'
#' @param folder_path (character) Path to a directory where data and metadata are located
#' @param fnc (character) Function to be used to read the data (default is readr::read_csv)
#' @param ... Parameters to pass onto the function specified in \code{fnc}
#'
#' @import purrr
#' @import readr
#' @import stringr
#' @importFrom stats setNames
#'
#' @return (list) named list containing data and metadata as data frames
#' @export
#'
#' @examples
#' \dontrun{
#' data_folder <- system.file("extdata", "test_data", package = "metajam")
#' soil_moist_data <- read_d1_files(data_folder)
#' 
#' # You can specify the function you would like to use to read the file, as well as passing parameters
#' soil_moist_data_skipped <- read_d1_files(data_folder, "read.csv", skip = 8, stringsAsFactors=FALSE)
#' }
#'

read_d1_files <- function(folder_path, fnc = "read_csv", ...) {
  
  files <- list.files(folder_path, full.names = TRUE)
  # files <- files[!grepl(pattern='full_metadata.xml', files)]
  filename <- files[grepl(pattern='__summary_metadata.csv', files)]
  filename <- gsub(pattern='__summary_metadata.csv','', basename(filename), fixed = TRUE)
  
  if (sum(filename == tools::file_path_sans_ext(basename(files))) > 1 ){
    stop("You have multiple files named ", filename)
    }
  
  data_meta <- purrr::map(files, function(x){
    if (grepl("[^_]+_metadata(?=\\.csv)", basename(x), perl = TRUE)) {
      readr::read_csv(x)
    } else if (tools::file_path_sans_ext(basename(x)) == filename) {
      eval(parse(text = paste0(fnc, '("', x, '", ...)')))
    }
  }) 
  data_meta_names <- purrr::map(files, function(x){
    if (grepl("[^_]+_metadata(?=\\.csv)", basename(x), perl = TRUE)) {
      stringr::str_extract(basename(x), "[^_]+_metadata(?=\\.csv)")
    } else if (tools::file_path_sans_ext(basename(x)) == filename){
      "data"
    }
  }) 
  data_meta <- stats::setNames(data_meta, data_meta_names) %>%
    purrr::compact()
  return(data_meta)
}
