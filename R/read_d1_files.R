#' Read data and metadata based on `download_d1_data()` file structure
#'
#' Reads data along with metadata into your R environment based on [download_d1_data()] file structure.
#'
#' @param folder_path (character) Path to a directory where data and metadata are located.
#' @param fnc (character) Function to be used to read the data (default is [readr::read_csv()]).
#' @param ... Parameters to pass into the function specified in `fnc`.
#'
#' @return (list) Named list containing data and metadata as data frames.
#'
#' @import purrr
#' @import readr
#' @importFrom stats setNames
#' @importFrom stringr str_extract
#' @importFrom tools file_path_sans_ext
#'
#' @export
#'
#' @seealso [download_d1_data()] [download_d1_data_pkg()]
#'
#' @examples
#' data_folder <- system.file(file.path("extdata", "test_data"), package = "metajam")
#' soil_moist_data <- read_d1_files(data_folder)
#'
#' # You can specify the function you would like to use to read the file and pass parameters
#' soil_moist_data_skipped <- read_d1_files(data_folder, "read.csv",
#'                                          skip = 8, stringsAsFactors = FALSE)

read_d1_files <- function(folder_path, fnc = "read_csv", ...) {

  # Check inputs and error out for unsupported entries
  stopifnot(is.character(folder_path), length(folder_path) == 1, nchar(folder_path) > 0, dir.exists(folder_path))
  stopifnot(is.character(fnc), length(fnc) == 1, nchar(fnc) > 0)

  # Find files in the specified folder
  files <- list.files(folder_path, full.names = TRUE)
  # files <- files[!grepl(pattern='full_metadata.xml', files)]
  filename <- files[grepl(pattern = '__summary_metadata.csv', files)]
  filename <- gsub(pattern = '__summary_metadata.csv', '.csv', basename(filename), fixed = TRUE)

  # Error out for more than one file of the same name
  if (sum(filename == tools::file_path_sans_ext(basename(files))) > 1 ) {
    stop("You have multiple files named ", filename)
  }

  # Do not attempt to read in XML files
  files_no_meta <- files[tools::file_ext(files) %in% c("xml", "log") == FALSE]

  # Make an empty list
  data_list <- list()

  # Read in each file
  for(single_file in files_no_meta){

    # Identify file path
    sub_name <- basename(single_file)

    # Use user-provided function
    sub_df <- eval(parse(text = paste0(fnc, '("', normalizePath(single_file, winslash = '/'), '", ...)')))

    # Add to list
    data_list[[sub_name]] <- sub_df }

  # Drop empty/NULL elements of list
  data_actual <- purrr::compact(.x = data_list)

  # Return list of data files
  return(data_actual) }
