% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_d1_files.R
\name{read_d1_files}
\alias{read_d1_files}
\title{Read data and metadata based on `download_d1_data()` file structure}
\usage{
read_d1_files(folder_path, fnc = "read_csv", ...)
}
\arguments{
\item{folder_path}{(character) Path to a directory where data and metadata are located.}

\item{fnc}{(character) Function to be used to read the data (default is [readr::read_csv()]).}

\item{...}{Parameters to pass into the function specified in `fnc`.}
}
\value{
(list) Named list containing data and metadata as data frames.
}
\description{
Reads data along with metadata into your R environment based on [download_d1_data()] file structure.
}
\examples{
data_folder <- system.file(file.path("extdata", "test_data"), package = "metajam")
soil_moist_data <- read_d1_files(data_folder)

# You can specify the function you would like to use to read the file and pass parameters
soil_moist_data_skipped <- read_d1_files(data_folder, "read.csv",
                                         skip = 8, stringsAsFactors = FALSE)
}
\seealso{
[download_d1_data()] [download_d1_data_pkg()]
}
