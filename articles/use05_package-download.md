# Use Case 5 - Downloading Entire Data Packages Using DOIs

## Summary

This vignette aims to showcase a use case when the user wants to
download all the datasets of a data package using `metajam` -
`download_d1_data_pkg`.

In this example we use a csv file storing packages returned after
searching for `soil bulk density` on [Arctic Data
Center](https://arcticdata.io/) and
[KNB](https://knb.ecoinformatics.org/) data repositories.

## Libraries and constants

``` r

# devtools::install_github("NCEAS/metajam")
library(metajam)  
library(readr)
library(purrr)
```

``` r

# Directory to save the data set
path_folder <- "./Soil_bulk"

# URL to read the search results stored as a csv on Google Drive
csv_search_results_url <- "https://drive.google.com/uc?export=download&id=1WTLP2BcXCXmUyv4kmntyhuPfrBNdPIqV"
```

## Download all the datasets from data packages using DOIs

``` r

# Create the local directory to store data sets
dir.create(path_folder, showWarnings = FALSE)

# Read the data listing from Google Drive: https://drive.google.com/open?id=1WTLP2BcXCXmUyv4kmntyhuPfrBNdPIqV
data_listing <- read_csv(csv_search_results_url)


### Download the data and metadata ----

# Create the list of unique dois
dois <- unique(data_listing$identifier)

# batch download the datasets
data_folders <- map(dois, ~download_d1_data_pkg(.x, path_folder))
```
