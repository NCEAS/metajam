# All data sets

# devtools::install_github("DataONEorg/rdataone") ## Version needed not on CRAN as of 2018/06/15
# devtools::install_github("cboettig/eml2")
# devtools::install_github("NCEAS/metajam")
library(metajam)

library(tidyverse)
library(googledrive)
library(googlesheets)

###### CONSTANTS ###### 

# Where we download the data from D1
data_folder <- "~/Desktop/all_datasets"

# Search for data on ADC, KNB, PASTA, EDI
# https://docs.google.com/spreadsheets/d/1rwa8Bsh3eNCL7ScEm-XepoRrrxU4SSBs8BWGknGFBmQ/edit#gid=0
data_listing_sheet <- "Metajam testing - data sets"


####### MAIN ##########

# Create the local directory to store data sets
dir.create(data_folder, showWarnings = FALSE)


###=============================================================###
### Download data
###=============================================================###
# Read the data listing in 
data_listing <- # Getting the test datasets listing
  test_datasets_listing <- gs_title(data_listing_sheet) %>%
  gs_read() %>%
  select(package_doi_url, pid, filename) %>%
  na.omit() # should be improved

### Download the data and metadata ----

# batch download the datasets
map2(data_listing$pid, data_folder, download_d1_data)
# Woot => 40 datasets dowloaded

###=============================================================###
### Read data in R
###=============================================================###

# List the data set folders
local_datasets <- dir(data_folder, full.names = TRUE)

# Read them all in as a names list (probably not the way scientisits want to do it)
test <- setNames(map(local_datasets, read_d1_files), basename(local_datasets))






