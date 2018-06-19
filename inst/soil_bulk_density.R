# Soil bulk density

# devtools::install_github("DataONEorg/rdataone") ## Version needed not on CRAN as of 2018/06/15
# devtools::install_github("cboettig/eml2")
# devtools::install_github("NCEAS/metajam")
library(metajam)

library(tidyverse)
library(googledrive)
library(googlesheets)

###### CONSTANTS ###### 

# Where we download the data from D1
data_folder <- "~/Desktop/Soil_bulk"

# Search for soil bulk density on ADC and KNB
adc_data_listing <- "Soil bulk density"


####### MAIN ##########

# Create the local directory to store data sets
dir.create(data_folder, showWarnings = FALSE)

# Read the data listing in 
data_listing <- # Getting the test datasets listing
  test_datasets_listing <- gs_title(adc_data_listing) %>%
  gs_read() %>%
  select(package_doi_url, pid, filename) %>%
  na.omit() # should be improved

### Download the data and metadata ----

# batch download the datasets
map2(data_listing$pid, data_folder, download_d1_data)


