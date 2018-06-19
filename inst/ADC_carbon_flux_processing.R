# devtools::install_github("DataONEorg/rdataone")
# devtools::install_github("cboettig/eml2")
# devtools::install_github("NCEAS/metajam")
library(metajam)

library(tidyverse)


###### CONSTANTS ###### 

# Where we download the data from D1
data_folder <- "Data"

# ADC permafrost data sets listing (thank you Steven!)
adc_data_listing <- "ADC_permafrost_related_datasets"


####### MAIN ##########

# Create the local directory to store data sets
dir.create(data_folder, showWarnings = FALSE)

# Read the data listing in 
data_listing <- # Getting the test datasets listing
  test_datasets_listing <- gs_title(adc_data_listing) %>%
  gs_read() %>%
  select(package_doi_url, pid, filename) %>%
  na.omit()

### Download the data and metadata ----

# batch download the datasets
map2(data_listing$pid, data_folder, download_D1Data)


