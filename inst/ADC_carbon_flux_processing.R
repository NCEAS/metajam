# devtools::install_github("DataONEorg/rdataone")
# devtools::install_github("cboettig/eml2")
# devtools::install_github("NCEAS/metajam")
library(metajam)

library(tidyverse)
library(googledrive)
library(googlesheets)
library(janitor)
library(skimr)


###### CONSTANTS ###### 

# Where we download the data from D1
data_folder <- "~/Desktop/DataADC"

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
map2(data_listing$pid, data_folder, download_d1_data)


### Read data files back into R as a named list ----

# List the files
csv_files <- list.files(data_folder, pattern = ".csv$", full.names = TRUE, recursive = TRUE)

# Read the data in a named list
df_luq <- setNames(map(csv_files, read_csv), basename(csv_files))


### check if the attributes are identical for each sampling sites ----

# list all the attributes
attributes_luq <- setNames(map(names(df_luq), function(x){colnames(df_luq[x][[1]])}),names(df_luq))

# Check if they are identical (could not find a way without a loop)
for(ds in names(attributes_luq)) {
  print(identical(attributes_luq[[1]], attributes_luq[ds][[1]]))
}
# => We are good, same data structure across the sampling sites

##->-------- SHOULD DO THE SAME ON UNITS!!! ----------<-###


