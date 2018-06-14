# devtools::install_github("cboettig/eml2")
# devtools::install_github("NCEAS/metajam")
library(metajam)
# devtools::install_github("isteves/dataimport")
library(dataimport)
# devtools::install_github("maier-m/download_data")
library(downloadData)

# the wranglers
library(readxl)
library(readr)
library(googledrive)
library(googlesheets)
library(dplyr)
library(purrr)


## CONSTANTS----

# Dir to store the templates "hand made" by the WG
template_folder <- "Templates" 

# Where we download the data from D1
data_folder <- "Data"


## MAIN ----

### Do the house keeping for dir structure in the working dir ----

# Create the local directory to store templates
dir.create(template_folder, showWarnings = FALSE)
# Create the local directory to store templates
dir.create(data_folder, showWarnings = FALSE)


### Getting fields we need to keep ----

# List the templates
drive_folder <- "1HgU9ynNdUGD-YoTbk4hoK8KTV-uChoB8"
templates_on_drive <- drive_ls(as_id(drive_folder), pattern = "xlsx")
# Select the LUQ sampling sites
luq_template_gd <- templates_on_drive %>%
  filter(grepl("LUQ", .$name))
# Download the template
drive_download(as_id(luq_template_gd$id), 
               file.path(template_folder, luq_template_gd$name),
               overwrite = TRUE)

# Read the xls template for LUQ
luq_template_data <- list.files(template_folder, full.names = TRUE) %>%
  read_excel(sheet = "Raw Data")

# fields name
template_fields <- colnames(luq_template_data)


### Getting the list of datasets to read ----

# Getting the test datasets listing
test_datasets_listing <- gs_title("[LTER - SEC] Sata sets listing") %>%
  gs_read()

# Keep only the LUQ related data sets
luq_test_datasets <- test_datasets_listing %>%
  filter(grepl("LUQ", .$`LTER site abbreviation`)) %>%
  select(`LTER site abbreviation`,
         `Data Repositorty (PASTA) URL to Archive/Metadata`,
         `Data Repositorty (PASTA) URL to File`,`Data Repositorty (PASTA) Filename`)


### Download the data and metadata ----

#********* Does not work for now ***********
# download_D1Data(luq_test_datasets$`Data Repositorty (PASTA) URL to File`[5], data_folder)

# Go the troll way and batch download the datasets
# map2(luq_test_datasets$`Data Repositorty (PASTA) URL to File`[2:nrow(luq_test_datasets)], data_folder, download_D1Data)
#*******************************************

# Going the basic way -- To be replaced
map2(luq_test_datasets$`Data Repositorty (PASTA) URL to File`,
     file.path(data_folder, luq_test_datasets$`Data Repositorty (PASTA) Filename`), 
     download.file)


### Read data files back into R as a named list----

# List the files
csv_files <- list.files(data_folder, pattern = ".csv$", full.names = TRUE)

# Read the data in a named list
df_luq <- setNames(map(csv_files, read_csv), basename(csv_files))


### check if the fields are identical for each sampling sites ---
# list all the fields
luq_fields <- setNames(map(names(df_luq), function(x){colnames(df_luq[x][[1]])}),names(df_luq))

# Check if they are identical (could not find a way without a loop)
for(ds in names(luq_fields)) {
  print(identical(luq_fields[[1]], luq_fields[ds][[1]]))
}
# We are good same data structure accors the sampling 

##->-------- SHOULD DO THE SAME ON UNITS!!! ----------<-###

### Create the master dataset for lus
master_luq <- bind_rows(df_luq, .id = "prov")   #  <- to be replaced by pids!!!!!!!!!!



