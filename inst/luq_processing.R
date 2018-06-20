# devtools::install_github("DataONEorg/rdataone")
# devtools::install_github("cboettig/eml2")
# devtools::install_github("NCEAS/metajam")
library(metajam)

# the wranglers
library(readxl)
library(readr)
library(googledrive)
library(googlesheets)
library(dplyr)
library(purrr)
library(lubridate)
library(janitor)
library(skimr)


###### CONSTANTS ###### 

# Dir to store the templates "hand made" by the WG
template_folder <- "Templates" 

# Where we download the data from D1
data_folder <- "~/Desktop/Data_SEC"

# Mapping the fields
mapper_file <- "fields_mapping_luq_template.csv" 
# Data sets listing GSheet
data_gs <- "[LTER - SEC] Sata sets listing"


###### FUNCTIONS ###### 

#' Map attributes between a data structure template and a data set; need a mapping file matching attibutes
#'
#' @param template_df A data frame with the template structure
#' @param data_df  A data frame with the data
#' @param attributes_matching A data frame with 2 columns: first - template attributes, second - matching data attributes
#'
#' @return A data frame with the template filled with the data
#'
#' @examples attribute_mapper(master_template_luq, master_luq, mapped_attributes)
attribute_mapper <- function(template_df, data_df, attributes_matching){
  n_tot <- nrow(attributes_matching)
  for (i in seq(1:n_tot)){
    template_df[, attributes_matching[[i,1]]] <- data_df[, attributes_matching[[i,2]]]
  }
  return(template_df)
}


#' Compute categorical frequency
#'
#' @param df A data frame containing the data to summarize
#'
#' @return summary statistics for categorical values
#'
#' @examples categorical_frequency(faimstracs_legacy)
categorical_frequency <- function(df){
  # Select the character columns
  cat_data <- Filter(is.character, df)
  # Compute the unique values and frequencies
  cat_freq_list <- map(cat_data, janitor::tabyl)
  # Create a data frame out of that
  cat_freq <- bind_rows(cat_freq_list, .id = "attribute")
  names(cat_freq)[[2]] <- "category"
  # add the total number of categories
  cat_freq <- cat_freq %>% 
    group_by(attribute) %>%
    mutate(ntotal_cat = n()) %>%
    ungroup() %>%
    #remove the one that are unique for each row (here 90%), ie no categories
    # Note: need to be improved regarding field with NAs
    filter(ntotal_cat < nrow(df)*0.9)
  
  return(cat_freq)
}


###### MAIN ###### 

### Do the housekeeping for dir structure (top level in the working dir) ----

# Create the local directory to store templates
dir.create(template_folder, showWarnings = FALSE)
# Create the local directory to store data sets
dir.create(data_folder, showWarnings = FALSE)

### Getting the list of datasets to read ----

# Getting the test datasets listing
test_datasets_listing <- gs_title(data_gs) %>%
  gs_read()

######### LUQ processing ##################

# Keep only the LUQ related data sets
luq_test_datasets <- test_datasets_listing %>%
  # filter(grepl("LUQ", .$`LTER site abbreviation`)) %>%
  select(`LTER site abbreviation`,
         `Data Repositorty (PASTA) URL to Archive/Metadata`,
         `Data Repositorty (PASTA) URL to File`,`Data Repositorty (PASTA) Filename`) %>%
  na.omit()


### Download the data and metadata ----

# batch download the datasets
map2(luq_test_datasets$`Data Repositorty (PASTA) URL to File`, data_folder, download_d1_data)


### Bulk Read data files back into R as a named list ----

# List the files
csv_files <- list.files(data_folder, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
csv_meta <- list.files(data_folder, pattern = "metadata.csv$", full.names = TRUE, recursive = TRUE)
csv_data <- setdiff(csv_files, csv_meta)

# Read the data in a named list
df_luq <- setNames(map(csv_files, read_csv), basename(csv_data))

###  Read a specific data set back into R as a named list ----





### check if the attributes are identical for each sampling sites ----

# list all the attributes
attributes_luq <- setNames(map(names(df_luq), function(x){colnames(df_luq[x][[1]])}),names(df_luq))

# Check if they are identical (could not find a way without a loop)
for(ds in names(attributes_luq)) {
  print(identical(attributes_luq[[1]], attributes_luq[ds][[1]]))
}
# => We are good, same data structure across the sampling sites

##->-------- SHOULD DO THE SAME ON UNITS!!! ----------<-###

### bind the sampling sites data into one master dataset for LUQ ----
all_sites_luq <- bind_rows(df_luq, .id = "prov")   #  <- filenames to be replaced by pids!!!!!!!!!!


### Map the attributes between the raw data and the template ----

## Get the existing templates and the data model structure from the WG GDrive 
# List the templates from the GDrive
drive_folder <- "1HgU9ynNdUGD-YoTbk4hoK8KTV-uChoB8"
templates_on_drive <- drive_ls(as_id(drive_folder), pattern = "xlsx")
map_files_on_drive <- drive_ls(as_id(drive_folder), pattern = "csv") # <- to be refined for more specific naming

# Select the LUQ sampling sites
luq_template_gd <- templates_on_drive %>%
  filter(grepl("LUQ", .$name))

# Download the template
drive_download(as_id(luq_template_gd$id), 
               file.path(template_folder, luq_template_gd$name),
               overwrite = TRUE)

# Read the xls template for LUQ
template_example_luq <- list.files(template_folder, pattern = ".xlsx", full.names = TRUE) %>%
  read_excel(sheet = "Raw Data")

# Attributes name
template_fields_luq <- colnames(template_example_luq)

# Read the file mapping attributes 
drive_download(as_id(map_files_on_drive$id), overwrite = TRUE)  # <- to be changed when several mapping files are uploaded
mapped_attributes <- read_csv(map_files_on_drive$name)

# Compare new mapping with existing template
setdiff(mapped_attributes$template, template_fields_luq)

# Create a new empty template
master_template_luq <- data.frame(matrix(NA, nrow = nrow(all_sites_luq), ncol = nrow(mapped_attributes)))
colnames(master_template_luq) <- mapped_attributes$template


### Map the attribute bertween the Working group template and the dataset ----

# Add the LTER site
master_template_luq$LTER <- "LUQ"

# Do the attributes mapping
mapped_attributes <- na.omit(mapped_attributes)
master_template_luq <- attribute_mapper(master_template_luq, all_sites_luq, mapped_attributes)  
# str(master_template_luq)

# Get the Look-up Table for units conversion
LUT_file <- file.path(template_folder, "Conversions.xlsx")

###->-------- Need metadata about units----------<-###


###->-------- Do the proper unit conversions to transform data to the template units----------<-###


### Some cleaning and QC checks ----

# Replace -9999 with NAs
master_template_luq[master_template_luq == -9999] <- NA

# Date formatting
master_template_luq$`Sampling Date` <- mdy(master_template_luq$`Sampling Date`)

# Run some checks on categorical data
cat_stat_luq  <- categorical_frequency(master_template_luq)

# Same on numeric
num_stat_luq <- skimr::skim(master_template_luq) %>%
  select(-level, -value) %>%
  filter(stat!="hist")

# write the LTER LUQ site level dataset
write_csv(master_template_luq, "Site_Data_Template_V5_LUQ.csv")

