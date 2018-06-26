# This script download stream chemistry data for the LTER Luquillo site
# Data source: http://dx.doi.org/doi:10.6073/pasta/f9df56348f510da0113b1e6012fa2967


# devtools::install_github("DataONEorg/rdataone")
# devtools::install_github("cboettig/eml2")
# devtools::install_github("NCEAS/metajam")
library(metajam)
library(udunits2)

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
  filter(grepl("LUQ", .$`LTER site abbreviation`)) %>%
  select(`LTER site abbreviation`,
         `Data Repository (PASTA) URL to Archive/Metadata`,
         `Data Repository (PASTA) URL to File`,`Data Repository (PASTA) Filename`) %>%
  na.omit()


### Download the data and metadata ----

# batch download the datasets
map(luq_test_datasets$`Data Repository (PASTA) URL to File`, ~download_d1_data(.,data_folder))

### Bulk Read data files back into R as a named list ----

# List the files
csv_files <- list.files(data_folder, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
csv_data <- csv_files[!grepl("metadata.csv$", csv_files)]
csv_meta <- csv_files[grepl("metadata.csv$", csv_files)]
# csv_data <- setdiff(csv_files, csv_meta) #look for "__" as an alternative

# Read the data in a named list
df_luq <- setNames(map(csv_data, read_csv), basename(csv_data))

###  Read a specific data set back into R as a named list ----
# List the data set folders
local_datasets <- dir(data_folder, full.names = TRUE)

# Read them all in as a named list (probably not the way scientisits want to do it)
luq_datasets <- setNames(map(local_datasets, read_d1_files), basename(local_datasets))


# df_qs <- read_d1_files("~/Desktop/Data_SEC/https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__QuebradaSonadora")


### check if the attributes are identical for each sampling sites ----
# list all the attributes
attributes_luq <- map(names(luq_datasets), function(x){colnames(luq_datasets[[x]]$data)}) %>%
  setNames(.,names(luq_datasets))

# Check if they are identical (could not find a way without a loop)
for(ds in names(attributes_luq)) {
  print(identical(attributes_luq[[1]], attributes_luq[ds][[1]]))
}
# => We are good, same data structure across the sampling sites

##->-------- SHOULD DO THE SAME ON UNITS!!! ----------<-###
luq_units <- map(names(luq_datasets), function(x){luq_datasets[[x]]$attribute_metadata$unit}) %>%
                setNames(.,names(luq_datasets))

for(us in names(luq_units)) {
  print(identical(luq_units[[1]], luq_units[us][[1]]))
}

# => The 2 last datasets have differen units!!!!!!!!!!

# Let's check
setdiff(luq_units[[1]], luq_units[7][[1]])
setdiff(luq_units[[1]], luq_units[8][[1]])

# More advanced way of doing this
luq_units_merged <- luq_datasets %>%
  map(`[[`, "attribute_metadata") %>%
  map(. %>% select(attributeName, unit)) %>%
  reduce(full_join, by = "attributeName") 

## Rename
# Create the new names
luq_new_colnames <- names(luq_units) %>%
  str_split_fixed(., "__", n=2) %>%
  .[,2] %>%
  paste("unit", ., sep = "_")
# Apply the new names
colnames(luq_units_merged) <- c("attributeName", luq_new_colnames)


# fix attribute naming discrepencies -- To be improved as hardwired like crazy --
# Copy the units for Gage height
luq_units_merged[which(luq_units_merged$attributeName=="Gage_Ht"), c("unit_RioIcacos", "unit_RioMameyesPuenteRoto")] <- "foot"

# Copy the units for NH4
luq_units_merged[which(luq_units_merged$attributeName=="NH4-N"), c("unit_RioIcacos", "unit_RioMameyesPuenteRoto")] <- "microgramsPerLiter"

# drop the 2 last rows
luq_units_merged <- head(luq_units_merged, -2)

### Implement the unit conversion for RioIcacos and RioMameyesPuenteRoto ----

## RioIcacos
# Fix NAs
luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioIcacos`$data[luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioIcacos`$data == -9999] <- NA

# Simplify naming
RioIcacos_data <- luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioIcacos`$data
RioIcacos_attrmeta <- luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioIcacos`$attribute_metadata

# Do the unit conversion  - Gage height - manual way
# RioIcacos_data$Gage_Ht <- 
#   RioIcacos_data$Gage_Ht * 0.3048

# Do the unit conversion  - Gage height - udunits way
RioIcacos_data$Gage_Ht <- ud.convert(RioIcacos_data$Gage_Ht, "foot", "meter")

# Do the unit conversion for RioIcacos and RioMameyesPuenteRoto - NH4 to NH4-N
coeff_conv_NH4_to_NH4N <- 0.7764676534
RioIcacos_data$`NH4-N` <- RioIcacos_data$`NH4-N` * coeff_conv_NH4_to_NH4N

# Update the main object 
luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioIcacos`$data <- RioIcacos_data

## RioMameyesPuenteRoto
# Replace -9999 with NAs 
luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioMameyesPuenteRoto`$data[luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioMameyesPuenteRoto`$data == -9999] <- NA
RioMameyesPuenteRoto_data <- luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioMameyesPuenteRoto`$data
RioMameyesPuenteRoto_attrmeta <- luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioMameyesPuenteRoto`$attribute_metadata


# Do the unit conversion  - Gage height - manual way
RioMameyesPuenteRoto_data$Gage_Ht <- ud.convert(RioMameyesPuenteRoto_data$Gage_Ht, "foot", "meter")

# Do the unit conversion for RioMameyesPuenteRoto - NH4 to NH4-N
RioMameyesPuenteRoto_data$`NH4-N` <- RioMameyesPuenteRoto_data$`NH4-N` * coeff_conv_NH4_to_NH4N

# Update the main object
luq_datasets$`https_pasta.lternet.edu_package_metadata_eml_knb-lter-luq_20_4923051__RioMameyesPuenteRoto`$data <- RioMameyesPuenteRoto_data 


### bind the sampling sites data into one master dataset for LUQ ----
all_sites_luq <- bind_rows(df_luq, .id = "prov")
# Replace -9999 with NAs
all_sites_luq[all_sites_luq == -9999] <- NA
# write as csv
write_csv(all_sites_luq, "~/Desktop/stream_chem_all_LUQ.csv")



#=========================== MATCHING TEMPLATE FROM WG ==============================#

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

