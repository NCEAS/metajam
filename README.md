<img src="inst/images/metajam_hex.png" align="right" width="10%" />


# metajam 

[![Travis-CI Build Status](https://travis-ci.org/NCEAS/metajam.svg?branch=master)](https://travis-ci.org/NCEAS/metajam)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/metajam)](https://cran.r-project.org/metajam)

Download and read data and metadata from repositories in the DataONE network.  

## Authors

Irene Steves, Mitchell Maier and Julien Brun; NCEAS 

## Installation

The `metajam` package can be installed from GitHub using the `devtools` package:

```
devtools::install_github("nceas/metajam")
```

## Download data

To download a data object, specify the data object URL and local download path in the `download_d1_data` function:

```
library(metajam)

download_d1_data("https://arcticdata.io/metacat/d1/mn/v2/object/urn%3Auuid%3A9e123f84-ce0d-4094-b898-c9e73680eafa", path = ".")
```
<br>
<img src="inst/images/download-output.png" width="60%"/>
<br>
<br>
<img src="inst/images/file-output.png" width="50%"/>
<br>

The output is saved in a folder with the name `{metadata_id}__{file_name}`, which contains the data file and its associated metadata. The metadata follows these conventions:

- `{file_name}__summary_metadata.csv` - summary metadata in a tabular format, which includes date downloaded, data file name, file/metadata URL's, etc.
- `{file_name}__full_metadata.xml` - metadata xml file, if it could be downloaded
- `{file_name}__attribute_metadata.csv` - attribute metadata in a tabular format, if included in the metadata xml
- `{file_name}__attribute_factor_metadata.csv` - attribute factor metadata in a tabular format, if included in the metadata xml

## How to get the URL to your dataset of interest ?

From DataONE or any currently supported data repository ([KNB](https://knb.ecoinformatics.org/), [Artic Data Center](https://arcticdata.io/), [LTER PASTA](https://portal.lternet.edu/nis/home.jsp) or [EDI](https://portal.edirepository.org/nis/home.jsp)), you can right-click on the `Download` button of a specific dataset and choose `Copy Link Address` to cpopy the URL to your clipboard

<br>
<img src="inst/images/copy-link.png" width="100%"/>
<br>

## Read data

The `read_d1_files` function allows you to read the downloaded data and metadata directly into your R environment. Simply run the function with the folder path to the downloaded objects, and all data and metadata files will be returned as data frames stored in a list. Use `{object_name}$data` to access the data, and `{object_name}${metadata_type}_metadata` to access its associated metadata.

```
schools <- read_d1_files("./doi_10.18739_A2DP3X__Alaska_Schools_Rentention2009_15")
```

<br>
<img src="inst/images/read-output.png" width="60%"/>
<br>
