# Download all data and metadata of a data package from DataONE

Downloads all the data objects of a data package from DataONE along with
metadata.

## Usage

``` r
download_d1_data_pkg(meta_obj, path)
```

## Arguments

- meta_obj:

  (character) A DOI or metadata object PID for a DataONE package to
  download.

- path:

  (character) Path to a directory to download data to.

## Value

(list) Paths where data are downloaded to.

## See also

\[read_d1_files()\] \[download_d1_data()\]

## Examples

``` r
# \donttest{
download_d1_data_pkg("doi:10.18739/A2CJ87M3J", tempdir())
#> doi:10.18739/A2CJ87M3J is the latest version for the provided persistent identifier
#> urn:uuid:8cbdadcc-b227-47a4-8fba-e3f01810bdff is the latest version for the provided persistent identifier
#> Warning: multiple metadata records found:
#> list(id = "doi:10.18739/A2CJ87M3J", dateUploaded = 1590093572)
#> list(id = "doi:10.18739/A2028W", dateUploaded = 1518476800)
#> The most recent record was used
#> Metadata is in EML format
#> 
#> Downloading data urn:uuid:8cbdadcc-b227-47a4-8fba-e3f01810bdff ...
#> Download complete
#> urn:uuid:65753f42-5204-40f7-92b8-b0fab9cf0b35 is the latest version for the provided persistent identifier
#> Warning: multiple metadata records found:
#> list(id = "doi:10.18739/A2CJ87M3J", dateUploaded = 1590093572)
#> list(id = "doi:10.18739/A2028W", dateUploaded = 1518476800)
#> The most recent record was used
#> Metadata is in EML format
#> 
#> Downloading data urn:uuid:65753f42-5204-40f7-92b8-b0fab9cf0b35 ...
#> Download complete
#> urn:uuid:ccc96cc5-3abc-479a-ae78-a0e12995c983 is the latest version for the provided persistent identifier
#> Warning: multiple metadata records found:
#> list(id = "doi:10.18739/A2CJ87M3J", dateUploaded = 1590093572)
#> list(id = "doi:10.18739/A2028W", dateUploaded = 1518476800)
#> The most recent record was used
#> Metadata is in EML format
#> 
#> Downloading data urn:uuid:ccc96cc5-3abc-479a-ae78-a0e12995c983 ...
#> Download complete
#> [[1]]
#> [1] "/tmp/Rtmp4XIePR/doi_10_18739_A2CJ87M3J__2015_2016_winter_E1_temperature__csv"
#> 
#> [[2]]
#> [1] "/tmp/Rtmp4XIePR/doi_10_18739_A2CJ87M3J__2015_2016_winter_E1_spconductance__csv"
#> 
#> [[3]]
#> [1] "/tmp/Rtmp4XIePR/doi_10_18739_A2CJ87M3J__2015_2016_winter_E1_dissoxy__csv"
#> 
download_d1_data_pkg("https://doi.org/10.18739/A2CJ87M3J", tempdir())
#> doi:10.18739/A2CJ87M3J is the latest version for the provided persistent identifier
#> urn:uuid:8cbdadcc-b227-47a4-8fba-e3f01810bdff is the latest version for the provided persistent identifier
#> Warning: multiple metadata records found:
#> list(id = "doi:10.18739/A2CJ87M3J", dateUploaded = 1590093572)
#> list(id = "doi:10.18739/A2028W", dateUploaded = 1518476800)
#> The most recent record was used
#> Metadata is in EML format
#> 
#> Downloading data urn:uuid:8cbdadcc-b227-47a4-8fba-e3f01810bdff ...
#> Download complete
#> urn:uuid:65753f42-5204-40f7-92b8-b0fab9cf0b35 is the latest version for the provided persistent identifier
#> Warning: multiple metadata records found:
#> list(id = "doi:10.18739/A2CJ87M3J", dateUploaded = 1590093572)
#> list(id = "doi:10.18739/A2028W", dateUploaded = 1518476800)
#> The most recent record was used
#> Metadata is in EML format
#> 
#> Downloading data urn:uuid:65753f42-5204-40f7-92b8-b0fab9cf0b35 ...
#> Download complete
#> urn:uuid:ccc96cc5-3abc-479a-ae78-a0e12995c983 is the latest version for the provided persistent identifier
#> Warning: multiple metadata records found:
#> list(id = "doi:10.18739/A2CJ87M3J", dateUploaded = 1590093572)
#> list(id = "doi:10.18739/A2028W", dateUploaded = 1518476800)
#> The most recent record was used
#> Metadata is in EML format
#> 
#> Downloading data urn:uuid:ccc96cc5-3abc-479a-ae78-a0e12995c983 ...
#> Download complete
#> [[1]]
#> [1] "/tmp/Rtmp4XIePR/doi_10_18739_A2CJ87M3J__2015_2016_winter_E1_temperature__csv__copy_1"
#> 
#> [[2]]
#> [1] "/tmp/Rtmp4XIePR/doi_10_18739_A2CJ87M3J__2015_2016_winter_E1_spconductance__csv__copy_1"
#> 
#> [[3]]
#> [1] "/tmp/Rtmp4XIePR/doi_10_18739_A2CJ87M3J__2015_2016_winter_E1_dissoxy__csv__copy_1"
#> 
# }
```
