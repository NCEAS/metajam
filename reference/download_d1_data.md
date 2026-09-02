# Download data and metadata from DataONE

Downloads a data object from DataONE along with metadata.

## Usage

``` r
download_d1_data(data_url, path)
```

## Arguments

- data_url:

  (character) An identifier or URL for a DataONE object to download.

- path:

  (character) Path to a directory to download data to.

## Value

(character) Path where data is downloaded to.

## See also

\[read_d1_files()\] \[download_d1_data_pkg()\]

## Examples

``` r
# \donttest{
download_d1_data("urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570", path = tempdir())
#> urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570 is the latest version for the provided persistent identifier
#> Metadata is in EML format
#> 
#> Downloading data urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570 ...
#> Download complete
#> [1] "/tmp/Rtmp4XIePR/doi_10_18739_A23F4KM7K__Alexander_ExpBurnSoilMois2012_2017__csv"
download_d1_data(
   "https://cn.dataone.org/cn/v2/resolve/urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570",
    path = tempdir()
    )
#> urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570 is the latest version for the provided persistent identifier
#> Metadata is in EML format
#> 
#> Downloading data urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570 ...
#> Download complete
#> [1] "/tmp/Rtmp4XIePR/doi_10_18739_A23F4KM7K__Alexander_ExpBurnSoilMois2012_2017__csv__copy_1"
# }
```
