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
if (FALSE) { # \dontrun{
download_d1_data_pkg("doi:10.18739/A2028W", ".")
download_d1_data_pkg("https://doi.org/10.18739/A2028W", ".")
} # }
```
