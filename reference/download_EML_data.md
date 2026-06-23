# Download data and metadata from a dataset that uses EML metadata.

This is an internal function called by the download_d1_data.R function.
Not to be exported

## Usage

``` r
download_EML_data(data_url, meta_obj, meta_id, data_id, metadata_nodes, path)
```

## Arguments

- data_url:

  (character) An identifier or URL for a DataONE object to download.

- meta_obj:

  (character) A metadata object produced by download_d1_data. This is a
  different format than the metadata object required for the analogous
  ISO function

- meta_id:

  (character) A metadata identifier produced by download_d1_data

- data_id:

  (character) A data identifier produced by download_d1_data

- metadata_nodes:

  (character) The member nodes where this metadata is stored, produced
  by download_d1_data

- path:

  (character) Path to a directory to download data to.
