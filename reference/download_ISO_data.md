# Download data and metadata from a dataset that uses ISO metadata.

This is an internal function called by the download_d1_data.R function.
Not to be exported

## Usage

``` r
download_ISO_data(meta_raw, meta_obj, meta_id, data_id, metadata_nodes, path)
```

## Arguments

- meta_raw:

  (character) A raw metadata object produced by download_d1_data

- meta_obj:

  (character) A metadata object produced by download_d1_data

- meta_id:

  (character) A metadata identifier produced by download_d1_data

- data_id:

  (character) A data identifier produced by download_d1_data

- metadata_nodes:

  (character) The member nodes where this metadata is stored, produced
  by download_d1_data

- path:

  (character) Path to a directory to download data to.
