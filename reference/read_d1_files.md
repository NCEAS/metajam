# Read data and metadata based on \`download_d1_data()\` file structure

Reads data along with metadata into your R environment based on
\[download_d1_data()\] file structure.

## Usage

``` r
read_d1_files(folder_path, fnc = "read_csv", ...)
```

## Arguments

- folder_path:

  (character) Path to a directory where data and metadata are located.

- fnc:

  (character) Function to be used to read the data (default is
  \[readr::read_csv()\]).

- ...:

  Parameters to pass into the function specified in \`fnc\`.

## Value

(list) Named list containing data and metadata as data frames.

## See also

\[download_d1_data()\] \[download_d1_data_pkg()\]

## Examples

``` r
data_folder <- system.file(file.path("extdata", "test_data"), package = "metajam")
soil_moist_data <- read_d1_files(data_folder)
#> Rows: 21 Columns: 11
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr  (1): Date
#> dbl (10): Unmanipulated Moisture (cm3 cm-3), Unmanipulated Moisure (SE), Unb...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 11 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (9): attributeName, attributeDefinition, formatString, measurementScale,...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 17 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> chr (2): name, value
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

# You can specify the function you would like to use to read the file and pass parameters
soil_moist_data_skipped <- read_d1_files(data_folder, "read.csv",
                                         skip = 8, stringsAsFactors = FALSE)
```
