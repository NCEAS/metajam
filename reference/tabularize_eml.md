# Get tabular metadata

This function takes a path to an EML (.xml) metadata file and returns a
data frame.

## Usage

``` r
tabularize_eml(eml, full = FALSE)
```

## Arguments

- eml:

  An emld class object, the path to an EML (.xml) metadata file, or a
  raw EML object.

- full:

  (logical) Returns the most commonly used metadata fields by default.
  If `full = TRUE` is specified, the full set of metadata fields are
  returned.

## Value

(data.frame) A data frame of selected EML values.

## Examples

``` r
   eml <- system.file("extdata", "test_data", "SoilMois2012_2017__full_metadata.xml",
                  package = "metajam")
   tabularize_eml(eml)
#> # A tibble: 16 × 2
#>    name                                       value                             
#>    <chr>                                      <chr>                             
#>  1 abstract                                   Fire severity is increasing acros…
#>  2 eml.version                                eml://ecoinformatics.org/eml-2.1.…
#>  3 geographicCoverage.eastBoundingCoordinate  161.4067                          
#>  4 geographicCoverage.geographicDescription   Far northeastern Siberia near Che…
#>  5 geographicCoverage.northBoundingCoordinate 68.7433                           
#>  6 geographicCoverage.southBoundingCoordinate 68.7433                           
#>  7 geographicCoverage.westBoundingCoordinate  161.4067                          
#>  8 keyword                                    None; fire; permafrost; Siberia; …
#>  9 methods                                    <title>Surface soil moisture</tit…
#> 10 objectName                                 Alexander_Exp Burn Soil Mois 2012…
#> 11 people                                     Alexander; Heather; D.; Michael; …
#> 12 taxonomicCoverage                          Larix cajanderi                   
#> 13 temporalCoverage.beginDate                 2012-07-01                        
#> 14 temporalCoverage.endDate                   2017-08-01                        
#> 15 title                                      Surface soil moisture across an e…
#> 16 url                                        download; https://cn.dataone.org/…
```
