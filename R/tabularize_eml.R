#' Get tabular metadata
#'
#' This function takes a path to an EML (.xml) metadata file and returns a data frame.
#'
#' @param eml An emld class object, the path to an EML (.xml) metadata file, or a raw EML object.
#' @param full (logical) Returns the most commonly used metadata fields by default.
#'   If \code{full = TRUE} is specified, the full set of metadata fields are returned.
#'
#' @return (data.frame) A data frame of selected EML values.
#'
#' @import dplyr
#' @importFrom emld as_emld
#' @importFrom tibble enframe
#' @importFrom stringr str_trim
#'
#' @export
#'
#' @examples
#'    eml <- system.file("extdata", "test_data", "SoilMois2012_2017__full_metadata.xml",
#'                   package = "metajam")
#'    tabularize_eml(eml)

tabularize_eml <- function(eml, full = FALSE) {
  # Silence 'visible bindings' note
  name <- value <- NULL

  if (any(class(eml) == "emld")) {
    eml <- eml
  } else if (is.character(eml) | is.raw(eml)) {
    eml <- emld::as_emld(eml)
  } else {
    stop("The EML input could not be parsed.")
  }

  # Transforming into a dataframe
  metadata <- eml %>%
    unlist() %>%
    tibble::enframe()

  if (full == FALSE) {
    metadata <- metadata %>%
      dplyr::mutate(name = dplyr::case_when(
        grepl("schemaLocation", name) ~ "eml.version",
        grepl("title", name) ~ "title",
        grepl("individualName", name) ~ "people",
        grepl("abstract", name) ~ "abstract",
        grepl("keyword", name) ~ "keyword",
        grepl("geographicDescription", name) ~ "geographicCoverage.geographicDescription",
        grepl("westBoundingCoordinate", name) ~ "geographicCoverage.westBoundingCoordinate",
        grepl("eastBoundingCoordinate", name) ~ "geographicCoverage.eastBoundingCoordinate",
        grepl("northBoundingCoordinate", name) ~ "geographicCoverage.northBoundingCoordinate",
        grepl("southBoundingCoordinate", name) ~ "geographicCoverage.southBoundingCoordinate",
        grepl("beginDate", name) ~ "temporalCoverage.beginDate",
        grepl("endDate", name) ~ "temporalCoverage.endDate",
        grepl("taxonRankValue", name) ~ "taxonomicCoverage",
        grepl("methods", name) ~ "methods",
        grepl("objectName", name) ~ "objectName",
        grepl("online.url", name) ~ "url"
      )) %>%
      dplyr::filter(!is.na(name)) %>%
      dplyr::mutate(value = stringr::str_trim(value)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(name) %>%
      dplyr::summarize(value = paste(value, collapse = "; "), .groups = "drop") %>%
      dplyr::mutate(value = gsub("\n", "", value))  #without this, fields get truncated in Excel
  }

  return(metadata)
}

