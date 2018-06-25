#' Get tabular metadata
#'
#' This function takes a path to an EML (.xml) metadata file and returns a data frame.
#'
#' @param eml An emld class object, the path to an EML (.xml) metadata file, or a raw EML object
#' @param full (logical) Returns the most commonly used metadata fields by default. 
#' If \code{full = TRUE} is specified, the full set of metadata fields are returned.
#'
#' @export
#'
#' @importFrom tibble enframe
#'
#' @examples
#' \dontrun{
#'    eml <- system.file("example-eml.xml", package = "arcticdatautils")
#'    tabularize_eml(eml)
#'    tabularize_eml(eml, full = TRUE)
#' }
#'

tabularize_eml <- function(eml, full = FALSE){
  
  if(any(class(eml) == "emld")) {
    eml <- eml
  } else if(is.character(eml) | is.raw(eml)) {
    eml <- emld::as_emld(eml)
  } else {
    stop("The eml input could not be parsed.")
  }
  
  metadata <- eml %>% 
    unlist() %>% 
    tibble::enframe()
  
  if(full == FALSE){
    metadata <- metadata %>% 
      dplyr::mutate(name = case_when(
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
        #taxonomicCoverage
        grepl("methods", name) ~ "methods",
        grepl("objectName", name) ~ "objectName",
        grepl("online.url", name) ~ "url"
      )) %>% 
      dplyr::filter(!is.na(name)) %>% 
      dplyr::distinct() %>% 
      dplyr::group_by(name) %>% 
      dplyr::summarize(value = paste(value, collapse = " ")) %>% 
      dplyr::mutate(value = gsub("\n", "", value)) #without this, fields get truncated in Excel
  }
  
  return(metadata)
}
