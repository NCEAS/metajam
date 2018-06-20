get_chunks <- function(x){
  #return first input
  
  # initialize structure
  if(!exists("chunk_all")){
    chunk_all <- x
  }
  
  # get first chunk
  chunk <- stringr::str_replace(x, "[^/=]*[/=]+", "")
  
  # recursion
  if(stringr::str_detect(x, "[/=]")){
    chunk2 <- get_chunks(chunk)
    chunk_all <- c(chunk, chunk2)
  }
  
  return(chunk_all)
}

get_pid_dates <- function(x){
  suppressMessages(dataone::query(dataone::CNode("PROD"), 
                                  list(q = sprintf('identifier:"%s"', x),
                                       fl = "identifier, dateUploaded, formatType, obsoletedBy",
                                       rows = "1000"),
                                  as = "data.frame"))
}

#' Check pid version
#'
#' This function takes an identifier and checks to see if it has been obsoleted.
#'
#' @param pid (character) The persistent identifier of a data, metadata, or resource map object on a DataONE member node
#' @param formatType (character) Optional. The format type to return (DATA, METADATA, RESOURCE)
#'
#' @importFrom dataone CNode getSystemMetadata
#' @import stringr
#' @import dplyr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Most data URL's and identifiers work
#' check_version("https://cn.dataone.org/cn/v2/resolve/urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570")
#' 
#' # Returns a warning if several identifiers are returned:
#' check_version("https://pasta.lternet.edu/package/data/eml/edi/195/2/51abf1c7a36a33a2a8bb05ccbf8c81c6")
#' check_version("doi:10.18739/A2ZF6M")
#' 
#' # You can specify a formatType (metadata, data, resource)
#' check_version("doi:10.18739/A2ZF6M", formatType = "metadata")
#' 
#' # Returns a warning if the identifier has been obsoleted
#' check_version("doi:10.18739/A2HF7Z", formatType = "metadata")
#' 
#' # Returns an error if no matching identifiers are found
#' check_version("a_test_pid")
#' }
#'

check_version <- function(pid, formatType = NULL){
  while(nchar(pid) > 0) {
    results <- suppressMessages(
      dataone::query(dataone::CNode(),
                     list(q = sprintf('identifier:"%s"', pid),
                          fl = "identifier, dateUploaded, formatType, obsoletedBy"),
                     as = "data.frame")
    )
    
    if (nrow(results) == 0) {
      pid <- gsub("^[^/=]+[/=]*", "", pid)
      
    } else {
      #what to do if multiple are returned
      break
    }
  }
  
  if(nrow(results) == 0){
    stop("No matching identifiers were found.")
  }
  
  # filter out extra types (resource map/etc with similar pid)
  if(!is.null(formatType)){
    formatType <- toupper(formatType)
    results <- results[results$formatType == formatType,]
  }
  
  if(nrow(results) == 1){
    if(is.null(results$obsoletedBy) || is.na(results$obsoletedBy)){
      message(results$identifier,
              " is the latest version of the identifier.")
    } else {
      warning("The identifier has been obsoleted by ", results$obsoletedBy)
    }
  } else {
    warning("Several identifiers are associated with ", pid)
  }
  
  return(results)
}
