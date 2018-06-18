get_chunks <- function(x){
  # initialize structure
  if(!exists("chunk_all")){
    chunk_all <- x
  }
  
  # get first chunk
  chunk <- str_replace(x, "[^/=]*[/=]", "")
  
  # recursion
  if(str_detect(x, "[/=]")){
    chunk2 <- get_chunks(chunk)
    chunk_all <- c(chunk, chunk2)
  }
  
  return(chunk_all)
}

get_pid_dates <- function(x){
  dataone::query(dataone::CNode("PROD"), 
                 list(q = sprintf('identifier:"%s"', x),
                      fl = "identifier, dateUploaded, formatType, obsoletedBy",
                      rows = "20"),
                 as = "data.frame")
}

#' Check pid version
#'
#' This function takes an identifier and checks to see if it has been obsoleted.
#'
#' @param pid (character) The persistent identifier of a data, metadata, or resource map object on a DataONE member node
#'
#' @importFrom dataone CNode getSystemMetadata
#'
#' @export
#'
#' @examples
#' \dontrun{
#'    pid <- "https://pasta.lternet.edu/package/data/eml/edi/195/2/51abf1c7a36a33a2a8bb05ccbf8c81c6"
#'    pid <- "doi:10.18739/A2HF7Z"
#'    pid <- "doi:10.18739/A2ZF6M"
#'    
#'    check_version(pid)
#' }
#'

check_version <- function(pid){
  pid_chunks <- get_chunks(pid)
  results <- lapply(pid_chunks, get_pid_dates)
  results_df <- bind_rows(results) %>% distinct()
  
  # filter out extra types (resource map/etc with similar pid)
  type <- results_df$formatType[results_df$identifier == pid] #check?
  results_typed <- results_df[results_df$formatType == type,]

  if(nrow(results_typed) == 1){
    if(is.null(results_typed$obsoletedBy)){
      print("This is the latest version of the identifier")
    } else {
      print(paste("The identifier has been obsoleted by", results_typed$obsoletedBy))
    }
  } else {
    print("The following identifiers are associated with the identifier provided.")
  }
  
  return(results_typed)
}

