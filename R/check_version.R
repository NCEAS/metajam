#' @title Check PID version
#'
#' @description This function takes an identifier and checks to see if it has been obsoleted.
#'
#' @param pid (character) The persistent identifier of a data, metadata, or resource map object on a DataONE member node.
#' @param formatType (character) Optional. The format type to return (one of 'data', 'metadata', or 'resource').
#'
#' @return (data.frame) A data frame of object version PIDs and related information.
#'
#' @importFrom dataone query CNode
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Most data URLs and identifiers work
#' check_version("https://cn.dataone.org/cn/v2/resolve/urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570")
#' check_version("doi:10.18739/A2ZF6M")
#'
#' # Specify a formatType (data, metadata, or resource)
#' check_version("doi:10.18739/A2ZF6M", formatType = "metadata")
#'
#' # Returns a warning if the identifier has been obsoleted
#' check_version("doi:10.18739/A2HF7Z", formatType = "metadata")
#'
#' # Returns an error if no matching identifiers are found
#' check_version("a_test_pid")
#'
#' # Returns a warning if several identifiers are returned
#' check_version("10.18739/A2057CR99")
#' }

check_version <- function(pid, formatType = NULL) {

  # Error out for missing pids
  if(all(is.character(pid), nchar(pid) > 0) != TRUE)
    stop("Argument 'pid' must be character class with non-zero number of characters.")

  # Error out for missing / unsupported `formatType` specification
  if(is.null(formatType) != TRUE & all(is.character(formatType), length(formatType) == 1, formatType %in% c("data", "metadata", "resource")) != TRUE)
    stop("Argument 'formatType' should either be NULL or one of 'data', 'metadata', or 'resource'.")

  # Query DataONE for the specified `pid`
  while (nchar(pid) > 5) {
    results <- suppressMessages(
      dataone::query(x = dataone::CNode(),
                     solrQuery = list(q = sprintf('identifier:"%s"', pid),
                                      fl = "identifier, dateUploaded, formatType, obsoletedBy, resourceMap"),
                     as = "data.frame")
    )
    # If results is null or empty dataframe, remove part of the URI
    if (is.null(results) || nrow(results) == 0) {
      pid <- gsub(pattern = "^[^/=]+[/=]*", replacement = "", x = pid)

      # If multiple are returned, break
    } else { break }
  }

  # filter out extra types (resource map/etc with similar pid)
  if (!is.null(formatType)) {
    formatType <- toupper(formatType)
    results <- results[results$formatType == formatType, ]
  }

  if (nrow(results) == 0) {
    stop("No matching identifiers were found.")
  } else if (nrow(results) == 1) {
    if (is.null(results$obsoletedBy) || is.na(results$obsoletedBy)) {
      message(results$identifier, " is the latest version for the provided persistent identifier")
    } else {
      warning("The identifier has been obsoleted by ", results$obsoletedBy)
    }
  } else {
    warning("Several identifiers are associated with ", pid)
  }

  return(results) }
