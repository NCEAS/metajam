#' Check pid version
#'
#' This function takes an identifier and checks to see if it has been obsoleted.
#'
#' @param pid (character) The persistent identifier of a data, metadata, or resource map object on a DataONE member node.
#' @param formatType (character) Optional. The format type to return (DATA, METADATA, RESOURCE).
#'
#' @return A data.frame of object version PIDs and related information.
#'
#' @import dataone
#' @import dplyr
#' @import stringr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Most data URL's and identifiers work
#' check_version("https://cn.dataone.org/cn/v2/resolve/urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570")
#'
#' # Returns a warning if several identifiers are returned:
#' check_version(
#'   "https://pasta.lternet.edu/package/data/eml/edi/195/2/51abf1c7a36a33a2a8bb05ccbf8c81c6"
#'   )
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

check_version <- function(pid, formatType = NULL) {

  # check that pid is of type character
  if (!all(is.character(pid), all(nchar(pid) > 0))) {
    stop("Argument 'pids' must be character class with non-zero number of characters.")
  }

  while (nchar(pid) > 5) {
    results <- suppressMessages(
      dataone::query(dataone::CNode(),
                     list(q = sprintf('identifier:"%s"', pid),
                          fl = "identifier, dateUploaded, formatType, obsoletedBy, resourceMap"),
                     as = "data.frame")
    )
    #if results is null or empty dataframe, remove part of the URI
    if (is.null(results) || nrow(results) == 0) {
      pid <- gsub("^[^/=]+[/=]*", "", pid)

    } else {
      #what to do if multiple are returned
      break
    }
  }

  if (nrow(results) == 0) {
    stop("No matching identifiers were found.")
  }

  # filter out extra types (resource map/etc with similar pid)
  if (!is.null(formatType)) {
    formatType <- toupper(formatType)
    results <- results[results$formatType == formatType,]
  }

  if (nrow(results) == 1) {
    if (is.null(results$obsoletedBy) || is.na(results$obsoletedBy)) {
      message("\n",
              results$identifier,
              "\nis the latest version for identifier\n",
              pid)
    } else {
      warning("The identifier has been obsoleted by ", results$obsoletedBy)
    }
  } else {
    warning("Several identifiers are associated with ", pid)
  }

  return(results)
}
