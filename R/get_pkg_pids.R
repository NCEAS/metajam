#' Get PIDs from a package DOI
#'
#' This function takes a package DOI or metadata object PID and optionally a data URL and returns the associated PIDs.
#' Alternative to `url_to_pid()`, based on `arcticdatautils::get_package()`.
#'
#' @param pkg_doi (character) A package DOI or metadata object PID on a DataONE member node. # TODO: figure out terminology
#'
#' @return (list) A list of PIDs.
#'
#' @importFrom dataone query CNode
#' @importFrom purrr %||%
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' #get all pids in the data package
#' get_pkg_pids("https://doi.org/10.6073/pasta/cd7ec5009430eb78f464a66f6794579f")
#' }

get_pkg_pids <- function(pkg_doi) {

  pkg_pid <- check_version(pkg_doi)

  if (pkg_pid$formatType == "RESOURCE") {
    rm_pid <- pkg_pid$identifier
  } else if (pkg_pid$formatType == "METADATA") {
    rm_pid <- pkg_pid$resourceMap
    # handling case the ressource map is not built correctly
    if (is.null(rm_pid)) {
      rm_pid <- pkg_pid$identifier
    }
  } else {
    stop("The data package could not be found. Please check the DOI and try again.")
    #TODO: test on pids that start with dx.doi
  }

  # It seems sometimes the id is duplicated; handles this
  in_case_set_wrong <- strsplit(rm_pid," ")[[1]]
  if (length(in_case_set_wrong) > 1) {
    rm_pid <- in_case_set_wrong[grepl(pattern = "resource_map_doi:", in_case_set_wrong)]
  }

  # Query to get all the pids
  query_params <- list(q = sprintf('resourceMap:"%s"', rm_pid),
                       fl = "identifier, formatType", rows = 10000)
  pids <- dataone::query(dataone::CNode(), query_params, as = "data.frame")

  # Remove EDI data package evaluation reports since they are not "DATA".
  # Including them as "DATA" results in errors.
  pids <- pids[
    !(stringr::str_detect(pids$identifier, 'pasta.lternet.edu') &
      stringr::str_detect(pids$identifier, 'report')), ]

  # Create the list
  pid_list <- list(resource_map = rm_pid,
                   metadata = pids$identifier[pids$formatType == "METADATA"],
                   data = pids$identifier[pids$formatType == "DATA"]) %||% NA

  return(pid_list)
}
