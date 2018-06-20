# Download data package

# merged csv's - add as option?
# pros
  # fewer files - single key file
  # better if dataset files have similar structure
# cons
  # different output than other fxn 
  # easier to see whether files are missing metadata
  # merge attributes that have nothing to do with each other

# 1) Get package 
# 2) Run lapply over download object
# downloadD1Data
# meta_obj = "doi.org/10.18739/A2TR4Q"
download_d1_data_pkg <- function(meta_obj, path) {
  stopifnot(is.character(meta_obj))
  stopifnot(dir.exists(path))
  
  ## Try to get DataONE data_id from data_obj ---------
  meta_obj <- utils::URLdecode(meta_obj)
  meta_version <- check_version(meta_obj, formatType = "metadata")
  
  
  
  # check to see if there's a new version of the package and throw warning
  # does not n
  
}




