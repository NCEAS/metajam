read_d1_files <- function(folder_path, fnc = "read_csv") {
  
  files <- list.files(folder_path, full.names = TRUE)
  # files <- files[!grepl(pattern='full_metadata.xml', files)]
  filename <- files[grepl(pattern='__summary_metadata.csv', files)]
  filename <- gsub(pattern='__summary_metadata.csv','', basename(filename), fixed = TRUE)
  
  if (sum(filename == tools::file_path_sans_ext(basename(files))) > 1 ){
    stop("You have multiple files named ", filename)
    }
  
  data_meta <- map(files, function(x){
    if (grepl("[^_]+_metadata(?=\\.csv)", basename(x), perl = TRUE)) {
      read_csv(x)
    } else if (tools::file_path_sans_ext(basename(x)) == filename) {
      eval(parse(text = paste0(fnc, '("', x, '")')))
    }
  }) 
  data_meta_names <- map(files, function(x){
    if (grepl("[^_]+_metadata(?=\\.csv)", basename(x), perl = TRUE)) {
      stringr::str_extract(basename(x), "[^_]+_metadata(?=\\.csv)")
    } else if (tools::file_path_sans_ext(basename(x)) == filename){
      "data"
    }
  }) 
  data_meta <- setNames(data_meta, data_meta_names) %>%
    compact()
  return(data_meta)
}
