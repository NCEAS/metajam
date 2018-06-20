read_d1_files <- function(files, fnc = "read_csv") {
  data_meta <- map(files, function(x){
    if (grepl("[^_]+_metadata(?=\\.csv)", basename(x), perl = TRUE)) {
      read_csv(x)
    } else {
      eval(parse(text = paste0(fnc, '("', x, '")')))
    }
  }) 
  data_meta_names <- map(files, function(x){
    if (grepl("[^_]+_metadata(?=\\.csv)", basename(x), perl = TRUE)) {
      stringr::str_extract(basename(x), "[^_]+_metadata(?=\\.csv)")
    } else {
      "data"
    }
  }) 
  data_meta <- setNames(data_meta, data_meta_names)
  return(data_meta)
}