`%|||%` <- function (x, y) {
  #based on the purrr/rlang op-null-default
  if (is.null(x) | is.na(x)) {
    y
  }
  else {
    x
  }
}
