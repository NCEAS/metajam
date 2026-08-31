`%|||%` <- function (x, y) {
  #based on the purrr/rlang op-null-default
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    y
  }
  else {
    x
  }
}
