# Table function to always include NAs
#' @export
tab <- function(...) { sort(table(..., useNA="always")) }
