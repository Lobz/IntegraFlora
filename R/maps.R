# Functions for reading and consolidating maps

#' Read shp
#'
#' @param filename A .shp file that can be read with st_read
#' @importFrom sf st_read
readShape <- function(filename) {
    tryCatch(st_read(filename),
        error = function(e) { warning(e); NULL })
}
