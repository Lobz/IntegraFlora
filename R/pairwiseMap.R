#' Pairwise Map
#'
#' Applies a function to each pair of members of two vectors
#'
#' @export
pairwiseMap <- function(x, y, FUN, simplify = T, ...) {
    if("list" %in% class(x) & "list" %in% class(y)) {
        lapply(1:length(x), function(i) {FUN(x[[i]], y[[i]], ...)})
    }
    else {
        sapply(1:length(x), function(i) {FUN(x[i], y[i], ...)}, simplify=simplify)
    }
}
