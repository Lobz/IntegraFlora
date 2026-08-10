#' Consolidate Case
#'
#' Unifies columns that have identical names except for case
#'
#' @param x A data.frame
#' @param ns A list of names for reference
consolidateCase <- function(x, ns = goodNames) {
    s <- names(x)
    correct <- ns[match(tolower(s), tolower(ns))]
    correct[is.na(correct)] <- s[is.na(correct)]
    print(paste("replaced", sum(correct!=s), "names"))
    names(x) <- s <- correct
    if(anyDuplicated(tolower(s))) {
        # TODO
        warning("There are duplicated names")
    }
    x
}
