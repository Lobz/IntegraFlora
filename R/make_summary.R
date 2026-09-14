#' Make summary
#'
#' This function factorizes a column in each data frame to make a coherent summary to be tabulated
#'
#' @param data A list of data.frames
#' @param column The name of the columns to be summarized
#' @param levels Possible values in the column (used in factor())
#' @param labels Rename the levels, as in factor()
#' @param UC Array of names (same length as data)
#' @export
make_summary <- function(data, column, levels, labels = levels, UC = sapply(data, function(x) {
    if("UC" %in% names(x)) x$UC[1]
    else if("Nome_UC" %in% names(x)) x$Nome_UC[1]
})) {
    ret <- lapply(data, function(x) {
        x <- x[,column]
        x <- factor(x, levels = levels, labels = labels)
        summary(x)
    })
    ret <- dplyr::bind_rows(ret)
    ret <- cbind(UC, ret)
    ret
}
