#' Try Again
#'
#' @description Try to run some function on a data.frame again, but only on rows that have passed some condition, then merge back to the original data.frame, but only the rows that don't pass condition.
#'
#' @param x A data.frame
#' @param condition A function that checks which lines must be tried again
#' @param FUN A function that takes a data frame, alters it and returns it
#' @param add_cols Logical. If TRUE, cols added by the function FUN will be added to the result. Defaults to FALSE.
#' @param ... Aditional parameters to pass to FUN
#'
#' @details You may pass aditional parameters to the function FUN. This function assumes that FUN preserves the order of the rows of x, and that condition() may be applied to both x and FUN(x). FUN may change the values of any columns of x, but only on rows that pass condition before FUN and do not pass condition after FUN.
#' @examples
#'
#' x <- data.frame(a = 1:100, b = 100:1)
#' y <- tryAgain(x, function(x) {x$a > x$b}, function(x) {x$b <- x$b*2; x})
#' @export
tryAgain <- function(x, condition, FUN, success_condition = function(x) !condition(x), add_cols = FALSE, label = NULL, ...) {
    # Label
    if(is.null(label)) {
        label = ""
    } else {
        label = paste0(label,": ")
    }

    # Select rows to treat
    unmatched <- condition(x)
    if(!any(unmatched, na.rm=T)) {
        print(paste0(label, "No lines satisfy condition"))
        return(x)
    }

    unmatched <- which(unmatched)
    to_rematch <- x[unmatched, ]

    # Apply function
    results <- FUN(to_rematch, ...)

    # Select which rows have succesfully changed with FUN
    rematched <- success_condition(results)
    if(!any(rematched, na.rm=T)) {
        print(paste0(label, "Retrying wielded no results"))
        return(x)
    } else {
        print(paste0(label, "Replaced ", sum(rematched, na.rm=T), " lines"))
    }

    newresults <- unmatched[which(rematched)] # let's replace these
    results <- results[which(rematched),]

    # Check if the result has less cols than the original
    missing_cols <- setdiff(names(x), names(results))
    if(length(missing_cols) > 0) {
        missing_data <- x[newresults, missing_cols]
        results[, missing_cols] <- missing_data
    }

    # If the result has added cols, we can keep them or not
    if(add_cols) {
        new_cols <- setdiff(names(results), names(x))
        x[,new_cols] <- NA
    }

    # Reorder fields to match the order of x
    results <- results[, names(x)]

    # Merge back and return
    x[newresults,] <- results
    x
}
