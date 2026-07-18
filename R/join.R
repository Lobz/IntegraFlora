#' Full join
#'
#' Join two data.frames, avoiding errors
#' importFrom dplyr full_join
join <- function(x, y) {
    # Check that columns of the same name have the same type
    colsX <- names(x)
    colsY <- names(y)
    same <- intersect(colsX, colsY)
    classesX <- lapply(same, function(s) class(x[,s]))
    classesY <- lapply(same, function(s) class(y[,s]))
    ok <- sapply(1:length(same), function(i) length(classesX[i])==length(classesY[i]) & all(classesX[[i]] == classesY[[i]]))
    if(any(!ok)) {
        # convert to character
        x[,same[!ok]] <- lapply(x[,same[!ok]], as.character)
        y[,same[!ok]] <- lapply(y[,same[!ok]], as.character)
    }
    # Join
    res <- dplyr::full_join(x, y)
}