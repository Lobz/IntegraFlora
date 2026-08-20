#' Full join
#'
#' Join two data.frames, avoiding errors
#' importFrom dplyr full_join bind_rows
join <- function(x, y, merge = T) {
    tryCatch({
        # Join
        if(merge) {
            return(dplyr::full_join(x, y))
        } else {
            return(dplyr::bind_rows(x, y))
        }
    }, error = function(e) {
        cat(e$message)
        print("Retrying...")
        # Check that columns of the same name have the same type
        colsX <- names(x)
        colsY <- names(y)
        same <- intersect(colsX, colsY)
        classesX <- lapply(same, function(s) class(x[,s]))
        classesY <- lapply(same, function(s) class(y[,s]))
        ok <- sapply(1:length(same), function(i) length(classesX[i])==length(classesY[i]) & all(classesX[[i]] == classesY[[i]]))
        if(any(!ok)) {
            # convert to character
            x[,same[!ok][1]] <- as.character(x[,same[!ok][1]])
            y[,same[!ok][1]] <- as.character(y[,same[!ok][1]])
        }
        return(join(x, y, merge))
    })
}