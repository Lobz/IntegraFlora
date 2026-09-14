
#' @export
memory_usage <- function() {
    objs <- ls(name=".GlobalEnv")
    gets <- sapply(objs, get)
    sizes_raw <- sapply(gets, object.size)
    sizes <- sapply(gets, function(x) format(object.size(x), units='auto'))
    x <- data.frame(objs, sizes, row.names = NULL)
    x <- x[order(sizes_raw, decreasing=T),]
    cat("\nTotal: ")
    print(object.size(x=gets), unit='auto')
    cat("\nGarbage collector:\n")
    print(gc())
    cat("\nPer object:\n")
    print(x)
}
