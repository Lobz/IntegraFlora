#' Isolate Problem Cases
#'
#' @importFrom parallel parSapply detectCores makeCluster
#' @export
isolateProblemCases <- function(x, FUN, breaks = 10, parallel = FALSE, no_cores = detectCores() -1, ...) {

    if(!"function" %in% class(FUN)) {
        stop("FUN must be a function")
    }
    n <- nrow(x)
    groups <- cut(1:n, breaks)
    l_orig <- split(x, groups)
    if(parallel && no_cores > 1) {
        # Initiate cluster
        cl <- makeCluster(no_cores)
        l_prob <- parSapply(makeCluster(no_cores), l_orig, function(x1) {
            tryCatch(
                {FUN(x1, ...); FALSE},
                error = function(e) {print(e); return(TRUE)}
            )
        })
    }
    else {
        l_prob <- vapply(l_orig, function(x1) {
            tryCatch(
                {FUN(x1, ...); FALSE},
                error = function(e) {return(TRUE)}
            )
        }, TRUE)
    }
    l_prob <- l_orig[l_prob]
    # if(recursive) {
        # l_prob <- lapply(l_prob, FUN = function(x2) isolateProblemCases(x2, FUN=FUN, breaks = breaks, recursive = F))
    # }
    x <- dplyr::bind_rows(l_prob)
    x
}

loc.cols <- c("country", "stateProvince", "municipality", "locality")
loc.cols.plantR <- c("country.new", "stateProvince.new", "municipality.new", "locality.new", "locality.scrap", "resol.orig", "loc", "loc.correct", "latitude.gazetteer", "longitude.gazetteer", "resolution.gazetteer")
geo.cols <- c("decimalLatitude", "decimalLongitude", "geodeticDatum")
geo.cols.plantR <- c("decimalLatitude.new", "decimalLongitude.new", "origin.coord", "precision.coord", "coord.check")
tax.cols <- c("scientificName", "family")
