#' Add admin information to data.frame
#'
#' @author Mali Oz C Salles
#'
#' @param x A data.frame treated with plantR::formatLoc()
#' @return The same data.frame, with added columns "loc.correct.admin", "country.correct", "stateProvince.correct", "municipality.correct", "locality.correct" and "source.loc", taken from the result of plantR::getAdmin()
addAdmin <- function(x) {

    locs <- plantR::getAdmin(x$loc.correct)
    names(locs)<-c("loc.correct.admin", "country.correct", "stateProvince.correct", "municipality.correct", "locality.correct", "source.loc")
    x[,names(locs)] <- NULL
    x <- cbind(x,locs)
    x
}
