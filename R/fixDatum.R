#' fixDatum
#'
#' Unifies all datum for a data.frame of sf points. Datum information must be in a column "geodeticDatum"
#'
#' @param x A non-empty data.frame of sf points
#' @param convert.to Desired datum. Values will be passed to function sf::st_crs(). Defaults to SIRGAS 2000 (EPSG:4674).
#' @param na What datum do you assume NA or unkown values to be. Defaults to SIRGAS 2000 (EPSG:4674).
#'
#' @importFrom sf st_crs st_transform
#' @export
fixDatum <- function(x, convert.to = "EPSG:4674", na = "EPSG:4674") {

    # Figure out datum
    datum <- toupper(x$geodeticDatum)
    table(datum)
    datum[grepl("84", datum)] <- "WGS84"
    datum[grepl("4326", datum)] <- "EPSG:4326"
    datum[grepl("SAD69", datum)] <- "EPSG:4618"
    datum[grepl("ED50", datum)] <- "EPSG:4230"
    datum[grepl("TWD67", datum)] <- "EPSG:3821"
    datum[grepl("SIRGAS 200", datum)] <- "EPSG:4674"

    # What do we consider NA values to be
    datum[grepl("NOT|UNKNOWN|DESCONHECIDO", datum)] <- na
    datum[is.na(datum)] <- na

    x$datum.new <- datum
    split_x <- by(x, datum, function(d) {
        st_crs(d) <- d$datum.new[1]
        d <- st_transform(d, convert.to)
        d
    })
    y <- do.call(rbind, split_x)

    y
}
