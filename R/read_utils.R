#' @export
read.gbif <- function(file, ...) {
    read.csv(file = file, sep = "\t", quote = NULL, ...)
}

#' @export
turn_to_array_string <- function(s) {
    paste("c(",paste(paste0("'",s,"'"), collapse=", "),")")
}

#' @export
remove_fields <- function(x, to_remove) {

  x[, !names(x) %in% to_remove]
}

# Select fields
f <-  plantR:::fieldNames
#' @export
plantR_fields <- subset(f, type=="required")[,"plantr"]
#' @export
extra_mine <- c("taxonRank", "verbatimScientificName", "acceptedScientificName", "species", "taxonID", "typeStatus", "recordID", "eventDate", "verbatimEventDate", "geodeticDatum", "associatedMedia",  "virtualDuplicates", "duplicates", "barcode", "downloadedFrom", "basisOfRecord", "genus", "taxonRank", "yearIdentified", "originFile")
#' @export
desired_fields <- union(plantR_fields, extra_mine)

#' Select fields
#'
#' A function to reduce the number of columns during data treatement
#'
#' @param x A data frame
#' @param desiredFields A list of names, ideally contained in names(x)
#' @export
selectDesiredFields <- function(x, desiredFields = desired_fields) x[,intersect(names(x), desiredFields)]
