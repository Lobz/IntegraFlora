#' Validate duplicate records
#'
#' Applies the plantR duplicate-detection routine with project-specific field
#' mappings and duplicate-comparison rules.
#' @param x A data.frame of occurrence records.
#' @return A data.frame with duplicate flags and validation output.
#' @details This function is a wrapper around the \code{validateDup} function from the
#' \code{plantR} package. It applies the duplicate-detection routine with project-specific field mappings and duplicate-comparison rules. The function takes a data.frame of occurrence records as input and returns a data.frame with duplicate flags and validation output. The function is designed to be used in the context of the IntegraFlora project, and it is not intended for general use. The function is exported for use in other scripts within the project.
#' @export
my_valDup <- function(x) {
  plantR::validateDup(
    x,
    noNumb = NA,
    noYear = NA,
    noName = NA,
    prop = 0.2,
    comb.fields = list(
      c("family", "col.last.name", "col.number", "col.loc"),
      c("family", "col.last.name", "col.number", "col.year"),
      c("species", "col.last.name", "col.number", "col.year"),
      c("species", "col.last.name", "col.number", "col.loc"),
      c("col.year", "col.last.name", "col.number", "col.loc")
    ),
    tax.names = c(
      family = "family.new",
      species = "scientificName.new",
      tax.auth = "scientificNameAuthorship.new",
      det.name = "identifiedBy.new",
      det.year = "yearIdentified.new",
      tax.check = "tax.check",
      tax.rank = "taxon.rank",
      status = "scientificNameStatus",
      id = "id",
      name.full = "scientificNameFull",
      gen = "genus.new",
      sp = "species.new"
    ),
    geo.names = c(
      lat = "decimalLatitude.new",
      lon = "decimalLongitude.new",
      org.coord = "origin.coord",
      prec.coord = "precision.coord",
      geo.check = "geo.check",
      datum = "geodeticDatum"
    ),
    loc.names = c(
      loc.str = "loc.correct",
      res.gazet = "resolution.gazetteer",
      res.orig = "resol.orig",
      loc.check = "loc.check",
      country = "country",
      stateProvince = "stateProvince",
      municipality = "municipality",
      locality = "locality",
      "country.new" = "country.new",
      "stateProvince.new" = "stateProvince.new",
      "municipality.new" = "municipality.new",
      "locality.new" = "locality.new",
      "longitude.gazetteer" = "longitude.gazetteer",
      "latitude.gazetteer" = "latitude.gazetteer"
    ),
    ignore.miss = TRUE
  )
}
