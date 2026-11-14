
#' @title plantRWorkflow_part1
#' @description This function is part of the plantR workflow, and is used to format and validate occurrence and location data.
#' @param x A data.frame containing occurrence data.
#' @export
plantRWorkflow_part1 <- function(x, project_root = ".") {
    # Standardize missing information
    x[x==""] <- NA

    # Apply corrections
    print("Applying corrections...")
    x <- apply_corrections(x, corrections_dir =
                file.path(project_root, "data-input/Occurrences/Corrections"))

    # Lets format this
    print("Formatting occs...")
    x <- plantR::formatOcc(x, noNumb = NA, noYear = NA, noName = NA)

    print("Formatting locs...")
    if(!exists("COUNTRY")) COUNTRY <- "Brazil"
    if(!exists("STATEPROVINCE")) STATEPROVINCE <- "São Paulo"
    STATE <- Sys.getenv("STATE")
    if(STATE == "") STATE <- STATEPROVINCE
    STATEPROVINCE <- STATE

    x <- fixLocation(x, extraTables = file.path(project_root, "data-input/Locations/extraTables"))

    # Treat gps data
    print("Formatting coords...")
    x <- plantR::formatCoord(x)
    x
}

#' Subset to Province
#'
#' This function subsets a data.frame to a specific country and state/province, and removes records that have no location information.
#' @param x A data.frame containing occurrence data.
#' @export
subsetToProvince <- function(x, stateProvince = STATEPROVINCE) {
    print("Subsetting to state...")

    x <- subset(x, country.correct %in% COUNTRY)

    # noCountry <- subset(dt, is.na(country.correct))
    tab(x$stateProvince.correct)
    tab(x$municipality.new[is.na(x$stateProvince.correct)])
    if(stateProvince %in% c("none","",NA,"noState")) {
        x <- subset(x, is.na(stateProvince.correct))
    } else {
        x <- subset(x, stateProvince.correct %in% stateProvince)
    }

    # Select only records that have SOME location info
    noloc <- is.na(x$municipality) & is.na(x$locality) & (x$origin.coord == "coord_gazet")
    x <- x[!noloc,]
    x
}

#' @title plantRWorkflow_part2
#' @description This function is part of the plantR workflow, and is used to format and validate taxonomic and geolocation data.
#' @param x A data.frame containing occurrence data.
#' @export
plantRWorkflow_part2 <- function(x, stateProvince = STATEPROVINCE) {

    # Get taxon IDs
    print("Formatting taxonomy...")
    x <- fullGetId(x)

    # validate
    print("Validating location info...")
    x <- plantR::validateLoc(x)

    print("Validating identification info...")
    # validate taxonomist
    x <- plantR::validateTax(x, generalist = TRUE)
    x$tax.check <- factor(x$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)

    print("Validating geolocation info...")
    if (!exists("latamMap")) {
        assign("latamMap", plantR::latamMap, envir = .GlobalEnv)
    }
    if (!exists("worldMap")) {
        assign("worldMap", plantR::worldMap, envir = .GlobalEnv)
    }
    map <- get("latamMap", envir = .GlobalEnv)$brazil
    map <- subset(map, NAME_1 == tolower(plantR::rmLatin(stateProvince)))
    x <- plantR::validateCoord(x, high.map = map) # WORKING
    x <- tryAgain(x, function(x) is.na(x$decimalLatitude.new), plantR::formatCoord)
    x <- tryAgain(x, function(x) is.na(x$geo.check), plantR::validateCoord, high.map=map)
    tab(is.na(x$geo.check))
    table(x$geo.check, x$origin.coord)

    x
}

#' Validate duplicate records
#'
#' Applies the plantR duplicate-detection routine with project-specific field
#' mappings and duplicate-comparison rules.
#' @param x A data.frame of occurrence records.
#' @return A data.frame with duplicate flags and validation output.
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
