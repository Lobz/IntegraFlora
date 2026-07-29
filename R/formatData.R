#' readGBIF
#'
#' Save some options to read properly from GBIF
#'
#' @param file Filename to pass to readOccurrence or readData
#' @importFrom plantR readData
#' @return data.frame
#' @export
readGBIF <- function(file, ...) {
    if(endsWith(file, ".zip")) {
        readData(file, quote = "", na.strings = c("", "NA"), output = c("occurrence", "verbatim"), ...)
    } else {
        readOccurrence(file = file, sep = "\t", quote = "", ...)
    }
}

#' readJabot
#'
#' Save some options to read properly from Jabot
#'
#' @param file Filename to pass to readOccurrence
#' @return data.frame
#' @export
readJabot <- function(file, ...) {
    readOccurrence(file, sep="|", ...)
}

#' readeReflora
#'
#' Save some options to read properly from Reflora
#'
#' @param file Filename to pass to readOccurrence
#' @return data.frame
#' @export
readReflora <- function(file, ...) {
    readOccurrence(file, ...)
}

#' readSpLink
#'
#' Save some options to read properly from SpeciesLink
#'
#' @param file Filename to pass to readOccurrence
#' @return data.frame
#' @export
readSpLink <- function(file, ...) {
    readOccurrence(file, sep = "\t", quote = "", ...)
}

#' Read darwin core data
#'
#' Basic funtion to read data
#'
#' @examples
#'
#' x <- readOccurrence("data-input/Occurrences/OtherSources/example.csv")
readOccurrence <- function(file, ...) {
    as.data.frame(data.table::fread(file, na.strings = c("", "NA"), ...))
}

#' Specific formatting for GBIF data
#'
#' @param x A data.frame (output from readx)
formatGBIF <- function(x) {
    x$taxonRank <- normalizeTaxonRank(tolower(x$taxonRank))
    x$verbatimBasisOfRecord <- x$basisOfRecord
    x$basisOfRecord <- as.basisOfRecord(x$basisOfRecord)

    if(!"county" %in% names(x)) {
        x$county <- NA
    }
    if(!"municipality" %in% names(x)) {
        x$municipality <- x$county
    }

    x$downloadedFrom <- "x"

    x <- plantR::formatDwc(gbif_data = x)

    x <- selectDesiredFields(x)
    x
}

#' Specific formatting for Jabot data
#'
#' @param x A data.frame (output from readJabot)
#' @return A data.frame formatted with formatDwc
formatJabot <- function(x) {
    # Fix names
    x <- consolidateCase(x)
    names(x)[names(x)=="scientifcname"] <- "scientificName"
    x$county <- NA

    # Normalize taxon Rank
    x$verbatimTaxonRank <- x$taxonRank
    x$taxonRank[grepl(" form.",x$scientificName)] <- "form"
    x$taxonRank <- normalizeTaxonRank(x$taxonRank)

    # Normalize basisOfRecord
    if("basisofrecord" %in% names(x)) {
        x$verbatimBasisOfRecord <- x$basisofrecord
        x$basisofrecord[x$basisofrecord=="Preserved Specimen"] <- "PRESERVED_SPECIMEN"
        x$basisofrecord[x$basisofrecord=="Xiloteca"] <- "PRESERVED_SPECIMEN"
        x$basisofrecord <- as.basisOfRecord(x$verbatimBasisOfRecord)
    } else {
        x$basisOfRecord <- NA
    }

    x$downloadedFrom <- "JABOT"
    x <- plantR::formatDwc(user_data = x)
    x <- selectDesiredFields(x)
    x
}

#' Specific formatting for Reflora data
#'
#' @param x A data.frame (output from readReflora)
#' @return A data.frame formatted with formatDwc
formatReflora <- function(x) {
    x <- parseReflora(x)

    # fix year data
    # dt <- as.Date(x$dateCollected, tryFormats=c("%d/%m/%Y","--/%d/%m%Y"))
    x$year <- getYear(x$dateCollected)

    x$basisOfRecord <- "PRESERVED_SPECIMEN"
    x$basisOfRecord <- as.basisOfRecord(x$basisOfRecord)

    x$downloadedFrom <- "REFLORA"
    x <- plantR::formatDwc(user_data = x)
    x <- selectDesiredFields(x)
    x$taxonRank <- normalizeTaxonRank(x$taxonRank)
    x
}

#' Specific formatting for SpLink data
#'
#' @param x A data.frame (output from readSpLink)
#' @return A data.frame formatted with formatDwc
formatSpLink <- function(x) {
    # Normalize basisOfRecord
    table(x$basisofrecord, useNA="always")
    x$verbatimbasisofrecord <- x$basisofrecord
    x$basisofrecord <- sub("([a-z])([A-Z])", "\\1_\\2", x$basisofrecord)
    x$basisofrecord[which(startsWith(x$basisofrecord, "Machine"))] <- "MACHINE_OBSERVATION"
    x$basisofrecord[which(startsWith(x$basisofrecord, "Preserved"))] <- "PRESERVED_SPECIMEN"
    x$basisofrecord[which(startsWith(x$basisofrecord, "Xil"))] <- "PRESERVED_SPECIMEN"
    x$basisofrecord[x$basisofrecord=="Carpo"] <- "PRESERVED_SPECIMEN"
    x$basisofrecord <- toupper(x$basisofrecord)
    x$basisofrecord <- as.basisOfRecord(x$basisofrecord)

    # Fix names
    x <- consolidateCase(x)

    x$downloadedFrom <- "splink"
    x <- plantR::formatDwc(splink_data = x)
    x <- selectDesiredFields(x)
    x$taxonRank <- normalizeTaxonRank(x$taxonRank)
    x
}

#' Generic formatting for darwinCore data
#'
#' @param x A data.frame (output from readOccurrence)
#' @return A data.frame formatted with formatDwc
formatOccurrence <- function(x) {
    # Fix names
    x <- consolidateCase(x)

    # Normalize basisOfRecord
    if("basisOfRecord" %in% names(x)) {
        x$verbatimBasisOfRecord <- x$basisOfRecord
        x$basisOfRecord <- toupper(x$basisOfRecord)
        x$basisOfRecord <- as.basisOfRecord(x$basisOfRecord)
    } else {
        x$basisOfRecord <- NA
    }
    if(!"county" %in% names(x)) {
        x$county <- NA
    }
    if(!"municipality" %in% names(x)) {
        x$municipality <- x$county
    }

    if(any(!minimumNames %in% names(x))) {
        warning(paste0("Missing required fields: ", paste(setdiff(minimumNames, names(x)), collapse=", "), ". This data will be removed"))
        return(data.frame())
    }
    if(any(!minimumNamesForWorkflow %in% names(x))) {
        warning(paste0("Missing required fields: ", paste(setdiff(minimumNamesForWorkflow, names(x)), collapse=", "), ". Filling with NA"))
        miss <- setdiff(minimumNamesForWorkflow, names(x))
        x[,miss] <- NA
    }
    x <- plantR::formatDwc(user_data = x)
    x <- selectDesiredFields(x)
    x$taxonRank <- normalizeTaxonRank(x$taxonRank)
    x
}

minimumNames <- c("scientificName", "locality")
minimumNamesForWorkflow <- c("institutionCode", "collectionCode",
    "catalogNumber", "recordNumber", "recordedBy", "year",
    "country", "stateProvince", "county", "municipality",
    "locality", "decimalLatitude", "decimalLongitude",
    "identifiedBy", "dateIdentified", "typeStatus", "family",
    "scientificName", "scientificNameAuthorship", "taxonRank", "geodeticDatum")
names(minimumNamesForWorkflow) <- minimumNamesForWorkflow
