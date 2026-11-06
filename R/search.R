#' Search for a pattern in one or more location fields
#'
#' This helper performs a case-insensitive regex match on a set of location
#' columns after normalizing accents with \code{rmLatin()}. It is intended for
#' matching locality-related terms, optionally across a municipal field.
#'
#' @param pattern A regular expression to search for.
#' @param corpus A data frame containing the locality information to scan.
#' @param fieldA Name of the primary locality field to search. Defaults to
#'   \code{"locality"}.
#' @param fieldB Name of the secondary locality field to search. Set to
#'   \code{""} to ignore this field. Defaults to \code{"municipality"}.
#' @return A logical vector with one element per row in \code{corpus}, where
#'   \code{TRUE} indicates a match in either field.
#' @export
searchLoc <- function(pattern, corpus, fieldA = "locality", fieldB = "municipality") {
    x <- grepl(pattern, x = rmLatin(corpus[,fieldA]), ignore.case = TRUE, perl = TRUE)
    if(fieldB != "") {
        y <- grepl(pattern, x = rmLatin(corpus[,fieldB]), ignore.case = TRUE, perl = TRUE)
        x | y
    } else {
        x
    }
}

#' Consolidate alternative names for location matching
#'
#' Prepare a names table for downstream searches by normalizing unique slugs and
#' generating regex fragments for each alternative name. The output is meant to be
#' used with search routines that compare alternative locality names against the
#' occurrence records in a corpus.
#'
#' @param checkedLocations A data frame with alternative/locality names to be
#'   consolidated.
#' @param extraNames An optional character vector of additional official names to
#'   append to the table.
#' @return A data frame with consolidated names, a normalized slug, and a
#'   generated \code{uc_strings} expression for regex matching.
#' @export
consolidateNamesTable <- function(checkedLocations = utils::read.csv("data-input/Locations/extraTables/checkedLocations.csv"), extraNames = c()) {

    if (!is.data.frame(checkedLocations)) {
        stop("checkedLocations must be a data.frame.")
    }

    if (!"slug" %in% names(checkedLocations)) {
        checkedLocations$slug <- toupper(slug(standardize_uc_name(checkedLocations$Nome_UC)))
    }

    # add official names
    if (length(extraNames) > 0) {
        officialNames <- data.frame(
            Nome_UC = standardize_uc_name(extraNames),
            Municipio = "QUALQUER",
            Nome_Alternativo = extraNames,
            Relação = "Igual",
            Confiança = "Ouro",
            slug = slug(standardize_uc_name(extraNames)),
            stringsAsFactors = FALSE
        )
        LT <- rbind(checkedLocations, officialNames)
    } else {
        LT <- checkedLocations
    }

    # Generate string for regex grepl in locality data
    LT$uc_strings <- paste0("(", generate_uc_string(LT$Nome_Alternativo), ")")

    # Summarize alternative names
    ST <- stats::aggregate(LT$Nome_Alternativo, list(slug = LT$slug, Municipio = LT$Municipio, relationship = LT$Relação, confidenceLocality = LT$Confiança), combineSearchStrings)
}

#' Combine several alternative search strings into one regex
#'
#' This helper takes a vector of candidate strings and creates a single regular
#' expression that matches any of them. The result is suitable for use with
#' \code{grepl()} and related pattern-matching operations.
#'
#' @param x A character vector containing alternative strings to match.
#' @param optimize Logical indicating whether the regex should be optimized to
#'   reduce redundancy. Defaults to \code{FALSE}.
#' @return A single regex string that matches any of the strings in \code{x}.
#' @export
combineSearchStrings <- function(x, optimize = FALSE) {
    # Generate a regex string that matches all strings in x
    x <- tolower(unique(x))
    if (!optimize) {
        return(paste0("(", paste(generate_uc_string(x), collapse = "|"), ")"))
    }
    st <- generate_uc_string(x[1])
    while(TRUE) {
        # print(x)
        (m <- grepl(st, rmLatin(x)))
        # discard matched strings
        x <- x[!m]
        if (length(x) > 0) {
            # pick one string
            y <- generate_uc_string(x[1])
            # check if this string is better than the previous one
            # if(grepl(y, st)) {
                # st <- y
            # } else {
                st <- paste0(st, "|", y)
            # }
        } else {
            return(st)
        }
    }
}


#' Search for multiple alternative names across the occurrence corpus
#'
#' Match a set of candidate names against the supplied corpus, optionally
#' restricting the search to a municipality when one is available. The function
#' returns a named list of logical vectors, one per unique slug.
#'
#' @param ucs A data frame or object containing the names to search. This
#'   argument is retained for API compatibility and is not directly used in the
#'   current implementation.
#' @param corpus A data frame with occurrence records to test.
#' @param LT A table of alternative names and metadata, including a slug and
#'   municipality field.
#' @param degrees Character vector indicating the confidence levels to keep when
#'   subsetting \code{LT}. Defaults to \code{"Ouro"}.
#' @return A named list of logical vectors, each aligned with the rows in
#'   \code{corpus} and indicating whether a given slug matched any occurrence.
#' @export
nameSearch <- function(ucs, corpus, LT, degrees = "Ouro") {

    # Subset to degrees
    LT <- subset(LT, confidenceLocality %in% degrees)

    # Use regex to look for more occs
    occs_string_mun <- pairwiseMap(LT$x, LT$Municipio, function(str, mun) {
        if (is.na(mun) || mun %in% c("", "QUALQUER", "Qualquer")) {
            res <- searchLoc(str, corpus)
        } else {
            in_mun <- which(corpus$municipality.correct == mun)
            res <- rep(FALSE, nrow(corpus))
            res[in_mun] <- searchLoc(str, corpus[in_mun, ])
        }
        res
    }, simplify = FALSE)

    # Combine positive matches from different municipalities
    occs_string <- sapply(unique(LT$slug), function(n) {
        Reduce("|", occs_string_mun[LT$slug == n])
    }, simplify = FALSE, USE.NAMES = TRUE)

    occs_string
}