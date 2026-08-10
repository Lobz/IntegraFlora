#' Search for a string in location fields of a data.frame
#' @param pattern Pattern to lookup
#' @param corpus A data.frame with locality information
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

#' Consolidate names table
#'
#' @param checkedLocations A table containing alternative names information
#' @export
consolidateNamesTable <- function(checkedLocations = utils::read.csv("data-input/Locations/extraTables/checkedLocations.csv"), extraNames = c()) {

    if(!"slug" %in% names(checkedLocations))
        checkedLocations$slug <- toupper(slug(standardize_uc_name(checkedLocations$Nome_UC)))

    # add oficial names
    if(length(extraNames) > 0) {
        ns <- standardize_uc_name(extraNames)
        officialNames <- data.frame(Nome_UC = ns, Municipio="QUALQUER", Nome_Alternativo = ns, Relação = "Igual", Confiança = "Ouro", slug = slug(extraNames))
        LT <- rbind(checkedLocations, officialNames)
    } else {
        LT <- checkedLocations
    }

    # Generate string for regex grepl in locality data
    LT$uc_strings <- paste0("(",generate_uc_string(LT$Nome_Alternativo),")")
    # Summarize alternative names
    ST <- stats::aggregate(LT$Nome_Alternativo, list(slug = LT$slug, Municipio = LT$Municipio, relationship = LT$Relação, confidenceLocality = LT$Confiança), combineSearchStrings)
}

#' Combine search strings
#'
#' Combine strings into a single string that correspond to searching for all strings
#'
#' @param x An array of patterns
#' @export
combineSearchStrings <- function(x) {
    x <- tolower(unique(x))
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


#' Search for several different entries
#'
#' @param ucs A data.frame containing names
#' @export
nameSearch <- function(ucs, corpus, LT, degrees = "Ouro") {

    # Subset to degrees
    LT <- subset(LT, confidenceLocality %in% degrees)

    # Use regex to look for more occs
    occs_string_mun <- pairwiseMap(LT$x, LT$Municipio, function(str, mun) {
        if(mun=="QUALQUER") {
            res <- searchLoc(str, corpus)
        } else {
            in_mun <- which(corpus$municipality.correct == mun)
            res <- rep(FALSE, nrow(corpus))
            res[in_mun] <- searchLoc(str, corpus[in_mun, ])
        }
        res
    }, simplify = FALSE)
    # Combine positive matches from different municipalities
    occs_string <- sapply(unique(LT$slug), function(n) {Reduce("|", occs_string_mun[LT$slug==n])}, simplify = FALSE, USE.NAMES = TRUE)

    occs_string
}