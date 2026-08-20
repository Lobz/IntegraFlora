#' Standardize UC Name
#'
#' Convert conservation unit name to long form and correct mistakes
#'
#' @param x UC names
#'
#' @details In addition to correcting common mistakes and coverting abbreviations to long form, this function also removes connectors between
standardize_uc_name <- function(x) {
    x <- plantR:::squish(x)
    x <- toupper(x)
    x <- sub("^AREA", "ÁREA", x)
    x <- sub(" AREA", "ÁREA", x, fixed = T)
    x <- sub("PATRIM.NIO", "PATRIMÔNIO", x)
    x <- sub("REFUGIO", "REFÚGIO", x, fixed = T)
    x <- sub("PATRIMÔNIO NATURA ", "PATRIMÔNIO NATURAL ", x, fixed = T)
    x <- sub(" AGUAS", " ÁGUAS", x, fixed = T)
    x <- sub("SITIO", "SÍTIO", x, fixed = T)
    # Remove problematic characters
    x <- gsub("\"", "", x, fixed = T)
    x <- gsub("“", "", x, fixed = T)
    x <- gsub("”", "", x, fixed = T)
    x <- gsub(".", "", x, fixed = T)
    x <- gsub("’", "'", x, fixed = T)
    x <- gsub("\\s", " ", x, perl = T)
    x <- gsub(",.*","", x, perl = T)
    # Expand abbreviations and remove connectors
    for(i in 1:nrow(uc_abbrevs)){
        x <- sub(paste0("^", uc_abbrevs$short[i]), uc_abbrevs$long[i], x)
        x <- sub(paste0(uc_abbrevs$long[i]," (D[OAE]S?|-) "), paste0(uc_abbrevs$long[i]," "), x)
    }
    x <- plantR:::squish(x)
    x
}

#' Shorten UC Name
#'
#' Convert conservation unit name to short form (abbreviated)
#'
#' @param x UC names
#'
#' @details This function assumes that the names have already been stantardized. It will not standardize names not correct mistakes, and will not work properly if the names are misspelled.
shorten_uc_name <- function(x) {
    L <- uc_abbrevs$long
    S <- sub("\\|.*","",uc_abbrevs$short)
    for(i in 1:nrow(uc_abbrevs)){
        x <- gsub(L[i], S[i], x)
    }
    x
}

#' Create slug from UC Name
#'
#' Convert conservation unit name into slug for file naming
#'
#' @param x UC names
#'
#' @details This function assumes that the names have already been stantardized. It will not standardize names not correct mistakes, and will not work properly if the names are misspelled.
#' @importFrom plantR rmLatin
slug <- function(x) {
    x <- shorten_uc_name(x)
    x <- plantR::rmLatin(x)
    x <- plantR:::squish(x)
    x <- gsub("\\s+","_",x)
    x
}
