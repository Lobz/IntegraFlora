
#' Which records have not been matched?
#' @export
not_found <- function(x) {
    x$tax.notes == "not found" | grepl("not resolved|+1",x$tax.notes)
}
#' @export
found <- function(x) !not_found(x)

#' getTaxonId
#'
#' Try to get taxon ID using many different strategies
#'
#' @param total A data.frame containing identification information with columns "scientificName", "scientificNameAuthorship", "genus"
#' @param complete Run all possible strategies? If false, will run a default formatTax. Defaults to TRUE
#' @param rm.miss Remove data with no identification info? Defaults to FALSE
#' @importFrom plantR formatTax
#' @export
getTaxonId <- function(total, complete = TRUE, rm.miss = FALSE, na.values = c("Indeterminado", "INDETERMINADA", "ndeterminado", "Indet", "INDET.", "sp.", "Plantae"), ...) {
    # Fix some issues with taxonomy:

    # Remove indeterminate markers
    invalid <- total$scientificName %in% na.values
    total$scientificName[invalid] <- NA

    # Some records don't have scientificName for some reason
    noName <- is.na(total$scientificName)
    table(noName)
    # if verbatim is present, use that
    if("verbatimScientificName" %in% names(total)) {
        total$scientificName[noName] <- total$verbatimScientificName[noName]
        noName <- is.na(total$scientificName)
    }
    # if species is present, use that
    if("species" %in% names(total)) {
        total$scientificName[noName] <- total$species[noName]
        noName <- is.na(total$scientificName)
    }
    table(noName)
    # else, use genus
    if("genus" %in% names(total)) {
        total$scientificName[noName] <- total$genus[noName]
        noName <- is.na(total$scientificName)
        table(noName)
    }
    # last resort, use family
    if("family" %in% names(total)) {
        total$scientificName[noName] <- total$family[noName]
        noName <- is.na(total$scientificName)
        table(noName)
    }
    # Remove indeterminate markers
    invalid <- total$scientificName %in% na.values
    total$scientificName[invalid] <- NA
    noName <- is.na(total$scientificName)
    table(noName)


    # Remove records with no identification?
    if(rm.miss) {
        total <- subset(total, !is.na(scientificName))
    }

    # Match scientificName to oficial F&FBR backbone
    if("tax.notes" %in% names(total)) {
        total <- tryAgain(total, not_found, formatTax, label = "Default formatTax", ...)
    } else {
        total <- formatTax(total, split.letters = TRUE, ...)
    }

    if (complete) {
    # Try again with verbatim
    if("verbatimScientificName" %in% names(total)) {
        total <- tryAgain(total, function(x) not_found(x) & x$scientificName != x$verbatimScientificName, formatTax, tax.name = "verbatimScientificName", label = "Verbatim", ...)
    }

    # we're gonna try again without author (see issue #170 in plantR)
    # total <- tryAgain(total, not_found, formatTax, use.authors = F)

    # And again with author
    # total <- tryAgain(total, not_found, formatTax, tax.name = "verbatimScientificName", use.author = F)

    # For records that have authorship inside scientific name, we want to remove that
    try(
    total <- tryAgain(total,
        condition = function(x) {
            not_found(x) & pairwiseMap(x$scientificNameAuthorship, x$scientificName, grepl, fixed = T)
            },
        FUN = function(x, ...) {
            x$scientificName <- plantR:::squish(pairwiseMap(x$scientificNameAuthorship, x$scientificName, function(x,y) sub(x, "", y, fixed = T)))
            x$scientificName <- sub(", \\d+","",x$scientificName)
            x <- formatTax(x, ...)
            x
        },
        success_condition = found,
        label = "Removed auth", ...)
    )

    # Isolate authorship
    total <- tryAgain(total, not_found, function(x, ...) {formatTax(isolateAuthorship(x), ...)}, label = "Isolate", ...)

    # What's still unmatched? Genus rank
    total <- tryAgain(total, condition = function(x) not_found(x) & x$taxonRank=="genus", FUN = formatTax, tax.name = "genus", label = "Genus", ...)

    # What's still unmatched? Vars and subspecies
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x, ...) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 var. ", x$scientificName)
            x <- formatTax(x, ...)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "Var.", ...)
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x, ...) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 subsp. ", x$scientificName)
            x <- formatTax(x, ...)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "Subsp.", ...)
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x, ...) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 f. ", x$scientificName)
            x <- formatTax(x, ...)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "F.", ...)

    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x, ...) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 form ", x$scientificName)
            x <- formatTax(x, ...)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "F.", ...)

    # What's still unmatched? Try again with less rigor?
    # total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, sug.dist=0.8 )

    # Finally, if something is still unmatched, give up and match higher taxon rank
    total <- tryAgain(total,
        condition = function(x) {not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName)},
        FUN = function(x, ...) {
            saved <- x$scientificName
            x$scientificName <- sub("(^\\w+ \\w+).*", "\\1", x$scientificName)
            x <- formatTax(x, ...)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "Remove infra", ...)
    }

    # Finally, match taxons from other dbs to bfo
    total <- tryAgain(total,
        condition = function(x) {found(x) & !startsWith(x$id, "bfo")},
        FUN = function(x) {
            saved <- x$scientificName
            saved2 <- x$scientificNameAuthorship
            x$scientificName <- x$scientificName.new
            x$scientificNameAuthorship <- x$scientificNameAuthorship.new
            x <- formatTax(x)
            x$scientificName <- saved
            x$scientificNameAuthorship <- saved2
            x
        },
        success_condition = function(x) {found(x) & startsWith(x$id, "bfo")},
        label = "Match back to BFO")


    # fix missing taxon rank
    total$taxon.rank <- as.taxon.rank(total$taxon.rank)
    table((total$taxon.rank), useNA = "always")
    table(is.na(total$taxon.rank))
    # If there's another entry of the same taxon, get the taxon rank from there
    total <- total[order(total$taxon.rank),]
    fix_these <- which(is.na(total$taxon.rank))
    x<- total$scientificName.new[fix_these]
    total$taxon.rank[fix_these] <- total$taxon.rank[match(x, total$scientificName.new)]
    # If possible, get it from taxonRank
    fix_these <- which(is.na(total$taxon.rank))
    total$taxon.rank[fix_these] <- tolower(total$taxonRank[fix_these])
    # Otherwise, look at scientific name
    fix_these <- which(is.na(total$taxon.rank))
    x<- total$scientificName.new[fix_these]
    x <- sub(" sp.","",x)
    rank <- rep(NA, length(fix_these))
    rank[grepl(" ",x)] <- "species"
    rank[grepl(" \\w+ ",x)] <- "subspecies" # anything with more than two words is less than species
    rank[grepl(" subsp[. ]",x)] <- "subspecies"
    rank[grepl(" var[. ]",x)] <- "variety"
    rank[x == total$family.new[fix_these]] <- "family"
    rank[is.na(rank)] <- "genus" # single word and not family? genus.
    total$taxon.rank[fix_these] <- rank

    # get species and genus?
    total <- get_species_and_genus(total)

    total
}
