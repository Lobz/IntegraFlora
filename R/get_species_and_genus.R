#' Get species and genus
#'
#' plantR doesn't return these by default so I have to do it myself
#'
#' @importFrom stringr str_extract
#' @export
get_species_and_genus <- function(x) {
    if(!"species.new" %in% names(x))
        x$species.new <- NA
    if(!"genus.new" %in% names(x))
        x$genus.new <- NA

    x$taxon.rank <- factor(x$taxon.rank, levels = taxonRanks, ordered=TRUE)

    sp <- which(x$taxon.rank <= "species")
    x$species.new[sp] <- stringr::str_extract(x$scientificName.new[sp], "^[\\w|-]+ [\\w|-]+")
    x$genus.new[sp] <- stringr::str_extract(x$scientificName.new[sp], "[\\w|-]+")
    gen <- which(x$taxon.rank == "genus")
    x$species.new[gen] <- NA
    x$genus.new[gen] <- stringr::str_extract(x$scientificName.new[gen], "[\\w|-]+")
    fam <- which(x$taxon.rank >= "family")
    x$species.new[fam] <- NA
    x$genus.new[fam] <- NA
    x
}
