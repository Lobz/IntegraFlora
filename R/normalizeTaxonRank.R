
taxonRanks <- c("form", "variety", "subspecies", "species", "genus", "family",
               "order", "class", "phylum", "kingdom")

taxonRankSubstitutions <- c(
    "Infr." = NA,
    "f." = "form",
    "form." = "form",
    "ssp." = "subspecies",
    "subsp." = "subspecies",
    "var." = "variety",
    "specie" = "species",
    "sp." = "species",
    "gen." = "genus",
    "fam." = "family"
)

taxonRankSubstitutions <- data.frame(
    original = c(names(taxonRankSubstitutions), taxonRanks, ""),
    final = c(taxonRankSubstitutions, taxonRanks, NA))

#' @export
as.taxon.rank <- function(x) factor(x, levels = taxonRanks, ordered=TRUE)

#' Normalize taxon rank as factor
#' @export
normalizeTaxonRank <- function(x) {
    x <- taxonRankSubstitutions$final[match(x, taxonRankSubstitutions$original)]
    as.taxon.rank(x)
}
