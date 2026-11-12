#' Compare taxonomic lists
#'
#' Summarizes differences in species, genera, and families between two checklists.
#' @param l1 First checklist data.frame.
#' @param l2 Second checklist data.frame; defaults to the UC catalog table.
#' @return Invisibly, the printed comparison statistics.
#' @export
compareLists <- function(l1, l2 = UC_catalogo) {
    sp1 <- unique(l1$species.new)
    sp2 <- unique(l2$species.new)
    gen1 <- unique(l1$genus.new)
    gen2 <- unique(l2$genus.new)
    f1 <- unique(l1$family.new)
    f2 <- unique(l2$family.new)
    print(c("List 1", sum(!is.na(sp1)), length(f1)))
    print("SPECIES")
    print(c(length(setdiff(sp1, sp2)), length(setdiff(sp2, sp1))))
    print("FAMILIES")
    print(c(length(setdiff(f1, f2)), length(setdiff(f2, f1))))
}
