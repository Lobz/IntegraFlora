

basisOfRecords <- c(
    "PRESERVED_SPECIMEN",
    "OCCURRENCE",
    "FOSSIL_SPECIMEN",
    "LIVING_SPECIMEN",
    "HUMAN_OBSERVATION",
    "MATERIAL_SAMPLE",
    "MATERIAL_CITATION",
    "MACHINE_OBSERVATION"
)

#' @export
as.basisOfRecord <- function(x, levels = basisOfRecords) {
    factor(x, levels = levels, labels = basisOfRecords)
}