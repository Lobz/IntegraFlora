library(testthat)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
# source(file.path(project_root, "R", "getTaxonId.R"))
# source(file.path(project_root, "R", "normalizeTaxonRank.R"))

test_that("completeScientificName fills missing scientific names from available fields", {
  dt <- data.frame(
    scientificName = c(NA, "", "Acacia sp.", "Indeterminada"),
    verbatimScientificName = c("Acacia mangium", NA, NA, NA),
    species = c(NA, NA, NA, "Acacia mangium"),
    genus = c("Acacia", NA, NA, NA),
    family = c(NA, NA, NA, "Fabaceae"),
    stringsAsFactors = FALSE
  )

  out <- completeScientificName(dt)

  expect_equal(out$scientificName, c("Acacia mangium", "", "Acacia sp.", "Acacia mangium"))
})

test_that("removeRepeatedAuthorship strips duplicated authorship from scientific names", {
  dt <- data.frame(
    scientificName = c("Acacia mangium Willd. Willd.", "Poa annua L."),
    scientificNameAuthorship = c("Willd.", "L."),
    stringsAsFactors = FALSE
  )

  out <- removeRepeatedAuthorship(dt)

  expect_equal(out$scientificName, c("Acacia mangium Willd.", "Poa annua"))
  expect_equal(out$scientificNameAuthorship, c("Willd.", "L."))
})

test_that("getTaxonRank infers rank from name and existing rank metadata", {
  dt <- data.frame(
    taxon.rank = c(NA, NA, "species", NA),
    taxonRank = c(NA, NA, "species", "genus"),
    scientificName.new = c("Acacia", "Acacia mangium", "Ocimum basilicum", "Ficus"),
    family.new = c("Fabaceae", "Fabaceae", "Lamiaceae", "Moraceae"),
    stringsAsFactors = FALSE
  )

  out <- getTaxonRank(dt)

  expect_equal(as.character(out$taxon.rank), c("species", "genus", "species", "genus"))
})
