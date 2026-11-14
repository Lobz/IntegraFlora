library(testthat)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

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

test_that("getTaxonId matches a minimal custom taxonomic backbone", {
  db <- data.frame(
    id = c("bfo-1", "bfo-2"),
    family = c("Fabaceae", "Poaceae"),
    tax.name = c("Acacia mangium", "Poa annua"),
    tax.authorship = c("Willd.", "L."),
    taxon.rank = c("species", "species"),
    stringsAsFactors = FALSE
  )

  dt <- data.frame(
    scientificName = c("Acacia mangium", "Poa annua"),
    scientificNameAuthorship = c("Willd.", "L."),
    stringsAsFactors = FALSE
  )

  out <- getTaxonId(dt, db = db, split.letters = TRUE)

  expect_true(all(c("tax.notes", "id") %in% names(out)))
  expect_equal(out$id, c("bfo-1", "bfo-2"))
  expect_equal(out$tax.notes, c("", ""))
})

test_that("fullGetId fills taxonomic IDs for a small dataset", {
  dt <- data.frame(
    scientificName = c("Acacia mangium", "Poa annua"),
    scientificNameAuthorship = c("Willd.", "L."),
    stringsAsFactors = FALSE
  )

  out <- fullGetId(dt)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("id", "taxon.rank", "tax.notes") %in% names(out)))
  expect_equal(nrow(out), 2L)
  expect_true(all(!is.na(out$id)))
})
