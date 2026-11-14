library(testthat)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(project_root, "R", "corrections.R"))

# Basic correction workflow

test_that("apply_corrections updates rows using the metadata identifiers", {
  tmp_dir <- tempfile("corrections")
  dir.create(tmp_dir)

  correction_meta <- data.frame(
    arquivo = "itapeva_bananal.csv",
    identificadores = "barcode",
    dado_corrigido = "locality",
    stringsAsFactors = FALSE
  )

  write.csv(correction_meta, file.path(tmp_dir, "file_descriptions.csv"), row.names = FALSE)

  corr <- data.frame(
    barcode = c("SPSF00030716", "SPSF00030914", "SPSF00000001"),
    locality = c("old value", "old value 2", "unchanged"),
    stringsAsFactors = FALSE
  )

  write.csv(corr, file.path(tmp_dir, "itapeva_bananal.csv"), row.names = FALSE)

  original <- data.frame(
    barcode = c("SPSF00030716", "SPSF00030914", "SPSF00000001"),
    locality = c("wrong", "wrong", "unchanged"),
    stringsAsFactors = FALSE
  )

  corrected <- apply_corrections(original, corrections_dir = tmp_dir)

  expect_equal(corrected$locality, c("old value", "old value 2", "unchanged"))
})

# Multi-column identifiers should also work

test_that("apply_corrections accepts multiple identifier columns", {
  tmp_dir <- tempfile("corrections_multi")
  dir.create(tmp_dir)

  meta <- data.frame(
    arquivo = "multi_id.csv",
    identificadores = "collectionCode,catalogNumber",
    dado_corrigido = "locality",
    stringsAsFactors = FALSE
  )

  write.csv(meta, file.path(tmp_dir, "file_descriptions.csv"), row.names = FALSE)

  corr <- data.frame(
    collectionCode = c("SPSF", "SPSF"),
    catalogNumber = c("30716", "30717"),
    locality = c("EEC BANANAL", "EEC USP"),
    stringsAsFactors = FALSE
  )

  write.csv(corr, file.path(tmp_dir, "multi_id.csv"), row.names = FALSE)

  original <- data.frame(
    collectionCode = c("SPSF", "SPSF"),
    catalogNumber = c("30716", "30717"),
    locality = c("old", "old"),
    stringsAsFactors = FALSE
  )

  corrected <- apply_corrections(original, corrections_dir = tmp_dir)

  expect_equal(corrected$locality, c("EEC BANANAL", "EEC USP"))
})

test_that("apply_corrections accepts multiple target fields in one metadata row", {
  tmp_dir <- tempfile("corrections_multiple_targets")
  dir.create(tmp_dir)

  meta <- data.frame(
    arquivo = "multi_target.csv",
    identificadores = "barcode",
    dado_corrigido = "locality,municipality",
    stringsAsFactors = FALSE
  )

  write.csv(meta, file.path(tmp_dir, "file_descriptions.csv"), row.names = FALSE)

  corr <- data.frame(
    barcode = c("SPSF00030716", "SPSF00030914"),
    locality = c("EEC BANANAL", "EEC BANANAL"),
    municipality = c("Bananal", "Bananal"),
    stringsAsFactors = FALSE
  )

  write.csv(corr, file.path(tmp_dir, "multi_target.csv"), row.names = FALSE)

  original <- data.frame(
    barcode = c("SPSF00030716", "SPSF00030914"),
    locality = c("wrong", "wrong"),
    municipality = c("old town", "old town"),
    stringsAsFactors = FALSE
  )

  corrected <- apply_corrections(original, corrections_dir = tmp_dir)

  expect_equal(corrected$locality, c("EEC BANANAL", "EEC BANANAL"))
  expect_equal(corrected$municipality, c("Bananal", "Bananal"))
})
