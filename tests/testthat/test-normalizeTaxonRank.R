library(testthat)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
# source(file.path(project_root, "R", "normalizeTaxonRank.R"))

test_that("normalizeTaxonRank translates common rank abbreviations", {
  out <- normalizeTaxonRank(c("sp.", "var.", "gen.", "subsp.", "Infr."))

  expect_s3_class(out, "ordered")
  expect_equal(as.character(out), c("species", "variety", "genus", "subspecies", NA))
})
