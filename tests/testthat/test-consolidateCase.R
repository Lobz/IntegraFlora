library(testthat)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
# source(file.path(project_root, "R", "consolidateCase.R"))
# load(file.path(project_root, "R", "sysdata.rda"), envir = .GlobalEnv)

test_that("consolidateCase normalizes names case-insensitively", {
  x <- data.frame(GBIFID = 1, AccessRights = 2)

  out <- consolidateCase(x)

  expect_equal(names(out), c("gbifID", "accessRights"))
  expect_equal(out$gbifID, 1)
  expect_equal(out$accessRights, 2)
})
