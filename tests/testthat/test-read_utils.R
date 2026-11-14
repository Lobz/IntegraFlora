library(testthat)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
# source(file.path(project_root, "R", "read_utils.R"))

test_that("turn_to_array_string produces a valid R vector literal", {
  expect_equal(turn_to_array_string(c("A", "B")), "c( 'A', 'B' )")
})

test_that("remove_fields drops only the requested columns", {
  dt <- data.frame(a = 1:2, b = 3:4, c = 5:6, stringsAsFactors = FALSE)

  out <- remove_fields(dt, "b")

  expect_equal(names(out), c("a", "c"))
  expect_equal(out$a, c(1, 2))
  expect_equal(out$c, c(5, 6))
})

test_that("selectDesiredFields keeps only the requested columns", {
  dt <- data.frame(a = 1:2, b = 3:4, c = 5:6, stringsAsFactors = FALSE)

  out <- selectDesiredFields(dt, desiredFields = c("c", "a"))

  expect_equal(names(out), c("a", "c"))
  expect_equal(out$a, c(1, 2))
  expect_equal(out$c, c(5, 6))
})

test_that("read.gbif reads tab-separated files with the expected columns", {
  tmp <- tempfile(fileext = ".txt")
  write.table(
    data.frame(
      recordID = c("1", "2"),
      scientificName = c("Acacia sp.", "Acacia mangium"),
      stringsAsFactors = FALSE
    ),
    file = tmp,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  out <- read.gbif(tmp)

  expect_s3_class(out, "data.frame")
  expect_equal(names(out), c("recordID", "scientificName"))
  expect_equal(out$scientificName, c("Acacia sp.", "Acacia mangium"))
})
