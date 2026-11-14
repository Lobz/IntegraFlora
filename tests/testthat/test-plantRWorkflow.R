library(testthat)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

make_workflow_data <- function() {
  data.frame(
    institutionCode = c("TEST", "TEST", "TEST"),
    collectionCode = c("TESTCOL", "TESTCOL", "TESTCOL"),
    catalogNumber = c("123", "124", "125"),
    barcode = c("BAR001", "BAR002", "BAR003"),
    recordNumber = c("1", "2", "3"),
    recordedBy = c("Silva", "Souza", "Oliveira"),
    year = c(2020, 2020, 2021),
    country = c("Brazil", "Brazil", "Brazil"),
    stateProvince = c("SP", "RJ", "MG"),
    county = c("São Paulo", "Rio de Janeiro", "Belo Horizonte"),
    municipality = c("São Paulo", "Rio de Janeiro", "Belo Horizonte"),
    locality = c("A", "A", "B"),
    decimalLatitude = c(-23.55, -22.91, -19.92),
    decimalLongitude = c(-46.63, -43.17, -43.94),
    identifiedBy = c("Silva", "Souza", "Oliveira"),
    dateIdentified = c("2020-01-10", "2020-01-12", "2021-02-05"),
    family = c("Fabaceae", "Fabaceae", "Fabaceae"),
    scientificName = c("Acacia sp.", "Acacia sp.", "Acacia mangium"),
    scientificNameAuthorship = c("L.", "L.", "Willd."),
    taxonRank = c("species", "species", "species"),
    geodeticDatum = c("WGS84", "WGS84", "WGS84"),
    col.last.name = c("Silva", "Souza", "Oliveira"),
    col.number = c("123", "124", "125"),
    col.loc = c("X", "Y", "Z"),
    col.year = c(2020, 2020, 2021),
    species = c("Acacia sp.", "Acacia sp.", "Acacia mangium"),
    typestatus = NA,
    stringsAsFactors = FALSE
  )
}

get_workflow_data <- function() {
  if (!exists("workflow_dat", mode = "any", inherits = FALSE)) {
    dat <- formatOccurrence(make_workflow_data())
    dat <- plantRWorkflow_part1(dat, project_root = project_root)
    workflow_dat <<- plantRWorkflow_part2(dat, stateProvince = "São Paulo")
  }
  get("workflow_dat", mode = "any", inherits = FALSE)
}

test_that("plantRWorkflow_part1 formats occurrence data and keeps required columns", {
  dt <- formatOccurrence(make_workflow_data())
  out <- plantRWorkflow_part1(dt, project_root = project_root)

  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(c("country.correct", "stateProvince.correct", "municipality.new", "decimalLatitude.new") %in% names(out)))
})

test_that("subsetToProvince filters to the active country and state", {
  COUNTRY <<- "Brazil"
  STATEPROVINCE <<- "São Paulo"
  on.exit({
    if (exists("COUNTRY", inherits = FALSE, mode = "any")) rm(COUNTRY, envir = .GlobalEnv)
    if (exists("STATEPROVINCE", inherits = FALSE, mode = "any")) rm(STATEPROVINCE, envir = .GlobalEnv)
  }, add = TRUE)

  out <- subsetToProvince(get_workflow_data())

  expect_s3_class(out, "data.frame")
  expect_true(all(out$country.correct %in% COUNTRY))
  expect_true(all(out$stateProvince.correct %in% STATEPROVINCE))
})

test_that("plantRWorkflow_part2 validates taxonomy and coordinates without errors", {
  out <- get_workflow_data()

  expect_s3_class(out, "data.frame")
  expect_gte(nrow(out), 1)
  expect_true(all(c("tax.check", "geo.check") %in% names(out)))
})

test_that("my_valDup runs on a minimal occurrence record", {
  dat <- rbind(get_workflow_data(), get_workflow_data())
  out <- my_valDup(dat)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(dat))
})
