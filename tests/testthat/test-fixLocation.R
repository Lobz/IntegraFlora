library(testthat)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
extra_tables <- file.path(project_root, "data-input", "Locations", "extraTables")

test_that("loadExtraLocationTables loads the supplemental gazetteer data", {
  loaded <- loadExtraLocationTables(extra_tables)

  expect_type(loaded, "list")
  expect_true(all(c("gazet", "uniqueMunicipalities") %in% names(loaded)))
  expect_s3_class(loaded$uniqueMunicipalities, "data.frame")
  expect_gt(nrow(loaded$gazet), 0)
  expect_gt(nrow(loaded$uniqueMunicipalities), 0)
})

test_that("fixLocation accepts preloaded gazetteer tables", {
  loaded <- loadExtraLocationTables(extra_tables)

  dt <- data.frame(
    country = c("Brazil", "Brazil"),
    stateProvince = c("SP", "sao paulo"),
    municipality = c("sao paulo", "campinas"),
    locality = c("bairro do limao", "jardim das flores"),
    stringsAsFactors = FALSE
  )

  expect_warning({
    out <- fixLocation(
      dt,
      selectedCountry = "Brazil",
      extraTables = extra_tables,
      gazet = loaded$gazet,
      uniqueMunicipalities = loaded$uniqueMunicipalities
    )
  }, "reserved to plantR")

  expect_s3_class(out, "data.frame")
  expect_true(all(c("country.new", "stateProvince.new", "municipality.new", "locality.new") %in% names(out)))
  expect_equal(nrow(out), nrow(dt))
})
