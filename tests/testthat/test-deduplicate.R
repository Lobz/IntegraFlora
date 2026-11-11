library(testthat)

test_that("my_valDup runs on a minimal occurrence record", {
  dt <- data.frame(
    institutionCode = c("TEST", "TEST", "TEST"),
    collectionCode = c("TESTCOL", "TESTCOL", "TESTCOL"),
    catalogNumber = c("123", "124", "125"),
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
    scientificName.new = c("Acacia sp.", "Acacia sp.", "Acacia mangium"),
    scientificNameAuthorship.new = c("L.", "L.", "Willd."),
    identifiedBy.new = c("Silva", "Souza", "Oliveira"),
    yearIdentified.new = c(2020L, 2020L, 2021L),
    tax.check = c(NA_character_, NA_character_, NA_character_),
    taxon.rank = c("species", "species", "species"),
    scientificNameStatus = c("accepted", "accepted", "accepted"),
    id = c(1L, 2L, 3L),
    scientificNameFull = c("Acacia sp.", "Acacia sp.", "Acacia mangium Willd."),
    genus.new = c("Acacia", "Acacia", "Acacia"),
    species.new = c("sp.", "sp.", "mangium"),
    decimalLatitude.new = c(-23.55, -22.91, -19.92),
    decimalLongitude.new = c(-46.63, -43.17, -43.94),
    origin.coord = c("coords_original", "coords_original", "coords_original"),
    precision.coord = c("high", "high", "high"),
    geo.check = c("ok", "ok", "ok"),
    loc.correct = c("A", "A", "B"),
    resolution.gazetteer = c("locality", "locality", "locality"),
    resol.orig = c("locality", "locality", "locality"),
    loc.check = c("good", "good", "good"),
    stringsAsFactors = FALSE
  )

  dat <- formatOccurrence(dt)
  dat <- plantRWorkflow_part1(dat)
  dat <- plantRWorkflow_part2(dat, stateProvince = "São Paulo")
  expect_s3_class(dat, "data.frame")
  expect_gte(nrow(dat), 2L)

  dat <- rbind(dat, dat)

  out <- my_valDup(dat)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(dat))
})
