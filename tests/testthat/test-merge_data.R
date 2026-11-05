library(testthat)
library(sf)

project_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

# Basic helper behavior

test_that("column_or_value returns the requested column or falls back to a literal", {
  dt <- data.frame(name = c("A", "B"), value = c(1, 2))

  expect_equal(column_or_value("name", dt), dt$name)
  expect_equal(column_or_value("literal", dt), "literal")
})

# Real shapefile metadata workflow

test_that("formatShapeInfo standardizes a real shapefile using the metadata map", {
  info <- read.csv(
    file.path(project_root, "data-input", "Locations", "shapes", "file_descriptions.csv"),
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )

  shp_path <- file.path(project_root, "data-input", "Locations", "shapes", info$arquivo[1])
  skip_if_not(file.exists(shp_path))

  shape <- sf::st_read(shp_path, quiet = TRUE)
  out <- formatShapeInfo(shape, i = 1L, info_files = info)

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), nrow(shape))
  expect_true(all(c(
    "name", "country", "stateProvince", "municipality",
    "protectionCategory", "UC_ID", "source", "type", "slug"
  ) %in% names(out)))
  expect_equal(unique(out$source), info$arquivo[1])
  expect_true(all(out$country == "brazil"))
})

test_that("readShapeInfo reads and combines all metadata-listed shapes", {
  info <- read.csv(
    file.path(project_root, "data-input", "Locations", "shapes", "file_descriptions.csv"),
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )

  first_shape <- file.path(project_root, "data-input", "Locations", "shapes", info$arquivo[1])
  skip_if_not(file.exists(first_shape))

  all_shapes <- readShapeInfo(
    info_files = info,
    shapes_dir = file.path(project_root, "data-input", "Locations", "shapes")
  )

  expect_class(all_shapes, "list")
  expect_all(sapply(all_shapes, function(x) inherits(x, "sf")))

  merged_shapes <- merge_shapes(all_shapes)

  expect_s3_class(merged_shapes, "sf")
  expect_gt(nrow(merged_shapes), 0L)
  expect_equal(nrow(merged_shapes), sum(sapply(all_shapes, nrow)))
  expect_equal(length(unique(merged_shapes$source)), nrow(info))
  expect_true(all(c(
    "name", "country", "stateProvince", "municipality",
    "protectionCategory", "UC_ID", "source", "type", "slug"
  ) %in% names(merged_shapes)))
  expect_true(length(unique(merged_shapes$source)) >= 2L)
})

test_that("merge_shapes preserves the default metadata columns and accepts keep_cols='all'", {
  s1 <- sf::st_sf(
    name = "A",
    id = 1,
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  )
  s2 <- sf::st_sf(
    name = "B",
    status = "active",
    geometry = sf::st_sfc(sf::st_point(c(1, 1)), crs = 4326)
  )

  merged <- merge_shapes(list(s1, s2))
  expect_s3_class(merged, "sf")
  expect_equal(nrow(merged), 2L)
  expect_true(all(c("name", "geometry") %in% names(merged)))

  merged_all <- merge_shapes(list(s1, s2), keep_cols = "all")
  expect_s3_class(merged_all, "sf")
  expect_equal(nrow(merged_all), 2L)
  expect_true(all(c("name", "geometry", "id", "status") %in% names(merged_all)))
})