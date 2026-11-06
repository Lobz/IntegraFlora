library(testthat)

test_that("searchLoc matches a pattern across locality and municipality fields", {
  corpus <- data.frame(
    locality = c("A", "B", "C"),
    municipality = c("M", "N", "M"),
    stringsAsFactors = FALSE
  )

  expect_equal(searchLoc("A", corpus), c(TRUE, FALSE, FALSE))
  expect_equal(searchLoc("M", corpus), c(TRUE, FALSE, TRUE))
  expect_equal(searchLoc("Z", corpus), c(FALSE, FALSE, FALSE))
})

test_that("combineSearchStrings merges alternative UC name variants into one regex", {
  x <- c("Aparecida", "Aparecida do Norte", "Aparecida do Sul")

  out <- combineSearchStrings(x, optimize = TRUE)
  out_simple <- combineSearchStrings(x, optimize = FALSE)

  expect_type(out, "character")
  expect_type(out_simple, "character")

  expected_result <- paste0("(", paste(generate_uc_string(x), collapse = "|"), ")")
  expect_equal(out_simple, expected_result)

  sample_text <- tolower(c(
    "Aparecida do Norte",
    "Aparecida",
    "Município de Aparecida do Sul",
    "Outra localidade"
  ))
  grep_results <- c(TRUE, TRUE, TRUE, FALSE)
  expect_equal(grepl(out, sample_text, perl = TRUE), grep_results)
  expect_equal(grepl(out_simple, sample_text, perl = TRUE), grep_results)

  expect_gte(nchar(out_simple), nchar(out))
})

test_that("consolidateNamesTable creates the LT structure used by nameSearch", {
  checked <- data.frame(
    Nome_UC = c("A", "A"),
    Municipio = c("M", "QUALQUER"),
    Nome_Alternativo = c("A", "A"),
    Relação = c("Igual", "Igual"),
    Confiança = c("Ouro", "Ouro"),
    slug = c("S1", "S1"),
    stringsAsFactors = FALSE
  )

  out <- consolidateNamesTable(checked, extraNames = c("A"))

  expect_s3_class(out, "data.frame")
  expect_true(all(c("slug", "Municipio", "relationship", "confidenceLocality", "x") %in% names(out)))
  expect_true(any(out$Municipio == "QUALQUER"))
  expect_true(any(out$confidenceLocality == "Ouro"))
})

test_that("nameSearch finds rows matching the same locality logic as getOccs", {
  corpus <- data.frame(
    locality = c("A", "B", "C"),
    municipality = c("M", "N", "M"),
    municipality.correct = c("M", "N", "M"),
    stringsAsFactors = FALSE
  )

  LT <- data.frame(
    x = c("A", "C"),
    Municipio = c("M", "QUALQUER"),
    slug = c("s1", "s1"),
    confidenceLocality = c("Ouro", "Ouro"),
    stringsAsFactors = FALSE
  )

  out <- nameSearch(data.frame(slug = "s1"), corpus, LT)

  expect_type(out, "list")
  expect_true("s1" %in% names(out))
  expect_equal(unname(out[["s1"]]), c(TRUE, FALSE, TRUE))
})
