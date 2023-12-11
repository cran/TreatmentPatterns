library(TreatmentPatterns)
library(testthat)
library(dplyr)

# General variables ----
dummyData <- data.frame(
  path = c("A+Z", "B", "C", "A-B", "B-C", "D+Z", "E", "F", "D-E", "D-E-F"),
  freq = c(25, 25, 25, 12, 13, 25, 25, 25, 12, 13),
  sex = rep("all", 10),
  age = rep("all", 10),
  index_year = c(rep("all", 5), rep("2020", 5))
)

tempFile <- file.path(tempdir(), "sankey.html")

fetchJson <- function(tempFile) {
  lines <- readLines(tempFile)

  json <- lines[seq(grep("var datajson =", lines) + 1, grep("];", lines)[1])] %>%
    paste0(collapse = "") %>%
    stringr::str_remove_all(pattern = ";") %>%
    rjson::fromJSON()
  return(json)
}

# Tests ----
test_that("void", {
  expect_error(createSankeyDiagram())
})

test_that("default depth: 2", {
  data <- dummyData %>%
    filter(.data$index_year == "all")

  expect_message(
    createSankeyDiagram(treatmentPathways = data, outputFile = tempFile),
    "Writing Sankey diagram to .+sankey\\.html"
  )

  json <- fetchJson(tempFile)

  expect_true(
    all(
      # 4  A-B   12
      all(unlist(json[1]) == c("1.A", "2.B", "12")),
      # 1    A   25
      all(unlist(json[2]) == c("1.A+Z", "2.Stopped", "25")),
      # 5  B-C   13
      all(unlist(json[3]) == c("1.B", "2.C", "13")),
      # 2    B   25
      all(unlist(json[4]) == c("1.B", "2.Stopped", "25")),
      # 3    C   25
      all(unlist(json[5]) == c("1.C", "2.Stopped", "25"))
    )
  )
})

test_that("default depth: 3", {
  data <- dummyData %>%
    filter(.data$index_year == "2020")

  expect_message(
    createSankeyDiagram(treatmentPathways = data, outputFile = tempFile),
    "Writing Sankey diagram to .+sankey\\.html"
  )

  json <- fetchJson(tempFile)

  expect_true(
    all(
      # Layer 1 -> 2
      all(unlist(json[1]) == c("1.D", "2.E", "6.5")),
      all(unlist(json[2]) == c("1.D", "2.E", "6")),
      all(unlist(json[3]) == c("1.D+Z", "2.Stopped", "12.5")),
      all(unlist(json[4]) == c("1.E", "2.Stopped", "12.5")),
      all(unlist(json[5]) == c("1.F", "2.Stopped", "12.5")),
      # Layer 2 -> 3
      all(unlist(json[6]) == c("2.E", "3.F", "6.5")),
      all(unlist(json[7]) == c("2.E", "3.Stopped", "6")),
      all(unlist(json[8]) == c("2.Stopped", "3.Stopped", "12.5")),
      all(unlist(json[9]) == c("2.Stopped", "3.Stopped", "12.5")),
      all(unlist(json[10]) == c("2.Stopped", "3.Stopped", "12.5"))
    )
  )
})

test_that("minCellCount", {
  data <- dummyData %>%
    filter(.data$index_year == "all")

  suppressMessages(
    createSankeyDiagram(
      treatmentPathways = data,
      outputFile = tempFile,
      minCellCount = 20
    )
  )

  json <- fetchJson(tempFile)

  expect_false(
    all(grepl("2. C", unlist(json))),
    all(grepl("2. B", unlist(json)))
  )
})

test_that("groupCombinations: TRUE", {
  data <- dummyData %>%
    filter(.data$index_year == "all")

  suppressMessages(
    createSankeyDiagram(
      treatmentPathways = data,
      outputFile = tempFile,
      groupCombinations = TRUE
    )
  )

  json <- fetchJson(tempFile)

  expect_true(all(unlist(json[5]) == c("1.Combination", "2.Stopped", "25")))
})

test_that("returnHTML: TRUE", {
  html <- createSankeyDiagram(
      treatmentPathways = dummyData,
      outputFile = tempFile,
      groupCombinations = TRUE,
      returnHTML = TRUE
    )
  
  expect_identical(html$type, "Sankey")
  expect_identical(html$chartid, 1)
  expect_equal(names(html$html), c("header", "chart", "caption", "footer"))
})
