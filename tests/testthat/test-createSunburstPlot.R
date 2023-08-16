library(TreatmentPatterns)
library(testthat)

# General variables ----
dummyData <- data.frame(
  path = c("A+Z", "B", "C", "A-B", "B-C", "D+Z", "E", "F", "D-E", "D-E-F"),
  freq = c(25, 25, 25, 12, 13, 25, 25, 25, 12, 13),
  sex = rep("all", 10),
  age = rep("all", 10),
  index_year = c(rep("all", 5), rep("2020", 5))
)

tempFile <- file.path(tempdir(), "sunburst.html")

test_that("void", {
  expect_error(
    createSunburstPlot()
  )
})

test_that("no outputFile", {
  expect_error(
    createSunburstPlot(dummyData),
    "argument \"outputFile\" is missing, with no default"
  )
})

test_that("no data", {
  expect_error(
    createSunburstPlot(outputFile = tempFile),
    "argument \"treatmentPathways\" is missing, with no default"
  )
})

test_that("minimal", {
  createSunburstPlot(dummyData, tempFile)

  lines <- readLines(tempFile)
  json <- lines[grep("<textarea  id=\"chartData\" style=\"visibility:hidden; width:\\d+px; height:\\d+px\">", lines) + 1]
  res <- rjson::fromJSON(json)

  expect_identical(length(res$data$children), 8L)
  expect_identical(length(res$data), 2L)
  expect_identical(length(res$lookup), 9L)
})
