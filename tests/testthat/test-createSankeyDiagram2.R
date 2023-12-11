library(TreatmentPatterns)
library(testthat)

dummyData <- data.frame(
  path = c("A+Z", "B", "C", "A-B", "B-C", "D+Z", "E", "F", "D-E", "D-E-F"),
  freq = c(25, 25, 25, 12, 13, 25, 25, 25, 12, 13),
  sex = rep("all", 10),
  age = rep("all", 10),
  index_year = c(rep("all", 5), rep("2020", 5))
)

test_that("void", {
  expect_error(createSankeyDiagram2())
})

test_that("minimal", {
  p <- createSankeyDiagram2(treatmentPathways = dummyData)
  
  pLabels <- stringr::str_remove_all(string = p$x$nodes$name, pattern = "\\d\\.")
  pLabels <- pLabels["Stopped" != pLabels] |>
    unique() |>
    sort()
  
  actualLabels <- stringr::str_split(string = dummyData$path, pattern = "-") |>
    unlist() |>
    unique() |>
    sort()
  
  expect_identical(pLabels, actualLabels)
})

test_that("groupCombinations: TRUE", {
  p <- createSankeyDiagram2(treatmentPathways = dummyData, groupCombinations = TRUE)
  
  pLabels <- stringr::str_remove_all(string = p$x$nodes$name, pattern = "\\d\\.")
  pLabels <- pLabels["Stopped" != pLabels] |>
    unique() |>
    sort()
  
  actualLabels <- stringr::str_split(string = dummyData$path, pattern = "-") |>
    unlist() |>
    stringr::str_replace_all(pattern = ".+\\+.+", replacement = "Combination") |>
    unlist() |>
    unique() |>
    sort()
  
  expect_identical(pLabels, actualLabels)
})

test_that("colors", {
  actualColors <- c("#ff33cc", "#ff0000", "#00ff00", "#0000ff", "#ffffff", "#000000")
  
  p <- createSankeyDiagram2(treatmentPathways = dummyData, colors = actualColors)
  
  pColors <- p$x$options$colourScale
  
  expect_identical(pColors, actualColors)
})

test_that("2 path levels", {
  dummyData <- data.frame(
    path = c("A", "A-B+C", "A-D", "B+C", "D"),
    freq = c(206, 6, 14, 48, 221),
    sex = rep("all", 5),
    age = rep("all", 5),
    index_year = rep("all", 5)
  )
  
  p <- createSankeyDiagram2(treatmentPathways = dummyData)
  
  pLabels <- stringr::str_remove_all(string = p$x$nodes$name, pattern = "\\d\\.")
  pLabels <- pLabels["Stopped" != pLabels] |>
    unique() |>
    sort()
  
  actualLabels <- stringr::str_split(string = dummyData$path, pattern = "-") |>
    unlist() |>
    unique() |>
    sort()
  
  expect_identical(pLabels, actualLabels)
})
