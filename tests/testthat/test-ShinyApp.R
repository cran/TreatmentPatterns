library(testthat)
library(shiny)
library(R6)

test_that("ShinyApp", {
  skip_if_not(ableToRun()$shiny)
  app <- TreatmentPatterns:::ShinyApp$new("app")
  
  # Fields
  expect_identical(app$namespace, "app")
  expect_true(is.R6(app$inputHandler))
  expect_true(is.R6(app$sunburstPlot))
  expect_true(is.R6(app$sankeyDiagram))
  expect_true(is.R6(app$characterizationPlots))
  
  # UI
  expect_s3_class(app$ui(), "shiny.tag")

  # Server
  testServer(app = app$server, {
    expect_true(is.character(session$token))
  })
})
