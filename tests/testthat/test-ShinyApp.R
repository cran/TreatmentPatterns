library(testthat)
library(shiny)
library(R6)

test_that("ShinyApp", {
  app <- TreatmentPatterns:::ShinyApp$new("app")
  
  # Fields
  expect_identical(app$namespace, "app")
  expect_true(is.R6(app$inputHandler))
  expect_true(is.R6(app$interactivePlots))
  expect_true(is.R6(app$characterizationPlots))
  
  # UI
  expect_s3_class(app$ui(), "shiny.tag")

  # Server
  testServer(app = app$server, {
    expect_true(is.character(session$token))
  })
})
