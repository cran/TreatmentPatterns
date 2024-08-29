library(testthat)
library(TreatmentPatterns)

test_that("launchResultsExplorer", {
  skip_if_not(ableToRun()$plotting)
  app <- launchResultsExplorer()
  expect_s3_class(app, "shiny.appobj")
  
  rm(app)
  gc()
})
