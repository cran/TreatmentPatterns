library(testthat)
library(TreatmentPatterns)

test_that("launchResultsExplorer", {
  app <- launchResultsExplorer()
  expect_s3_class(app, "shiny.appobj")
  
  rm(app)
  gc()
})
