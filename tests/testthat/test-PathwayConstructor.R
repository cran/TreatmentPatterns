library(testthat)
library(TreatmentPatterns)

test_that("new", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(cdm = globals$cdm)
  
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  expect_equal(class(pathwayConstructor), c("PathwayConstructor", "R6"))
})

test_that("getSettings", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(cdm = globals$cdm)
  
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  res <- pathwayConstructor$getSettings()

  expect_equal(length(res), 13)
  expect_equal(class(res), "list")
})

test_that("editSettings", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(cdm = globals$cdm)
  
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  before <- pathwayConstructor$getSettings()
  
  pathwayConstructor$editSettings(minEraDuration = 100)
  
  after <- pathwayConstructor$getSettings()
  
  expect_false(identical(before, after))
})

test_that("getAndromeda", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(cdm = globals$cdm)
  
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  res <- pathwayConstructor$getAndromeda()
  
  expect_null(res)
})

test_that("construct", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(cdm = globals$cdm)
  
  pathwayConstructor <- TreatmentPatterns:::PathwayConstructor$new(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdmInterface = cdmInterface
  )
  
  pathwayConstructor$construct()
  
  res <- pathwayConstructor$getAndromeda()
  
  expect_true(Andromeda::isAndromeda(res))
})
