library(testthat)
library(TreatmentPatterns)
library(CDMConnector)

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
  
  pathwayConstructor$editSettings(minEraDuration = 25)
  
  after <- pathwayConstructor$getSettings()
  
  expect_false(identical(before, after))
  
  expect_warning(
    pathwayConstructor$editSettings(minEraDuration = 10, minPostCombinationDuration = 5)
  )
  
  expect_warning(
    pathwayConstructor$editSettings(minEraDuration = 10, combinationWindow = 5)
  )
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

test_that("datatypes", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     5,           as.POSIXct("2014-01-01"), as.POSIXct("2015-01-01"),
    2,                     5,           as.POSIXct("2014-01-03"), as.POSIXct("2014-03-02"),
    3,                     5,           as.POSIXct("2014-03-10"), as.POSIXct("2014-05-10"),
    2,                     5,           as.POSIXct("2014-05-12"), as.POSIXct("2014-07-12"),
    3,                     5,           as.POSIXct("2014-07-14"), as.POSIXct("2014-09-14")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  expect_error(
    TreatmentPatterns::computePathways(
      cohorts = cohorts,
      cohortTableName = "cohort_table",
      cdm = cdm,
      includeTreatments = "startDate",
      periodPriorToIndex = 0,
      minEraDuration = 0,
      eraCollapseSize = 5,
      combinationWindow = 30,
      minPostCombinationDuration = 30,
      filterTreatments = "All",
      maxPathLength = 5
    )
  )
  
  DBI::dbDisconnect(con)
})
