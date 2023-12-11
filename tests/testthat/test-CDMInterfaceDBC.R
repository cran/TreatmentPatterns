library(testthat)
library(TreatmentPatterns)
library(dplyr)
require("Eunomia", character.only = TRUE, quietly = TRUE)
library(DatabaseConnector)

test_that("Method: new", {
  skip_on_cran()
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  expect_true(R6::is.R6(
    TreatmentPatterns:::CDMInterface$new(
      connectionDetails = connectionDetails,
      cdmSchema = "main",
      resultSchema = "main"
    )
  ))
})

test_that("Method: validate", {
  skip_on_cran()
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  expect_true(R6::is.R6(cdmInterface$validate()))
})

test_that("Method: fetchMetadata", {
  skip_on_cran()
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  andromeda <- Andromeda::andromeda()

  cdmInterface$fetchMetadata(andromeda)

  metadata <- andromeda$metadata %>% collect()

  expect_in(
    c("cdmSourceName", "cdmSourceAbbreviation", "cdmReleaseDate", "vocabularyVersion"),
    names(metadata)
  )

  expect_identical(metadata$rVersion, base::version$version.string)
  expect_identical(metadata$platform, base::version$platform)
  expect_identical(nrow(metadata), 1L)
  expect_identical(ncol(metadata), 8L)
})

test_that("Method: fetchCohortTable", {
  skip_on_cran()
  # skip_on_ci()
  
  globals <- generateCohortTableCG()
  
  andromeda <- Andromeda::andromeda()
  andromedaTableName <- "cohortTable"
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )

  cdmInterface$fetchCohortTable(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    andromeda = andromeda,
    andromedaTableName = andromedaTableName,
    minEraDuration = 0
  )

  expect_equal(names(andromeda), andromedaTableName)
})

test_that("fetchCohortTable: empty", {
  skip_on_cran()
  # skip_on_ci()
  
  globals <- generateCohortTableCG()
  
  andromeda <- Andromeda::andromeda()
  andromedaTableName <- "cohortTable"
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )
  
  cohorts <- data.frame(
    cohortId = numeric(),
    cohortName = character(),
    type = character()
  )
  
  # Empty
  cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = globals$cohortTableName,
    andromeda = andromeda,
    andromedaTableName = andromedaTableName,
    minEraDuration = 5
  )
  
  res <- andromeda[[andromedaTableName]] %>% dplyr::collect()
  
  expect_identical(ncol(res), 6L)
  expect_identical(nrow(res), 0L)
})

test_that("Method: disconnect", {
  skip_on_cran()
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )
  
  andromeda <- Andromeda::andromeda()

  cdmInterface$disconnect()
  
  expect_error(cdmInterface$fetchMetadata(andromeda))
})
