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
  skip_on_ci()
  
  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  cohortTableName <- "cohort_table"
  andromedaTableName <- "cohortTable"
  cdmSchema <- "main"
  resultSchema <- "main"
  
  connection <- DatabaseConnector::connect(connectionDetails)
  
  DatabaseConnector::renderTranslateExecuteSql(
    connection = connection,
    sql = "
  DROP TABLE IF EXISTS cohort_table;

  CREATE TABLE cohort_table (
    cohort_definition_id INT,
    subject_id INT,
    cohort_start_date DATE,
    cohort_end_date DATE
  );

  INSERT INTO cohort_table (
    cohort_definition_id,
    subject_id,
    cohort_start_date,
    cohort_end_date
  ) VALUES
  (3, 1, 2014-10-10, 2015-08-01),
  (2, 1, 2014-11-17, 2014-12-04),
  (1, 1, 2014-10-10, 2015-08-01);
  "
  )
  
  DatabaseConnector::disconnect(connection)
  
  andromeda <- Andromeda::andromeda()
  
  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = cdmSchema,
    resultSchema = resultSchema
  )

  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("Disease X", "Drug A", "Drug B"),
    type = c("target", "event", "event")
  )

  # Viral Sinusitis
  cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    andromeda = andromeda,
    andromedaTableName = andromedaTableName,
    minEraDuration = 0
  )

  res <- andromeda[[andromedaTableName]] %>% dplyr::collect()

  expect_identical(ncol(res), 6L)
  expect_identical(nrow(res), 3L)

  # Empty
  cdmInterface$fetchCohortTable(
    cohorts = data.frame(
      cohortId = numeric(),
      cohortName = character(),
      type = character()
    ),
    cohortTableName = "cohort_table",
    andromeda = andromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = 5
  )

  res <- andromeda[[andromedaTableName]] %>% dplyr::collect()

  expect_identical(ncol(res), 6L)
  expect_identical(nrow(res), 0L)
  
  cdmInterface$destroy()
})
