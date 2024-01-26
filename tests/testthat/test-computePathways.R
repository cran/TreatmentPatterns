# Global ----
library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(stringr)
library(CDMConnector)

test_that("computePathways DatabaseConnector", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  
  globals <- generateCohortTableCG()
  
  expect_message(
    expect_message(
      expect_message(
        computePathways(
          cohorts = globals$cohorts,
          cohortTableName = globals$cohortTableName,
          connectionDetails = globals$connectionDetails,
          cdmSchema = "main",
          resultSchema = "main"
        ),
        "After maxPathLength: 554"
      ),
      "After combinationWindow: 555"
    ),
    "Original number of rows: 8352"
  )
})

test_that("computePathways CDMConnector", {
  skip_on_cran()
  globals <- generateCohortTableCDMC()
  
  expect_message(
    expect_message(
      expect_message(
        computePathways(
          cdm = globals$cdm,
          cohorts = globals$cohorts,
          cohortTableName = globals$cohortTableName
        ),
        "After maxPathLength: 554"
      ),
      "After combinationWindow: 555"
    ),
    "Original number of rows: 8352"
  )
  
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("nrow exitCohorts > 0", {
  skip_on_cran()
  globals <- generateCohortTableCDMC()
  
  cohorts <- globals$cohorts %>%
    mutate(type = case_when(
      .data$cohortName == "Acetaminophen" ~ "exit",
      .default = .data$type
    ))
  
  expect_message(
    computePathways(
      cdm = globals$cdm,
      cohorts = cohorts,
      cohortTableName = globals$cohortTableName
    ),
    "After maxPathLength: 2117"
  )
})

# Parameter sweep ----
test_that("includeTreatments", {
  skip_on_cran()
  globals <- generateCohortTableCDMC()

  andromeda_startDate <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    includeTreatments = "startDate"
  )

  andromeda_endDate <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    includeTreatments = "endDate"
  )

  startDate <- andromeda_startDate$treatmentHistory %>% dplyr::collect()
  endDate <- andromeda_endDate$treatmentHistory %>% dplyr::collect()

  expect_false(identical(
    startDate$eventStartDate,
    endDate$eventStartDate
  ))

  expect_false(identical(
    startDate$durationEra,
    endDate$durationEra
  ))

  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      includeTreatments = "asdlf"
    )
  )

  Andromeda::close(andromeda_startDate)
  Andromeda::close(andromeda_endDate)
})

test_that("periodPriorToIndex", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      periodPriorToIndex = "0"
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("minEraDuration", {
  testthat::skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      minEraDuration = "0"
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("splitEventCohorts", {
  skip_on_cran()
  globals <- generateCohortTableCDMC()
  
  andromeda_empty <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    splitEventCohorts = NULL
  )
  
  andromeda_Clavulanate <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    splitEventCohorts = 4,
    splitTime = 30
  )

  empty <- andromeda_empty[["treatmentHistory"]] %>% collect()
  clavulanate <- andromeda_Clavulanate[["treatmentHistory"]] %>% collect()
  
  expect_false(identical(empty$eventCohortId, clavulanate$eventCohortId))
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      splitEventCohorts = "1"
    ),
    "Must be of type.+'integerish'"
  )
  
  Andromeda::close(andromeda_Clavulanate)
  Andromeda::close(andromeda_empty)
})

test_that("splitTime", {
  testthat::skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      splitTime = "1"
    ),
    "Must be of type.+'integerish'"
  )
})

test_that("eraCollapseSize", {
  skip_on_cran()
  globals <- generateCohortTableCDMC()

  andromeda_0 <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    eraCollapseSize = 0
  )

  andromeda_10000 <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    eraCollapseSize = 10000
  )

  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      eraCollapseSize = ""
    ),
    " Must be of type.+'numeric'"
  )

  Andromeda::close(andromeda_0)
  Andromeda::close(andromeda_10000)
})

test_that("combinationWindow", {
  skip_on_cran()
  globals <- generateCohortTableCDMC()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      combinationWindow = ""
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("minPostCombinationDuration: 30", {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  
  cohorts <- data.frame(
    cohortId = c(1, 2, 3),
    cohortName = c("X", "A", "B"),
    type = c("target", "event", "event")
  )
  
  cohort_table <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date,    ~cohort_end_date,
    1,                     1,           as.Date("2020-01-01"), as.Date("2023-01-01"),
    2,                     1,           as.Date("2020-01-01"), as.Date("2020-03-01"),
    3,                     1,           as.Date("2020-01-10"), as.Date("2020-03-15")
  )
  
  copy_to(con, cohort_table, overwrite = TRUE)
  
  cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main", cohortTables = "cohort_table")
  
  ## 30 ----
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 3,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A+B")
  
  ## 12 ----
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 3,
    combinationWindow = 30,
    minPostCombinationDuration = 12,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A+B-B")
  
  
  ## 8 ----
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "cohort_table",
    cdm = cdm,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    eraCollapseSize = 3,
    combinationWindow = 30,
    minPostCombinationDuration = 8,
    filterTreatments = "All",
    maxPathLength = 5
  )
  
  tempDir <- tempdir()
  TreatmentPatterns::export(andromeda, tempDir, minCellCount = 1)
  
  treatmentPaths <- read.csv(file.path(tempDir, "treatmentPathways.csv"))
  
  path <- treatmentPaths %>%
    dplyr::filter(
      .data$age == "all",
      .data$sex == "all",
      .data$indexYear == "all") %>%
    dplyr::pull(.data$path)
  
  expect_identical(path, "A-A+B-B")
  
  DBI::dbDisconnect(con)
})
p
test_that("filterTreatments", {
  skip_on_cran()
  globals <- generateCohortTableCDMC()

  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      filterTreatments = ""
    ),
    "Must be a subset of"
  )

  first <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    filterTreatments = "First"
  )

  changes <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    filterTreatments = "Changes"
  )

  all <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm,
    filterTreatments = "All"
  )

  expect_true(Andromeda::isAndromeda(first))
  expect_true(Andromeda::isAndromeda(changes))
  expect_true(Andromeda::isAndromeda(all))

  Andromeda::close(first)
  Andromeda::close(changes)
  Andromeda::close(all)
})
