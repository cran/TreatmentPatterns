# Global ----
library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(stringr)

test_that("computePathways DatabaseConnector", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()
  
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

# CDMConnector ----
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
  testthat::skip_on_cran()

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
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      includeTreatments = "asdlf"
    )
  )

  Andromeda::close(andromeda_startDate)
  Andromeda::close(andromeda_endDate)
})

test_that("periodPriorToIndex", {
  testthat::skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      periodPriorToIndex = "0"
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("minEraDuration", {
  testthat::skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      minEraDuration = "0"
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("splitEventCohorts", {
  skip_on_cran()
  skip_on_ci()
  
  globals <- generateCohortTableCG()
  
  andromeda_empty <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema,
    splitEventCohorts = NULL
  )
  
  andromeda_Clavulanate <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema,
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
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      splitEventCohorts = "1"
    ),
    "Must be of type.+'integerish'"
  )
  
  Andromeda::close(andromeda_Clavulanate)
  Andromeda::close(andromeda_empty)
})

test_that("splitTime", {
  testthat::skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      splitTime = "1"
    ),
    "Must be of type.+'integerish'"
  )
})

test_that("eraCollapseSize", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()

  globals <- generateCohortTableCG()

  andromeda_0 <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema,
    eraCollapseSize = 0
  )

  andromeda_10000 <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema,
    eraCollapseSize = 10000
  )

  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      eraCollapseSize = ""
    ),
    " Must be of type.+'numeric'"
  )

  Andromeda::close(andromeda_0)
  Andromeda::close(andromeda_10000)
})

test_that("combinationWindow", {
  testthat::skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      combinationWindow = ""
    ),
    "Must be of type.+'numeric'"
  )
})

test_that("minPostCombinationDuration", {
  testthat::skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      minPostCombinationDuration = "Stuff"
    ),
    "Must be of.+type.+"
  )
})

test_that("filterTreatments", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()

  globals <- generateCohortTableCG()

  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      connectionDetails = globals$connectionDetails,
      cdmSchema = globals$cdmSchema,
      resultSchema = globals$resultSchema,
      filterTreatments = ""
    ),
    "Must be a subset of"
  )

  first <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema,
    filterTreatments = "First"
  )

  changes <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema,
    filterTreatments = "Changes"
  )

  all <- computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema,
    filterTreatments = "All"
  )

  expect_true(Andromeda::isAndromeda(first))
  expect_true(Andromeda::isAndromeda(changes))
  expect_true(Andromeda::isAndromeda(all))

  Andromeda::close(first)
  Andromeda::close(changes)
  Andromeda::close(all)
})


test_that("identical treatment timeframe", {
  # Setup connection
  localCon <- DBI::dbConnect(duckdb::duckdb(), dbdir = CDMConnector::eunomia_dir())
  
  withr::defer({
    DBI::dbDisconnect(localCon, shutdown = TRUE)
  })

  # Setup local Cohorts
  localCohorts <- data.frame(
    cohortId = c(28, 6, 3,11),
    cohortName = c("Disease X", "Drug A", "Drug B","Drug C"),
    type = c("target", "event", "event", "event")
  )

  # Setup cohort table
  cohort_table <- tibble(
    cohort_definition_id = c(11, 6, 6, 3, 11, 6, 28),
    subject_id = rep(1, 7),
    cohort_start_date = as.Date(c(
      "2017-03-21", "2017-03-21", "2018-07-18", "2019-06-25",
      "2015-10-05", "2015-10-05", "2016-07-18"
    )),
    cohort_end_date = as.Date(c(
      "2017-06-04", "2017-06-04", "2018-09-11", "2019-07-22",
      "2015-11-29", "2016-10-23", "2020-10-23"
    ))
  )

  # Write cohort table to connection
  dplyr::copy_to(localCon, cohort_table, overwrite = TRUE)

  localCDM <- CDMConnector::cdmFromCon(
    con = localCon,
    cdmSchema = "main",
    writeSchema = "main",
    cohortTables = "cohort_table"
  )

  expect_message(
    andromeda <- TreatmentPatterns::computePathways(
      cohorts = localCohorts,
      cdm = localCDM,
      cohortTableName = "cohort_table"
    ),
    "LRFS Combinations: 1"
  )

  result <- andromeda$treatmentHistory %>%
    collect()

  expect_identical(result$eventCohortId, c("11+6", "6"))
  Andromeda::close(andromeda)
})
