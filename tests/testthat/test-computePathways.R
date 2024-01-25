# Global ----
library(testthat)
library(TreatmentPatterns)
library(dplyr)
library(stringr)

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
        "After maxPathLength: 6675"
      ),
      "After combinationWindow: 6682"
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
        "After maxPathLength: 6675"
      ),
      "After combinationWindow: 6682"
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
    "After maxPathLength: 6919"
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

test_that("minPostCombinationDuration", {
  skip_on_cran()
  globals <- generateCohortTableCDMC()
  
  expect_error(
    computePathways(
      cohorts = globals$cohorts,
      cohortTableName = globals$cohortTableName,
      cdm = globals$cdm,
      minPostCombinationDuration = "Stuff"
    ),
    "Must be of.+type.+"
  )
})

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
