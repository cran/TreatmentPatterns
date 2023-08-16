if (ableToRun()) {
  library(testthat)
  library(TreatmentPatterns)

  test_that("computePathways DatabaseConnector", {
    expect_message(
      expect_message(
        expect_message(
          computePathways(
            cohorts = cohorts,
            cohortTableName = "CohortTable",
            connectionDetails = connectionDetails,
            cdmSchema = "main",
            resultSchema = "main"
          ),
          "After maxPathLength: 553"
        ),
        "After combinationWindow: 554"
      ),
      "Original number of rows: 8352"
    )
  })


  test_that("computePathways CDMConnector", {
    # CDMConnector's `generateCohortSet` function is not compatible with the
    # default TreatmentPattern cohorts.
    skip()
    computePathways(
      cdm = cdm,
      cohorts = cohorts,
      cohortTableName = "CohortTable"
    )
  })


  test_that("includeTreatments", {
    expect_error(
      expect_error(
        computePathways(
          cohorts = cohorts,
          cohortTableName = "CohortTable",
          connectionDetails = connectionDetails,
          cdmSchema = "main",
          resultSchema = "main",
          includeTreatments = 0
        ),
        "Must be of type 'character'"
      ),
      "Must be a subset of.+'startDate','endDate'.+"
    )
  })

  test_that("periodPriorToIndex", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        periodPriorToIndex = "0"
      ),
      "Must be of type.+'numeric'"
    )
  })

  test_that("minEraDuration", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        minEraDuration = "0"
      ),
      "Must be of type.+'numeric'"
    )
  })

  test_that("splitEventCohorts", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        splitEventCohorts = 1
      ),
      "Must be of type.+'character'"
    )
  })

  test_that("splitTime", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        splitTime = "1"
      ),
      "Must be of type.+'numeric'"
    )
  })

  test_that("eraCollapseSize", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        eraCollapseSize = ""
      ),
      " Must be of type.+'numeric'"
    )
  })

  test_that("combinationWindow", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        combinationWindow = ""
      ),
      "Must be of type.+'numeric'"
    )
  })

  test_that("minPostCombinationDuration", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        minPostCombinationDuration = "Stuff"
      ),
      "Must be of.+type.+"
    )
  })

  test_that("filterTreatments", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        filterTreatments = ""
      ),
      "Must be a subset of"
    )
  })

  test_that("includeTreatments", {
    expect_error(
      computePathways(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        maxPathLength = ""
      ),
      "Must be of type.+'numeric'"
    )
  })
}
