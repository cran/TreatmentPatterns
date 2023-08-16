if (ableToRun()) {
  library(testthat)
  library(TreatmentPatterns)

  tempDir <- file.path(tempdir(), "executeTreatmentPatterns")

  test_that("void", {
    expect_error(TreatmentPatterns::executeTreatmentPatterns())
  })

  test_that("minimal", {
    testthat::skip_on_ci()
    expect_message(
      TreatmentPatterns::executeTreatmentPatterns(
        cohorts = cohorts,
        cohortTableName = "CohortTable",
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main",
        outputPath = tempDir
      )
    )

    expect_true(
      file.exists(file.path(tempDir, "treatmentPathways.csv"))
    )

    expect_true(
      file.exists(file.path(tempDir, "summaryStatsTherapyDuraion.csv"))
    )

    expect_true(
      file.exists(file.path(tempDir, "countsYear.csv"))
    )

    expect_true(
      file.exists(file.path(tempDir, "countsAge.csv"))
    )

    expect_true(
      file.exists(file.path(tempDir, "countsSex.csv"))
    )
  })
}
