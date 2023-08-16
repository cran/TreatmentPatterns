if (ableToRun()) {
  library(TreatmentPatterns)
  library(testthat)

  andromedaSetup <- Andromeda::loadAndromeda(
    fileName = file.path(setupTempDir, "Andromeda")
  )

  tempDir <- file.path(tempdir(), "testing")

  test_that("void", {
    expect_error(
      export()
    )
  })

  test_that("default", {
    testthat::skip_on_ci()
    expect_message(
      export(
        andromeda = andromedaSetup,
        outputPath = tempDir,
        archiveName = "output.zip"
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

    expect_true(
      file.exists(file.path(tempDir, "output.zip"))
    )
  })
}
