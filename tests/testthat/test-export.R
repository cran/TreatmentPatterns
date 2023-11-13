# General ----
library(TreatmentPatterns)
library(testthat)

# Tests ----
test_that("void", {
  expect_error(
    TreatmentPatterns::export()
  )
})

# CohortGenerator ----
test_that("outputPath", {
  skip_on_ci()
  skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema)
  
  ## file.path(tempDirCG) ----
  tempDirLocal <- file.path(tempdir(), "output")

  export(andromeda, outputPath = tempDirLocal)

  expect_true(
    file.exists(file.path(tempDirLocal, "treatmentPathways.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "summaryStatsTherapyDuraion.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsYear.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsAge.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsSex.csv"))
  )

  ## 3 ----
  expect_error(
    TreatmentPatterns::export(andromeda, outputPath = 3),
    "Variable 'outputPath': No path provided"
  )
  
  Andromeda::close(andromeda)
})

test_that("ageWindow", {
  skip_on_ci()
  skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )
  
  tempDirLocal <- file.path(tempdir(), "output")

  ## 10 ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      ageWindow = 10
    )
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_true(
    all(c("0-10", "10-20", "20-30", "30-40", "40-50", "all") %in% treatmentPathways$age))

  ## c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150) ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      ageWindow = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150)
    )
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_true(all(
    c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12",
      "12-14", "14-16", "16-18", "18-150", "all") %in% treatmentPathways$age
  ))
  
  Andromeda::close(andromeda)
})

test_that("minFreq", {
  skip_on_ci()
  skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  
  ## 10 ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      minFreq = 10
    ),
    "Removed \\d+ pathways with a frequency < 10."
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_equal(min(treatmentPathways$freq), 10)

  ## "10" ----
  expect_error(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      minFreq = "10"
    )
  )
  
  Andromeda::close(andromeda)
})

test_that("archiveName", {
  skip_on_ci()
  skip_on_cran()
  
  globals <- generateCohortTableCG()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    connectionDetails = globals$connectionDetails,
    cdmSchema = globals$cdmSchema,
    resultSchema = globals$resultSchema
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  
  ## "output.zip" ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      archiveName = "output.zip"
    )
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "output.zip"))
  )

  ## 3 ----
  expect_error(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      archiveName = 3
    )
  )
  
  Andromeda::close(andromeda)
})

# CDMConnector ----
test_that("outputPath", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  
  export(andromeda, outputPath = tempDirLocal)

  expect_true(
    file.exists(file.path(tempDirLocal, "treatmentPathways.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "summaryStatsTherapyDuraion.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsYear.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsAge.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsSex.csv"))
  )

  ## 3 ----
  expect_error(
    export(andromeda, outputPath = 3),
    "Variable 'outputPath': No path provided"
  )
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("ageWindow", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  
  ## 10 ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      ageWindow = 10
    )
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_true(all(c("0-10", "10-20", "all") %in% treatmentPathways$age))

  ## c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150) ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      ageWindow = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150)
    )
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_true(all(
    c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12",
      "12-14", "14-16", "16-18", "18-150", "all") %in% treatmentPathways$age
  ))
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("minFreq", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  
  ## 10 ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      minFreq = 10
    ),
    "Removed \\d+ pathways with a frequency < 10."
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_equal(min(treatmentPathways$freq), 10)

  ## "10" ----
  expect_error(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      minFreq = "10"
    )
  )
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})

test_that("archiveName", {
  skip_on_cran()
  
  globals <- generateCohortTableCDMC()
  
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = globals$cohorts,
    cohortTableName = globals$cohortTableName,
    cdm = globals$cdm
  )
  
  tempDirLocal <- file.path(tempdir(), "output")
  
  ## "output.zip" ----
  expect_message(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      archiveName = "output.zip"
    )
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "output.zip"))
  )

  ## 3 ----
  expect_error(
    export(
      andromeda = andromeda,
      outputPath = tempDirLocal,
      archiveName = 3
    )
  )
  
  Andromeda::close(andromeda)
  DBI::dbDisconnect(globals$con, shutdown = TRUE)
})
