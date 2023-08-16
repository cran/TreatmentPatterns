## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----libs, eval=FALSE---------------------------------------------------------
#  library(dplyr)

## ----setupCohorts, eval=FALSE-------------------------------------------------
#  # Select Viral Sinusitis Cohort
#  targetCohorts <- cohortsGenerated %>%
#    filter(cohortName == "ViralSinusitis") %>%
#    select(cohortId, cohortName)
#  
#  # Select everything BUT Viral Sinusitis cohorts
#  eventCohorts <- cohortsGenerated %>%
#    filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
#    select(cohortId, cohortName)
#  
#  exitCohorts <- cohortsGenerated %>%
#    filter(cohortName == "Death") %>%
#    select(cohortId, cohortName)
#  
#  cohorts <- dplyr::bind_rows(
#    targetCohorts %>% mutate(type = "target"),
#    eventCohorts %>% mutate(type = "event"),
#    exitCohorts %>% mutate(type = "exit")
#  )

## ----eval=FALSE---------------------------------------------------------------
#  tempDir <- tempdir()
#  allDir <- file.path(tempDir, "all_in_one")
#  
#  TreatmentPatterns::executeTreatmentPatterns(
#    cohorts = cohorts,
#    cohortTableName = "CohortTable",
#    outputPath = allDir,
#    connectionDetails = connectionDetails,
#    cdmSchema = "main",
#    resultSchema = "main",
#    # Optional settings
#    includeTreatments = "startDate",
#    periodPriorToIndex = 0,
#    minEraDuration = 0,
#    splitEventCohorts = "",
#    splitTime = 30,
#    eraCollapseSize = 30,
#    combinationWindow = 30,
#    minPostCombinationDuration = 30,
#    filterTreatments = "First",
#    maxPathLength = 5,
#    minFreq = 5,
#    addNoPaths = TRUE
#  )

## ----eval=FALSE---------------------------------------------------------------
#  andromeda <- TreatmentPatterns::computePathways(
#    cohorts = cohorts,
#    cohortTableName = "CohortTable",
#    connectionDetails = connectionDetails,
#    cdmSchema = "main",
#    resultSchema = "main"
#  )

## ----intermediateResults, eval=FALSE------------------------------------------
#  names(andromeda)

## ----treatmentHistory, eval=FALSE---------------------------------------------
#  andromeda$treatmentHistory

## ----eval=FALSE---------------------------------------------------------------
#  segDir <- file.path(tempDir, "segmented")
#  TreatmentPatterns::export(andromeda, outputPath = segDir)

## ----eval=FALSE---------------------------------------------------------------
#  cdmDir <- file.path(tempDir, "CDMCon")
#  
#  con <- DBI::dbConnect(duckdb::duckdb(), eunomia_dir())
#  cdm <- CDMConnector::cdm_from_con(
#    con = con,
#    cdm_schema = "main",
#    write_schema = "main"
#  )
#  
#  TreatmentPatterns::executeTreatmentPatterns(
#    cohorts = cohorts,
#    cohortTableName = "CohortTable",
#    outputPath = cdmDir,
#    cdm = cdm
#  )

