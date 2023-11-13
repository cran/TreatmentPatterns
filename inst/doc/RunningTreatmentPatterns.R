## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----prep, echo=FALSE, results='hide', message=FALSE, warning=FALSE, error=FALSE, eval=FALSE----
#  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#  cdmDatabaseSchema <- "main"
#  resultSchema <- "main"
#  cohortTable <- "CohortTable"
#  
#  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
#  
#  cohortJsonFiles <- list.files(
#    system.file(
#      package = "TreatmentPatterns",
#      "exampleCohorts"),
#    full.names = TRUE)
#  
#  for (i in seq_len(length(cohortJsonFiles))) {
#    cohortJsonFileName <- cohortJsonFiles[i]
#    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
#    cohortJson <- readChar(cohortJsonFileName, file.info(
#      cohortJsonFileName)$size)
#  
#    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
#  
#    cohortSql <- CirceR::buildCohortQuery(
#      cohortExpression,
#      options = CirceR::createGenerateOptions(generateStats = FALSE))
#    cohortsToCreate <- rbind(
#      cohortsToCreate,
#      data.frame(
#        cohortId = i,
#        cohortName = cohortName,
#        sql = cohortSql,
#        stringsAsFactors = FALSE))
#  }
#  
#  cohortTableNames <- CohortGenerator::getCohortTableNames(
#    cohortTable = cohortTable)
#  
#  CohortGenerator::createCohortTables(
#    connectionDetails = connectionDetails,
#    cohortDatabaseSchema = resultSchema,
#    cohortTableNames = cohortTableNames)
#  
#  # Generate the cohorts
#  cohortsGenerated <- CohortGenerator::generateCohortSet(
#    connectionDetails = connectionDetails,
#    cdmDatabaseSchema = cdmDatabaseSchema,
#    cohortDatabaseSchema = resultSchema,
#    cohortTableNames = cohortTableNames,
#    cohortDefinitionSet = cohortsToCreate)
#  

## ----library------------------------------------------------------------------
library(dplyr)

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

## ----executeTreatmentPatterns, eval=FALSE-------------------------------------
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
#  
#  export(andromeda = andromeda, outputPath = file.path(tempDir, "segmented"))

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

## ---- eval=FALSE--------------------------------------------------------------
#  treatmentPathways <- read.csv(file.path(allDir, "treatmentPathways.csv"))
#  
#  data <- treatmentPathways %>%
#    filter(sex == "all") %>%
#    filter(age == "all") %>%
#    filter(indexYear == "all") %>%
#    filter(path != "None")

## ----sunburst, eval=FALSE-----------------------------------------------------
#  TreatmentPatterns::createSunburstPlot2(data)

## ----sankey, eval=FALSE-------------------------------------------------------
#  TreatmentPatterns::createSankeyDiagram2(data)

