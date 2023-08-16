## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----dataSettingsVars, eval=FALSE---------------------------------------------
#  connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#  cdmDatabaseSchema <- "main"
#  resultSchema <- "main"
#  cohortTable <- "cohortTable"

## ----generatingCohorts, eval=FALSE--------------------------------------------
#  cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
#  
#  # Get json-files included with TreatmentPatterns
#  cohortJsonFiles <- list.files(
#    system.file(
#      package = "TreatmentPatterns",
#      "exampleCohorts"
#    ),
#    full.names = TRUE
#  )
#  
#  # add cohort definition per file
#  for (i in seq_len(length(cohortJsonFiles))) {
#    cohortJsonFileName <- cohortJsonFiles[i]
#    cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
#    # Here we read in the JSON in order to create the SQL
#    # using [CirceR](https://ohdsi.github.io/CirceR/)
#    # If you have your JSON and SQL stored differenly, you can
#    # modify this to read your JSON/SQL files however you require
#    cohortJson <- readChar(cohortJsonFileName, file.info(
#      cohortJsonFileName
#    )$size)
#  
#    cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
#  
#    cohortSql <- CirceR::buildCohortQuery(
#      cohortExpression,
#      options = CirceR::createGenerateOptions(generateStats = FALSE)
#    )
#    cohortsToCreate <- rbind(
#      cohortsToCreate,
#      data.frame(
#        cohortId = i,
#        cohortName = cohortName,
#        sql = cohortSql,
#        stringsAsFactors = FALSE
#      )
#    )
#  }

## ----cohortTableNames, eval=FALSE---------------------------------------------
#  # Create the cohort tables to hold the cohort generation results
#  cohortTableNames <- CohortGenerator::getCohortTableNames(
#    cohortTable = cohortTable
#  )

## ---- generateCohorts, eval=FALSE---------------------------------------------
#  CohortGenerator::createCohortTables(
#    connectionDetails = connectionDetails,
#    cohortDatabaseSchema = resultSchema,
#    cohortTableNames = cohortTableNames
#  )

## ----getCohortsGenerated, eval=FALSE------------------------------------------
#  # Generate the cohorts
#  cohortsGenerated <- CohortGenerator::generateCohortSet(
#    connectionDetails = connectionDetails,
#    cdmDatabaseSchema = cdmDatabaseSchema,
#    cohortDatabaseSchema = resultSchema,
#    cohortTableNames = cohortTableNames,
#    cohortDefinitionSet = cohortsToCreate
#  )

