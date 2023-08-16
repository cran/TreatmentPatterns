#' executeTreatmentPatterns
#'
#' Compute treatment patterns according to the specified parameters within
#' specified cohorts. For more customization, or investigation of patient level
#' outcomes, you can run \link[TreatmentPatterns]{computePathways} and
#' \link[TreatmentPatterns]{export} separately.
#'
#' @template param_cohorts
#' @template param_cohortTableName
#' @template param_outputPath
#' @template param_cdm
#' @template param_connectionDetails
#' @template param_cdmSchema
#' @template param_resultSchema
#' @template param_includeTreatments
#' @template param_periodPriorToIndex
#' @template param_minEraDuration
#' @template param_splitEventCohorts
#' @template param_splitTime
#' @template param_eraCollapseSize
#' @template param_combinationWindow
#' @template param_minPostCombinationDuration
#' @template param_filterTreatments
#' @template param_maxPathLength
#' @template param_minFreq
#' @template param_addNoPaths
#'
#' @return (`invisible(NULL)`)
#' @export
#'
#' @examples
#' \donttest{
#'   # Using CDMConnector
#'   ableToRun <- all(c(
#'     require("CDMConnector", character.only = TRUE),
#'     require("DBI", character.only = TRUE),
#'     require("duckdb", character.only = TRUE),
#'     require("dplyr", character.only = TRUE)
#'   ))
#'   
#'   if (ableToRun) {
#'     withr::local_envvar(
#'       EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
#'     )
#'     
#'     CDMConnector::downloadEunomiaData()
#'   
#'     con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#'     cdm <- cdm_from_con(con, cdm_schema = "main")
#'
#'     cdm$CohortTable <- dplyr::union_all(
#'     # Viral Sinusitis
#'     cdm$condition_occurrence %>%
#'       filter(.data$condition_concept_id == 40481087) %>%
#'       inner_join(cdm$person, by = join_by(person_id == person_id)) %>%
#'       select("person_id", "condition_start_date", "condition_end_date") %>%
#'       mutate(cohort_definition_id = 1) %>%
#'       rename(
#'         cohort_start_date = "condition_start_date",
#'         cohort_end_date = "condition_end_date"
#'       ),
#'
#'     # Aspirin
#'     cdm$drug_era %>%
#'       filter(.data$drug_concept_id == 1112807) %>%
#'       inner_join(cdm$person, by = join_by(person_id == person_id)) %>%
#'       select("person_id", "drug_era_start_date", "drug_era_end_date") %>%
#'       mutate(cohort_definition_id = 2) %>%
#'       rename(
#'         cohort_start_date = "drug_era_start_date",
#'         cohort_end_date = "drug_era_end_date"
#'       ),
#'
#'     # Acetaminophen
#'     cdm$drug_era %>%
#'       filter(.data$drug_concept_id == 1125315) %>%
#'       inner_join(cdm$person, by = join_by(person_id == person_id)) %>%
#'       select("person_id", "drug_era_start_date", "drug_era_end_date") %>%
#'       mutate(cohort_definition_id = 3) %>%
#'       rename(
#'         cohort_start_date = "drug_era_start_date",
#'         cohort_end_date = "drug_era_end_date"
#'       ),
#'
#'     # Clavunate
#'     cdm$drug_era %>%
#'       filter(.data$drug_concept_id == 1759842) %>%
#'       inner_join(cdm$person, by = join_by(person_id == person_id)) %>%
#'       select("person_id", "drug_era_start_date", "drug_era_end_date") %>%
#'       mutate(cohort_definition_id = 4) %>%
#'       rename(
#'         cohort_start_date = "drug_era_start_date",
#'         cohort_end_date = "drug_era_end_date"
#'       ),
#'
#'     # Death
#'     cdm$observation %>%
#'       filter(.data$observation_concept_id == 4306655) %>%
#'       inner_join(cdm$person, by = join_by(person_id == person_id)) %>%
#'       select("person_id", "observation_date") %>%
#'       mutate(
#'         cohort_definition_id = 5,
#'         cohort_end_date = observation_date) %>%
#'       rename(
#'         cohort_start_date = "observation_date"
#'       )
#'     )
#'
#'     cdm$CohortTable <- cdm$CohortTable %>%
#'       rename(
#'         SUBJECT_ID = "person_id",
#'         COHORT_START_DATE = "cohort_start_date",
#'         COHORT_END_DATE = "cohort_end_date",
#'         COHORT_DEFINITION_ID = "cohort_definition_id"
#'       )
#'
#'     cohortTableName <- "CohortTable"
#'
#'     cohorts <- data.frame(
#'       cohortId = c(1, 2, 3, 4, 5),
#'       cohortName = c("ViralSinusitis", "Acetaminophen", "Aspirin", "Clavulanate", "Death"),
#'       type = c("target", "event", "event", "event", "exit")
#'     )
#'
#'     try(
#'       executeTreatmentPatterns(
#'         cohorts,
#'         cohortTableName,
#'         tempdir(),
#'         cdm
#'       )
#'     )
#'     
#'     DBI::dbDisconnect(con, shutdown = TRUE)
#'   }
#'
#'   # Using DatabaseConnector
#'   ableToRun <- invisible(all(
#'     require("Eunomia", character.only = TRUE),
#'     require("CirceR", character.only = TRUE),
#'     require("CohortGenerator", character.only = TRUE),
#'     require("dplyr", character.only = TRUE)
#'   ))
#'   
#'   if (ableToRun) {
#'     # CohortGenerator example
#'     connectionDetails <- Eunomia::getEunomiaConnectionDetails()
#'     cdmDatabaseSchema <- "main"
#'     resultSchema <- "main"
#'     cohortTable <- "CohortTable"
#' 
#'     cohortsToCreate <- CohortGenerator::createEmptyCohortDefinitionSet()
#'   
#'     cohortJsonFiles <- list.files(
#'       system.file(
#'         package = "TreatmentPatterns",
#'         "exampleCohorts"),
#'         full.names = TRUE)
#' 
#'     for (i in seq_len(length(cohortJsonFiles))) {
#'       cohortJsonFileName <- cohortJsonFiles[i]
#'       cohortName <- tools::file_path_sans_ext(basename(cohortJsonFileName))
#'       cohortJson <- readChar(cohortJsonFileName, file.info(
#'         cohortJsonFileName)$size)
#'
#'       cohortExpression <- CirceR::cohortExpressionFromJson(cohortJson)
#' 
#'       cohortSql <- CirceR::buildCohortQuery(
#'         cohortExpression,
#'         options = CirceR::createGenerateOptions(generateStats = FALSE))
#'     
#'       cohortsToCreate <- rbind(
#'         cohortsToCreate,
#'         data.frame(
#'           cohortId = i,
#'           cohortName = cohortName,
#'           sql = cohortSql,
#'           stringsAsFactors = FALSE))
#'     }
#'
#'     cohortTableNames <- CohortGenerator::getCohortTableNames(
#'       cohortTable = cohortTable)
#'
#'     CohortGenerator::createCohortTables(
#'       connectionDetails = connectionDetails,
#'       cohortDatabaseSchema = resultSchema,
#'       cohortTableNames = cohortTableNames)
#'
#'     # Generate the cohorts
#'     cohortsGenerated <- CohortGenerator::generateCohortSet(
#'       connectionDetails = connectionDetails,
#'       cdmDatabaseSchema = cdmDatabaseSchema,
#'       cohortDatabaseSchema = resultSchema,
#'       cohortTableNames = cohortTableNames,
#'       cohortDefinitionSet = cohortsToCreate)
#'     
#'     # Select Viral Sinusitis
#'     targetCohorts <- cohortsGenerated %>%
#'       filter(cohortName == "ViralSinusitis") %>%
#'       select(cohortId, cohortName)
#' 
#'     # Select everything BUT Viral Sinusitis cohorts
#'     eventCohorts <- cohortsGenerated %>%
#'       filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
#'       select(cohortId, cohortName)
#' 
#'     exitCohorts <- cohortsGenerated %>%
#'       filter(cohortName == "Death") %>%
#'       select(cohortId, cohortName)
#' 
#'     cohorts <- dplyr::bind_rows(
#'       targetCohorts %>% mutate(type = "target"),
#'       eventCohorts %>% mutate(type = "event"),
#'       exitCohorts %>% mutate(type = "exit")
#'     )
#'     
#'     try(
#'       executeTreatmentPatterns(
#'         cohorts = cohorts,
#'         cohortTableName = cohortTable,
#'         outputPath = tempdir(),
#'         connectionDetails = connectionDetails,
#'         cdmSchema = cdmDatabaseSchema,
#'         resultSchema = resultSchema
#'       )
#'     )
#'   }
#' }
executeTreatmentPatterns <- function(
    cohorts,
    cohortTableName,
    outputPath,
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = "",
    splitTime = 30,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5,
    minFreq = 5,
    addNoPaths = TRUE) {
  checkmate::assert_character(outputPath, len = 1, null.ok = FALSE)
  checkmate::assert_integerish(minFreq, len = 1, null.ok = FALSE, lower = 0)

  # Compute pathways on patient level
  andromeda <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    cdm = cdm,
    connectionDetails = connectionDetails,
    cdmSchema = cdmSchema,
    resultSchema = resultSchema,
    includeTreatments = includeTreatments,
    periodPriorToIndex = periodPriorToIndex,
    minEraDuration = minEraDuration,
    splitEventCohorts = splitEventCohorts,
    splitTime = splitTime,
    eraCollapseSize = eraCollapseSize,
    combinationWindow = combinationWindow,
    minPostCombinationDuration = minPostCombinationDuration,
    filterTreatments = filterTreatments,
    maxPathLength = maxPathLength,
    addNoPaths = addNoPaths
  )

  # Export csv-files
  TreatmentPatterns::export(andromeda, outputPath = outputPath, minFreq = minFreq)

  Andromeda::close(andromeda)
  return(invisible(NULL))
}
