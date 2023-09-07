#' computePathways
#'
#' Compute treatment patterns according to the specified parameters within
#' specified cohorts.
#'
#' @template param_cohorts
#' @template param_cohortTableName
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
#' @template param_addNoPaths
#'
#' @return (`Andromeda::andromeda()`)
#' \link[Andromeda]{andromeda} object containing non-sharable patient level
#' data outcomes.
#'
#' @export
#'
#' @examples
#' \donttest{
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
#'     computePathways(
#'       cohorts = cohorts,
#'       cohortTableName = cohortTable,
#'       connectionDetails = connectionDetails,
#'       cdmSchema = cdmDatabaseSchema,
#'       resultSchema = resultSchema
#'     )
#'   }
#' }
computePathways <- function(
    cohorts,
    cohortTableName,
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
    addNoPaths = TRUE) {
  cdmInterface <- CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = cdmSchema,
    resultSchema = resultSchema,
    cdm = cdm
  )
  on.exit(cdmInterface$destroy())

  pathwayConstructor <- PathwayConstructor$new(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    cdmInterface = cdmInterface
  )

  pathwayConstructor$editSettings(
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

  pathwayConstructor$construct()
  andromeda <- pathwayConstructor$getAndromeda()
  cdmInterface$addSex(andromeda)
  cdmInterface$addAge(andromeda)

  andromeda$metadata <- andromeda$metadata %>%
    dplyr::collect() %>%
    dplyr::mutate(execution_end_date = as.character(Sys.Date()))
  
  
  return(andromeda)
}
