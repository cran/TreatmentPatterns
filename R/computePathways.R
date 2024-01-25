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
#' @param tempEmulationSchema Schema used to emulate temp tables
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
#'
#' @return (`Andromeda::andromeda()`)
#' \link[Andromeda]{andromeda} object containing non-sharable patient level
#' data outcomes.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(TreatmentPatterns)
#' library(CDMConnector)
#' library(dplyr)
#' 
#' if (require("CirceR", character.only = TRUE, quietly = TRUE)) {
#'   withr::local_envvar(
#'     EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
#'   )
#'
#'   downloadEunomiaData(overwrite = TRUE)
#'
#'   con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
#'   cdm <- cdmFromCon(con, cdmSchema = "main", writeSchema = "main")
#'
#'   cohortSet <- readCohortSet(
#'     path = system.file(package = "TreatmentPatterns", "exampleCohorts")
#'   )
#'
#'   cdm <- generateCohortSet(
#'     cdm = cdm,
#'     cohortSet = cohortSet,
#'     name = "cohort_table"
#'   )
#'
#'   cohorts <- cohortSet %>%
#'     # Remove 'cohort' and 'json' columns
#'     select(-"cohort", -"json") %>%
#'     mutate(type = c("event", "event", "event", "event", "exit", "event", "event", "target")) %>%
#'     rename(
#'       cohortId = "cohort_definition_id",
#'       cohortName = "cohort_name",
#'     ) %>%
#'     select("cohortId", "cohortName", "type")
#'
#'   outputEnv <- computePathways(
#'     cohorts = cohorts,
#'     cohortTableName = "cohort_table",
#'     cdm = cdm
#'   )
#'
#'   Andromeda::close(outputEnv)
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
computePathways <- function(
    cohorts,
    cohortTableName,
    cdm = NULL,
    connectionDetails = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    tempEmulationSchema = NULL,
    includeTreatments = "startDate",
    periodPriorToIndex = 0,
    minEraDuration = 0,
    splitEventCohorts = NULL,
    splitTime = NULL,
    eraCollapseSize = 30,
    combinationWindow = 30,
    minPostCombinationDuration = 30,
    filterTreatments = "First",
    maxPathLength = 5) {
  
  cdmInterface <- CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = cdmSchema,
    resultSchema = resultSchema,
    tempEmulationSchema = tempEmulationSchema,
    cdm = cdm
  )
  
  withr::defer({
    cdmInterface$disconnect()
  })
  
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
    maxPathLength = maxPathLength
  )
  pathwayConstructor$construct()
  andromeda <- pathwayConstructor$getAndromeda()

  andromeda$metadata <- andromeda$metadata %>%
    dplyr::collect() %>%
    dplyr::mutate(execution_end_date = as.character(Sys.Date()))
  return(andromeda)
}
