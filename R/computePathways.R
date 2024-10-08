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
#' ableToRun <- all(
#'   require("CirceR", character.only = TRUE, quietly = TRUE),
#'   require("CDMConnector", character.only = TRUE, quietly = TRUE),
#'   require("TreatmentPatterns", character.only = TRUE, quietly = TRUE),
#'   require("dplyr", character.only = TRUE, quietly = TRUE)
#' )
#' 
#' if (ableToRun) {
#'   library(TreatmentPatterns)
#'   library(CDMConnector)
#'   library(dplyr)
#'
#'   withr::local_envvar(
#'     R_USER_CACHE_DIR = tempfile(),
#'     EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
#'   )
#'
#'   tryCatch({
#'     if (Sys.getenv("skip_eunomia_download_test") != "TRUE") {
#'       CDMConnector::downloadEunomiaData(overwrite = TRUE)
#'     }
#'   }, error = function(e) NA)
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
  validateComputePathways()
  
  args <- eval(
    expr = expression(mget(names(formals()))),
    envir = sys.frame(sys.nframe())
  )
  
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

  andromeda <- Andromeda::andromeda()
  andromeda <- cdmInterface$fetchMetadata(andromeda)
  andromeda <- cdmInterface$fetchCohortTable(
    cohorts = cohorts,
    cohortTableName = cohortTableName,
    andromeda = andromeda,
    andromedaTableName = "cohortTable",
    minEraDuration = minEraDuration
  )
  
  checkCohortTable(andromeda)
  
  andromeda$cohortTable <- andromeda$cohortTable %>%
    dplyr::rename(
      cohortId = "cohort_definition_id",
      personId = "subject_id",
      startDate = "cohort_start_date",
      endDate = "cohort_end_date"
    )
  
  andromeda <- constructPathways(
    settings = args,
    andromeda = andromeda
  )

  andromeda$metadata <- andromeda$metadata %>%
    dplyr::collect() %>%
    dplyr::mutate(execution_end_date = as.character(Sys.Date()))
  return(andromeda)
}


validateComputePathways <- function() {
  args <- eval(
    # Expression to get names of function arguments in current function
    expr = expression(mget(names(formals()))),
    # Run expression in function that calls `validateComputePathways`
    envir = sys.frame(sys.nframe() - 1)
  )
  
  if (args$minEraDuration > args$minPostCombinationDuration) {
    warning("The `minPostCombinationDuration` is set lower than the `minEraDuration`, this might result in unexpected behavior")
  }
  
  if (args$minEraDuration > args$combinationWindow) {
    warning("The `combinationWindow` is set lower than the `minEraDuration`, this might result in unexpected behavior")
  }
  
  assertCol <- checkmate::makeAssertCollection()
  
  checkmate::assertCharacter(
    args$includeTreatments,
    len = 1,
    add = assertCol,
    .var.name = "includeTreatments"
  )
  
  checkmate::assertSubset(
    args$includeTreatments,
    choices = c("startDate", "endDate"),
    add = assertCol,
    .var.name = "includeTreatments"
  )
  
  checkmate::assertNumeric(
    args$periodPriorToIndex,
    len = 1,
    finite = TRUE,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "periodPriorToIndex"
  )
  
  checkmate::assertNumeric(
    x = args$minEraDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "minEraDuration"
  )
  
  checkmate::assertIntegerish(
    x = args$splitEventCohorts,
    null.ok = TRUE,
    add = assertCol,
    .var.name = "splitEventCohorts"
  )
  
  checkmate::assertIntegerish(
    x = args$splitTime,
    lower = 0,
    null.ok = TRUE,
    add = assertCol,
    .var.name = "splitTime"
  )
  
  checkmate::assertNumeric(
    x = args$eraCollapseSize,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "eraCollapseSize"
  )
  
  checkmate::assertNumeric(
    x = args$combinationWindow,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "combinationWindow"
  )
  
  checkmate::assertNumeric(
    x = args$minPostCombinationDuration,
    lower = 0,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "minPostCombinationDuration"
  )
  
  checkmate::assertCharacter(
    x = args$filterTreatments,
    len = 1,
    add = assertCol,
    .var.name = "filterTreatments"
  )
  
  checkmate::assertSubset(
    x = args$filterTreatments,
    choices = c("First", "Changes", "All"),
    add = assertCol,
    .var.name = "filterTreatments"
  )
  
  checkmate::assertNumeric(
    x = args$maxPathLength,
    lower = 0,
    upper = 5,
    finite = TRUE,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "maxPathLength"
  )
  
  checkmate::assertDataFrame(
    x = args$cohorts,
    types = c("integerish", "character", "character"),
    any.missing = FALSE,
    all.missing = FALSE,
    ncols = 3,
    min.rows = 1,
    col.names = "named",
    add = assertCol,
    .var.name = "cohorts"
  )
  
  checkmate::assertSubset(
    x = names(args$cohorts),
    choices = c("cohortId", "cohortName", "type"),
    add = assertCol,
    .var.name = "cohorts"
  )
  
  checkmate::assertSubset(
    x = args$cohorts$type,
    choices = c("event", "target", "exit"),
    add = assertCol,
    .var.name = "cohorts"
  )
  
  checkmate::assertCharacter(
    x = args$cohortTableName,
    len = 1,
    null.ok = FALSE,
    .var.name = "cohortTableName"
  )
  
  checkmate::assertClass(
    x = args$connectionDetails,
    classes = "ConnectionDetails",
    null.ok = TRUE,
    add = assertCol,
    .var.name = "connectionDetails"
  )
  
  checkmate::assertCharacter(
    x = args$connectionDetails$dbms,
    len = 1,
    null.ok = TRUE,
    add = assertCol,
    .var.name = "connectionDetails"
  )
  
  checkmate::assertCharacter(
    args$cdmDatabaseSchema,
    null.ok = TRUE,
    len = 1,
    add = assertCol,
    .var.name = "cdmDatabaseSchema"
  )
  
  checkmate::assertCharacter(
    args$resultSchema,
    null.ok = TRUE,
    len = 1,
    add = assertCol,
    .var.name = "resultSchema"
  )
  
  checkmate::assertClass(
    args$cdm,
    classes = "cdm_reference",
    null.ok = TRUE,
    add = assertCol,
    .var.name = "cdm"
  )
  
  checkmate::reportAssertions(collection = assertCol)
}

checkCohortTable = function(andromeda) {
  cohortTableHead <- andromeda[["cohortTable"]] %>%
    head() %>%
    dplyr::collect()
  
  assertions <- checkmate::makeAssertCollection()
  checkmate::assertIntegerish(cohortTableHead$cohort_definition_id, add = assertions)
  checkmate::assertIntegerish(cohortTableHead$subject_id, add = assertions)
  checkmate::assertDate(cohortTableHead$cohort_start_date, add = assertions)
  checkmate::assertDate(cohortTableHead$cohort_end_date, add = assertions)
  checkmate::reportAssertions(assertions)
}
