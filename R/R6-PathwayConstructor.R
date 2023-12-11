#' @title PathwayConstructor
#'
#' @description
#' PathwayConstructor R6 object.
#'
#' @noRd
PathwayConstructor <- R6::R6Class(
  classname = "PathwayConstructor",
  public = list(
    # Public ----

    #' @description
    #' Initialize method called by `PathwayConstructor$new()`.
    #' \cr\cr
    #' Choose the way you interface with the CDM, either through `DatabaseConnector` or `CDMConnector`.
    #'
    #' @template param_cohorts
    #' @template param_cohortTableName
    #' @template param_cdmInterface
    #'
    #' @return (`invisible(self)`)
    initialize = function(cohorts, cohortTableName, cdmInterface) {
      private$cohorts <- cohorts
      private$cohortTableName <- cohortTableName
      private$cdmInterface <- cdmInterface
      
      self$validate()

      private$settings$targetCohortIds <- cohorts %>%
        filter(.data$type == "target") %>%
        select("cohortId") %>%
        pull() %>%
        paste(collapse = ",")
      private$settings$eventCohortIds <- cohorts %>%
        filter(.data$type == "event") %>%
        select("cohortId") %>%
        pull() %>%
        paste(collapse = ",")
      private$settings$exitCohortIds <- cohorts %>%
        filter(.data$type == "exit") %>%
        select("cohortId") %>%
        pull() %>%
        paste(collapse = ",")
      
      return(invisible(self))
    },
    
    #' @description
    #' Validation method
    #'
    #' @return (`invisible(self)`)
    validate = function() {
      private$cdmInterface$validate()
      
      errorMessages <- checkmate::makeAssertCollection()
      
      checkmate::assertCharacter(
        x = private$settings$includeTreatments,
        len = 1,
        add = errorMessages
      )
      
      checkmate::assertSubset(
        x = private$settings$includeTreatments,
        choices = c("startDate", "endDate"),
        add = errorMessages
      )
      
      checkmate::assertNumeric(
        x = private$settings$periodPriorToIndex,
        len = 1,
        finite = TRUE,
        null.ok = FALSE,
        add = errorMessages
      )
      
      checkmate::assertNumeric(
        x = private$settings$minEraDuration,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE,
        add = errorMessages
      )
      
      checkmate::assertIntegerish(
        x = private$settings$splitEventCohorts,
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::assertIntegerish(
        x = private$settings$splitTime,
        lower = 0,
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::assertNumeric(
        x = private$settings$eraCollapseSize,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE,
        add = errorMessages
      )
      
      checkmate::assertNumeric(
        x = private$settings$combinationWindow,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE,
        add = errorMessages
      )
      
      checkmate::assertNumeric(
        x = private$settings$minPostCombinationDuration,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        x = private$settings$filterTreatments,
        len = 1,
        add = errorMessages
      )
      
      checkmate::assertSubset(
        x = private$settings$filterTreatments,
        choices = c("First", "Changes", "All"),
        add = errorMessages
      )
      
      checkmate::assertNumeric(
        x = private$settings$maxPathLength,
        lower = 0,
        upper = 5,
        finite = TRUE,
        len = 1,
        null.ok = FALSE,
        add = errorMessages
      )
      
      checkmate::assertDataFrame(
        x = private$cohorts,
        types = c("integerish", "character", "character"),
        any.missing = FALSE,
        all.missing = FALSE,
        ncols = 3,
        min.rows = 1,
        col.names = "named",
        add = errorMessages
      )
      
      checkmate::assertSubset(
        x = names(private$cohorts),
        choices = c("cohortId", "cohortName", "type"),
        add = errorMessages
      )
      
      checkmate::assertSubset(
        x = private$cohorts$type,
        choices = c("event", "target", "exit"),
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        x = private$cohortTableName,
        len = 1, null.ok = FALSE
      )
      
      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    },
    
    #' @description
    #' Construct the pathways. Generates `Andromeda::andromeda()` objects,
    #' which can be fetched using `self$getAndromeda()`.
    construct = function() {
      # Set up Andromeda sqlite environment
      private$andromeda <- Andromeda::andromeda()
      private$cdmInterface$fetchMetadata(private$andromeda)
      
      private$cdmInterface$fetchCohortTable(
        cohorts = private$cohorts,
        cohortTableName = private$cohortTableName,
        andromeda = private$andromeda,
        andromedaTableName = "cohortTable",
        minEraDuration = private$settings$minEraDuration
      )
      
      private$andromeda$cohortTable <- private$andromeda$cohortTable %>%
        dplyr::rename(
          cohortId = "cohort_definition_id",
          personId = "subject_id",
          startDate = "cohort_start_date",
          endDate = "cohort_end_date"
        )
      
      private$andromeda <- constructPathways(
        settings = private$settings,
        cohorts = private$cohorts,
        andromeda = private$andromeda
      )
      return(invisible(self))
    },
    
    #' @description
    #' Gets the `Andromeda::andromeda()` objects in a list.
    #'
    #' @return (`list()`)
    getAndromeda = function() {
      return(private$andromeda)
    },
    
    
    #' @description
    #' Edit settings
    #'
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
    #' @return (`data.frame()`)
    editSettings = function(
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
      settings <- mget(
        x = names(formals()),
        envir = sys.frame(
          which = sys.nframe()
        )
      )
      private$settings <- utils::modifyList(private$settings, settings)
      self$validate()
    },
    
    #' @description
    #' Getter method to get the specified settings
    #'
    #' @return (`data.frame()`)
    getSettings = function() {
      return(private$settings)
    }
  ),
  private = list(
    # Private ----
    ## Fields ----
    cohorts = NULL,
    cohortTableName = NULL,
    cdmInterface = NULL,
    andromeda = NULL,
    settings = list(
      targetCohortIds = NULL,
      eventCohortIds = NULL,
      exitCohortIds = NULL,
      includeTreatments = "startDate",
      periodPriorToIndex = 0,
      minEraDuration = 0,
      splitEventCohorts = NULL,
      splitTime = NULL,
      eraCollapseSize = 30,
      combinationWindow = 30,
      minPostCombinationDuration = 30,
      filterTreatments = "First",
      maxPathLength = 5
    )
  )
)
