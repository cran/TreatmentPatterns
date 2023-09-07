#' @title PathwayConstructor
#'
#' @description
#' PathwayConstructor R6 object.
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
      
      checkmate::assertCharacter(
        x = private$settings$splitEventCohorts,
        len = 1,
        add = errorMessages
      )
      
      checkmate::assertNumeric(
        x = private$settings$splitTime,
        lower = 0,
        finite = TRUE,
        len = 1,
        null.ok = FALSE,
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
      
      checkmate::assertLogical(
        x = private$settings$addNoPaths,
        any.missing = FALSE,
        len = 1,
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

      private$andromeda$fullCohorts <- private$cdmInterface$fetchCohortTable(
        cohortIds = private$cohorts$cohortId,
        cohortTableName = private$cohortTableName
      )

      private$andromeda$fullCohorts <- private$andromeda$fullCohorts %>%
        dplyr::rename_with(toupper) %>%
        dplyr::rename(
          cohort_id = "COHORT_DEFINITION_ID",
          person_id = "SUBJECT_ID",
          start_date = "COHORT_START_DATE",
          end_date = "COHORT_END_DATE"
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
    #' @template param_addNoPaths
    #'
    #' @return (`data.frame()`)
    editSettings = function(
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
      splitEventCohorts = "",
      splitTime = 30,
      eraCollapseSize = 30,
      combinationWindow = 30,
      minPostCombinationDuration = 30,
      filterTreatments = "First",
      maxPathLength = 5,
      addNoPaths = TRUE
    )
  )
)
