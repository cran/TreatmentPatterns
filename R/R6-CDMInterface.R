#' @title CDMInterface
#'
#' @description
#' Abstract interface to the CDM, using CDMConnector or DatabaseConnector.
#'
#' @noRd
CDMInterface <- R6::R6Class(
  classname = "CDMInterface",
  public = list(
    ## Public ----
    ### Methods ----
    #' @description
    #' Initializer method
    #'
    #' @template param_connectionDetails
    #' @template param_cdmSchema
    #' @template param_resultSchema
    #' @param tempEmulationSchema Schema used to emulate temp tables.
    #' @template param_cdm
    #'
    #' @return (`invisible(self)`)
    initialize = function(connectionDetails = NULL, cdmSchema = NULL, resultSchema = NULL, tempEmulationSchema = NULL, cdm = NULL) {
      private$connectionDetails <- connectionDetails
      if (!is.null(private$connectionDetails)) {
        private$connection <- DatabaseConnector::connect(private$connectionDetails)
      }
      private$cdmSchema <- cdmSchema
      private$resultSchema <- resultSchema
      private$tempEmulationSchema <- tempEmulationSchema
      private$cdm <- cdm

      if (!is.null(cdm)) {
        private$type <- "CDMConnector"
      } else if (!is.null(connectionDetails)) {
        private$type <- "DatabaseConnector"
      } else {
        stop("Could not assert if CDMConnector or DatabaseConnector is being used.")
      }
      self$validate()
      return(invisible(self))
    },
    
    #' @description
    #' Validation method
    #'
    #' @return (`invisible(self)`)
    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      
      checkmate::assertClass(
        x = private$connectionDetails,
        "ConnectionDetails",
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        x = private$connectionDetails$dbms,
        len = 1,
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        private$cdmDatabaseSchema,
        null.ok = TRUE,
        len = 1,
        add = errorMessages
      )
      
      checkmate::assertCharacter(
        private$resultSchema,
        null.ok = TRUE,
        len = 1,
        add = errorMessages
      )
      
      checkmate::assertClass(
        private$cdm,
        classes = "cdm_reference",
        null.ok = TRUE,
        add = errorMessages
      )
      
      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    },
    
    #' @description
    #' Fetch specified cohort IDs from a specified cohort table
    #'
    #' @template param_cohorts
    #' @template param_cohortTableName
    #' @template param_andromeda
    #' @param andromedaTableName (`character(1)`)\cr
    #' Name of the table in the Andromeda object where the data will be loaded.
    #' @template param_minEraDuration
    #'
    #' @return (`data.frame`)
    fetchCohortTable = function(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration = NULL) {
      switch(private$type,
             CDMConnector = private$cdmconFetchCohortTable(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration),
             DatabaseConnector = private$dbconFetchCohortTable(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration)
      )
    },
    
    #' @description
    #' Fetch metadata from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`invisible(NULL)`)
    fetchMetadata = function(andromeda) {
      switch(private$type,
             CDMConnector = private$cdmconFetchMetadata(andromeda),
             DatabaseConnector = private$dbconFetchMetadata(andromeda)
      )
      return(invisible(self))
    },
    
    #' @description
    #' Destroys instance
    #' 
    #' @return (NULL)
    disconnect = function() {
      if (!is.null(private$connection)) {
        DatabaseConnector::disconnect(private$connection)
      }
      private$cdm <- NULL
    }
  ),
  private = list(
    ## Private ----
    ### Fields ----
    connectionDetails = NULL,
    connection = NULL,
    cdmSchema = NULL,
    resultSchema = NULL,
    tempEmulationSchema = NULL,
    cdm = NULL,
    type = "",
    
    ### Methods ----
    finalize = function() {
      self$disconnect()
    },
    
    #### DatabaseConnector ----
    # cohortIds (`integer(n)`)
    # cohortTableName (`character(1)`)
    dbconFetchCohortTable = function(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
      targetCohortId <- cohorts %>%
        dplyr::filter(.data$type == "target") %>%
        dplyr::select("cohortId") %>%
        dplyr::pull()
      
      # Select relevant data
      sql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "selectData.sql",
        packageName = "TreatmentPatterns",
        dbms = private$connection@dbms,
        tempEmulationSchema = private$tempEmulationSchema,
        resultSchema = private$resultSchema,
        cdmSchema = private$cdmSchema,
        cohortTable = cohortTableName,
        cohortIds = cohorts$cohortId,
        minEraDuration = minEraDuration,
        targetCohortId = targetCohortId
      )
      
      DatabaseConnector::querySqlToAndromeda(
        connection = private$connection,
        sql = sql,
        andromeda = andromeda,
        andromedaTableName = andromedaTableName
      )
      
      return(invisible(self))
    },
    
    dbconFetchMetadata = function(andromeda) {
      renderedSql <- SqlRender::render(
        sql = "
        SELECT
          cdm_source_name,
          cdm_source_abbreviation,
          cdm_release_date,
          vocabulary_version
        FROM @cdmSchema.cdm_source
      ;",
        cdmSchema = private$cdmSchema
      )
      
      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = private$connection@dbms
      )
      
      andromeda$metadata <- DatabaseConnector::querySql(
        connection = private$connection,
        sql = translatedSql,
        snakeCaseToCamelCase = TRUE) %>%
        dplyr::mutate(
          executionStartDate = as.character(Sys.Date()),
          packageVersion = as.character(utils::packageVersion("TreatmentPatterns")),
          rVersion = base::version$version.string,
          platform = base::version$platform
        )
      
      return(invisible(self))
    },
    
    #### CDMConnector ----
    # cohortIds (`integer(n)`)
    # cohortTableName (`character(1)`)
    # andromeda (`Andromeda::andromeda()`)
    # andromedaTableName (`character(1)`)
    cdmconFetchCohortTable = function(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
      targetCohortIds <- cohorts %>%
        dplyr::filter(.data$type == "target") %>%
        dplyr::select("cohortId") %>%
        dplyr::pull()
      
      cohortIds <- cohorts$cohortId
      
      andromeda[[andromedaTableName]] <- private$cdm[[cohortTableName]] %>%
        dplyr::filter(.data$cohort_definition_id %in% cohortIds) %>%
        dplyr::filter(.data$cohort_end_date - .data$cohort_start_date >= 0) %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::filter(any(.data$cohort_definition_id %in% targetCohortIds, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(
          private$cdm$person,
          by = dplyr::join_by(subject_id == person_id)) %>%
        dplyr::inner_join(
          private$cdm$concept,
          by = dplyr::join_by(gender_concept_id == concept_id)) %>%
        dplyr::filter(!!CDMConnector::datediff("cohort_start_date", "cohort_end_date") >= minEraDuration) %>%
        dplyr::mutate(date_of_birth = as.Date(paste0(as.integer(year_of_birth), "-01-01"))) %>%
        dplyr::mutate(age = !!CDMConnector::datediff("date_of_birth", "cohort_start_date", interval = "year")) %>%
        dplyr::rename(sex = "concept_name") %>%
        dplyr::select(
          "cohort_definition_id",
          "subject_id",
          "cohort_start_date",
          "cohort_end_date",
          "age",
          "sex")
      return(invisible(self))
    },
    
    # andromeda (`Andromeda::andromeda()`)
    cdmconFetchMetadata = function(andromeda) {
      andromeda$metadata <- private$cdm$cdm_source %>%
        dplyr::select(
          "cdm_source_name",
          "cdm_source_abbreviation",
          "cdm_release_date",
          "vocabulary_version"
        ) %>%
        dplyr::collect() %>%
        SqlRender::snakeCaseToCamelCaseNames() %>%
        dplyr::mutate(
          executionStartDate = as.character(Sys.Date()),
          packageVersion = as.character(utils::packageVersion("TreatmentPatterns")),
          rVersion = base::version$version.string,
          platform = base::version$platform
        )
      return(invisible(self))
    }
  )
)
