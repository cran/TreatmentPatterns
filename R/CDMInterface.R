# Copyright 2024 DARWIN EU®
#
# This file is part of TreatmentPatterns
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
      private$.connectionDetails <- connectionDetails
      if (!is.null(private$.connectionDetails)) {
        private$.connection <- DatabaseConnector::connect(private$.connectionDetails)
      }
      private$.cdmSchema <- cdmSchema
      private$.resultSchema <- resultSchema
      private$.tempEmulationSchema <- tempEmulationSchema
      private$.cdm <- cdm
      
      if (!is.null(cdm)) {
        private$.type <- "CDMConnector"
      } else if (!is.null(connectionDetails)) {
        private$.type <- "DatabaseConnector"
      } else {
        stop("Could not assert if CDMConnector or DatabaseConnector is being used.")
      }
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
    #' @return (`andromeda`)
    fetchCohortTable = function(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration = NULL) {
      switch(
        private$.type,
        CDMConnector = private$cdmconFetchCohortTable(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration),
        DatabaseConnector = private$dbconFetchCohortTable(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration)
      )
    },

    #' @description
    #' Fetch cdm_source from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`andromeda`)
    fetchCdmSource = function(andromeda) {
      switch(
        private$.type,
        CDMConnector = private$cdmconFetchCdmSource(andromeda),
        DatabaseConnector = private$dbconFetchCdmSource(andromeda)
      )
    },

    #' @description
    #' Fetch meta data from CDM
    #'
    #' @template param_andromeda
    #'
    #' @return (`andromeda`)
    fetchMetadata = function(andromeda) {
      andromeda$metadata <- data.frame(
        execution_start = as.numeric(Sys.time()),
        package_version = as.character(utils::packageVersion("TreatmentPatterns")),
        r_version = base::version$version.string,
        platform = base::version$platform
      )
      return(andromeda)
    },

    #' @description
    #' Destroys instance
    #' 
    #' @return (NULL)
    disconnect = function() {
      if (!is.null(private$.connection)) {
        DatabaseConnector::disconnect(private$.connection)
      }
      private$.cdm <- NULL
    },
    
    checkCohortTable = function() {
      switch(
        private$.type,
        CDMConnector = private$cdmconCheckCohortTable(andromeda),
        DatabaseConnector = private$dbconCheckCohortTable(andromeda)
      )
    }
  ),
  private = list(
    ## Private ----
    ### Fields ----
    .connectionDetails = NULL,
    .connection = NULL,
    .cdmSchema = NULL,
    .resultSchema = NULL,
    .tempEmulationSchema = NULL,
    .cdm = NULL,
    .type = "",
    
    ### Methods ----
    finalize = function() {
      self$disconnect()
    },
    
    dbAppendAttrition = function(n, andromeda, cohortIds) {
      appendAttrition(
        toAdd = data.frame(
          number_records = sum(n),
          number_subjects = length(n),
          reason_id = 1,
          reason = sprintf("Qualifying records for cohort definitions: %s", paste(cohortIds, collapse = ", ")),
          time_stamp = as.numeric(Sys.time())
        ),
        andromeda = andromeda
      )
    },

    #### DatabaseConnector ----
    # cohortIds (`integer(n)`)
    # cohortTableName (`character(1)`)
    dbconFetchCohortTable = function(cohorts, cohortTableName, andromeda, andromedaTableName, minEraDuration) {
      targetCohortId <- getCohortIds(cohorts, "target")

      n <- lapply(cohortTableName, function(cohortTable) {
        DatabaseConnector::renderTranslateQuerySql(
          connection = private$.connection,
          sql = "
        SELECT COUNT(*)
        FROM @resultSchema.@cohortTable
        WHERE cohort_definition_id IN (@cohortIds)
        GROUP BY subject_id;",
          resultSchema = private$.resultSchema,
          cohortTable = cohortTable,
          cohortIds = cohorts$cohortId
        ) |>
          unlist() |>
          as.numeric()
      }) |>
        unlist()

      private$dbAppendAttrition(n, andromeda, sort(cohorts$cohortId))

      # Select relevant data
      sql <- lapply(cohortTableName, function(tableName) {
        SqlRender::loadRenderTranslateSql(
          sqlFilename = "selectData.sql",
          packageName = "TreatmentPatterns",
          dbms = private$.connection@dbms,
          tempEmulationSchema = private$.tempEmulationSchema,
          resultSchema = private$.resultSchema,
          cdmSchema = private$.cdmSchema,
          cohortTable = tableName,
          cohortIds = cohorts$cohortId,
          minEraDuration = minEraDuration
        )
      })

      renderedSql <- paste(sql, collapse = "\nUNION ALL\n")

      DatabaseConnector::renderTranslateExecuteSql(
        connection = private$.connection,
        oracleTempSchema = private$.tempEmulationSchema,
        sql = sprintf(
          "DROP TABLE IF EXISTS #tp_dbc_cohort_table;
          
          SELECT *
          INTO #tp_dbc_cohort_table
          FROM (
            %s
          ) a;",
          renderedSql
        ),
        tempEmulationSchema = private$.tempEmulationSchema
      )

      DatabaseConnector::renderTranslateQuerySqlToAndromeda(
        connection = private$.connection,
        andromeda = andromeda,
        andromedaTableName = andromedaTableName,
        tempEmulationSchema = private$.tempEmulationSchema,
        sql = "
        SELECT 
          #tp_dbc_cohort_table.cohort_definition_id AS cohort_definition_id,
          #tp_dbc_cohort_table.subject_id AS subject_id,
          #tp_dbc_cohort_table.cohort_start_date,
          #tp_dbc_cohort_table.cohort_end_date,
          #tp_dbc_cohort_table.age,
          #tp_dbc_cohort_table.sex,
          #tp_dbc_cohort_table.subject_id_origin
        FROM #tp_dbc_cohort_table
        INNER JOIN (
          SELECT #tp_dbc_cohort_table.subject_id
          FROM #tp_dbc_cohort_table
          WHERE #tp_dbc_cohort_table.cohort_definition_id IN (@targetCohortId)
        ) AS cross_sec
        ON cross_sec.subject_id = #tp_dbc_cohort_table.subject_id",
        targetCohortId = targetCohortId
      )

      names(andromeda[[andromedaTableName]]) <- tolower(names(andromeda[[andromedaTableName]]))

      if (utils::packageVersion("Andromeda") >= package_version("1.0.0")) {
        andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
          dplyr::mutate(
            cohort_start_date = dplyr::sql("datediff('day', DATE '1970-01-01', cohort_start_date)"),
            cohort_end_date = dplyr::sql("datediff('day', DATE '1970-01-01', cohort_end_date)")
          )
      } else {
        andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
          dplyr::mutate(
            cohort_start_date = as.integer(.data$cohort_start_date),
            cohort_end_date = as.integer(.data$cohort_end_date)
          )
      }

      n <- andromeda[[andromedaTableName]] %>%
        dplyr::group_by(.data$subject_id) %>% 
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull()

      appendAttrition(
        toAdd = data.frame(
          number_records = sum(n),
          number_subjects = length(n),
          reason_id = 2,
          reason = sprintf("Removing records < minEraDuration (%s)", minEraDuration),
          time_stamp = as.numeric(Sys.time())
        ),
        andromeda = andromeda
      )
      return(andromeda)
    },

    dbconFetchCdmSource = function(andromeda) {
      renderedSql <- SqlRender::render(
        sql = "SELECT * FROM @cdmSchema.cdm_source;",
        cdmSchema = private$.cdmSchema
      )

      translatedSql <- SqlRender::translate(
        sql = renderedSql,
        targetDialect = private$.connection@dbms
      )

      DatabaseConnector::querySqlToAndromeda(
        connection = private$.connection,
        sql = translatedSql,
        andromeda = andromeda,
        andromedaTableName = "cdm_source_info"
      )
      names(andromeda$cdm_source_info) <- tolower(names(andromeda$cdm_source_info))
      return(andromeda)
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

      n <- sapply(cohortTableName, function(tableName) {
        private$.cdm[[tableName]] %>%
          dplyr::group_by(.data$subject_id) %>% 
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::pull()
      }) |> unlist()

      if (length(n) == 0) {
        n <- 0
      }

      private$dbAppendAttrition(n, andromeda, sort(cohorts$cohortId))

      cohortIds <- cohorts$cohortId

      for (tableName in cohortTableName) {
        tbl <- private$.cdm[[tableName]] %>%
          dplyr::group_by(.data$subject_id) %>%
          dplyr::mutate(
            subject_id_origin = .data$subject_id
          ) %>%
          dplyr::ungroup() %>%
          mutate(r = dplyr::row_number()) %>%
          dplyr::group_by(.data$subject_id_origin) %>%
          dplyr::mutate(
            subject_id = min(.data$r, na.rm = TRUE)
          ) %>%
          dplyr::select(-"r") %>%
          dplyr::ungroup() %>%
          dplyr::filter(.data$cohort_definition_id %in% cohortIds) %>%
          dplyr::filter(!!CDMConnector::datediff("cohort_start_date", "cohort_end_date", interval = "day") >= minEraDuration) %>%
          dplyr::group_by(.data$subject_id) %>%
          dplyr::ungroup() %>%
          dplyr::inner_join(
            private$.cdm$person,
            by = dplyr::join_by(subject_id_origin == person_id)
          ) %>%
          dplyr::inner_join(
            private$.cdm$concept,
            by = dplyr::join_by(gender_concept_id == concept_id)) %>%
          dplyr::mutate(
            date_of_birth = as.Date(paste0(as.character(year_of_birth), "-01-01"))) %>%
          dplyr::mutate(
            age = !!CDMConnector::datediff("date_of_birth", "cohort_start_date", interval = "year")) %>%
          dplyr::mutate(
            subject_id_origin = as.character(subject_id_origin)
          ) %>%
          dplyr::rename(sex = "concept_name") %>%
          dplyr::mutate(
            temp_date = as.Date("1970-01-01")
          ) %>%
          dplyr::mutate(
            cohort_start_date = !!CDMConnector::datediff(start = "temp_date", end = "cohort_start_date", interval = "day"),
            cohort_end_date = !!CDMConnector::datediff(start = "temp_date", end = "cohort_end_date", interval = "day")
          ) %>%
          dplyr::select(
            "cohort_definition_id",
            "subject_id",
            "subject_id_origin",
            "cohort_start_date",
            "cohort_end_date",
            "age",
            "sex"
          )

        if (is.null(andromeda[[andromedaTableName]])) {
          dplyr::copy_to(dest = andromeda, df = tbl, name = andromedaTableName, overwrite = TRUE)
          # andromeda[[andromedaTableName]] <- tbl
        } else {
          andromeda$tbl_temp <- tbl
          andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
            dplyr::union_all(andromeda$tbl_temp)
          andromeda$tbl_temp <- NULL
        }
      }
      
      targetId <- as.numeric(targetCohortIds)

      andromeda[[andromedaTableName]] <- andromeda[[andromedaTableName]] %>%
        dplyr::mutate(cohort_definition_id = as.numeric(.data$cohort_definition_id)) %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::filter(any(.data$cohort_definition_id %in% targetId, na.rm = TRUE)) %>%
        dplyr::ungroup()

      n <- andromeda[[andromedaTableName]] %>%
        dplyr::group_by(.data$subject_id) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull()

      appendAttrition(
        toAdd = data.frame(
          number_records = sum(n),
          number_subjects = length(n),
          reason_id = 2,
          reason = sprintf("Removing records < minEraDuration (%s)", minEraDuration),
          time_stamp = as.numeric(Sys.time())
        ),
        andromeda = andromeda
      )
      return(andromeda)
    },
    
    # andromeda (`Andromeda::andromeda()`)
    cdmconFetchCdmSource = function(andromeda) {
      cdmSource <- private$.cdm$cdm_source %>%
        dplyr::collect()
      andromeda$cdm_source_info <- cdmSource
      return(andromeda)
    }
  ),
  
  # Active ----
  active = list(
    connectionDetails = function() return(private$.connectionDetails),
    connection = function() return(private$.connection),
    cdmSchema = function() return(private$.cdmSchema),
    resultSchema = function() return(private$.resultSchema),
    tempEmulationSchema = function() return(private$.tempEmulationSchema),
    cdm = function() return(private$.cdm),
    type = function() return(private$.type)
  )
)
