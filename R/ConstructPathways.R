#' constructPathway
#'
#' Constructs the pathways.
#'
#' @param settings (`data.frame`)
#' @param cohorts (`data.frame`)
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return `invisible(NULL)`
constructPathways <- function(
    settings,
    cohorts,
    andromeda) {
  targetCohortIds <- cohorts %>%
    filter(.data$type == "target") %>%
    select("cohortId") %>%
    pull()
  eventCohortIds <- cohorts %>%
    filter(.data$type == "event") %>%
    select("cohortId") %>%
    pull()
  exitCohortIds <- cohorts %>%
    filter(.data$type == "exit") %>%
    select("cohortId") %>%
    pull()

  andromeda$cohorts <- cohorts

  message(sprintf("Constructing treatment pathways: %s", settings$studyName))

  selectPeople <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId == targetCohortIds) %>%
    dplyr::select("personId") %>%
    dplyr::pull()

  andromeda$currentCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$personId %in% selectPeople)

  nRows <- andromeda$cohortTable %>%
    dplyr::count() %>%
    dplyr::pull()
  
  if (nRows > 0) {
    # Preprocess the target/event cohorts to create treatment history
    createTreatmentHistory(
      andromeda,
      targetCohortIds,
      eventCohortIds,
      exitCohortIds,
      settings$periodPriorToIndex,
      settings$includeTreatments
    )

    andromeda$exitHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$type == "exit") %>%
      dplyr::select(-"type")

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$type == "event")

    # Apply pathway settings to create treatment pathways
    message("Construct treatment pathways, this may take a while for larger datasets.")

    message(glue::glue("Original number of rows: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))

    doEraDuration(
      andromeda,
      settings$minEraDuration
    )

    doSplitEventCohorts(
      andromeda,
      settings$splitEventCohorts,
      settings$splitTime
    )

    doEraCollapse(
      andromeda,
      settings$eraCollapseSize
    )

    doCombinationWindow(
      andromeda,
      settings$combinationWindow,
      settings$minPostCombinationDuration
    )

    doFilterTreatments(
      andromeda,
      settings$filterTreatments
    )

    andromeda$exitHistory <- andromeda$exitHistory %>%
      dplyr::mutate(eventCohortId = as.character(as.integer(.data$eventCohortId)))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::union_all(andromeda$exitHistory)

    if (andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% pull() > 0) {
      # Add eventSeq number to determine order of treatments in pathway
      message("Adding drug sequence number.")

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::group_by(.data$personId) %>%
        dplyr::mutate(eventSeq = dplyr::row_number())

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::filter(.data$eventSeq <= !!settings$maxPathLength)

      nRows <- andromeda$treatmentHistory %>%
        dplyr::count() %>%
        dplyr::pull()
      message(sprintf("After maxPathLength: %s", nRows))

      # Add event_cohort_name (instead of only event_cohort_id)
      message("Adding concept names.")

      addLabels(andromeda = andromeda)

      # Order the combinations
      message("Ordering the combinations.")

      eventCohortNames <- andromeda$treatmentHistory %>%
        dplyr::select("eventCohortName") %>%
        dplyr::pull() %>%
        stringr::str_split(pattern = "\\+") %>%
        lapply(FUN = function(x) {
          paste(sort(x), collapse = "+")
        }) %>%
        unlist()

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::collect() %>%
        dplyr::mutate(eventCohortName = eventCohortNames)

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        mutate(indexYear = floor(.data$indexYear / 365.25) + 1970)
    }
  }
  message("constructPathways done.")
  return(andromeda)
}

#' createTreatmentHistory
#'
#' @template param_andromeda
#' @param targetCohortIds (`numeric(n)`)
#' @param eventCohortIds (`numeric(n)`)
#' @param exitCohortIds (`numeric(n)`)
#' @template param_periodPriorToIndex
#' @template param_includeTreatments
#'
#' @return (`data.frame()`)\cr
#' \enumerate{
#'   \item (`numeric()`) personId
#'   \item (`numeric()`) indexYear
#'   \item (`numeric()`) eventCohortId
#'   \item (`as.Date()`) eventStartDate
#'   \item (`as.Date()`) eventEndDate
#'   \item (`character()`) type
#'   \item (`difftime()`) durationEra
#'   \item (`difftime()`) gapSame
#' }
createTreatmentHistory <- function(
    andromeda,
    targetCohortIds,
    eventCohortIds,
    exitCohortIds,
    periodPriorToIndex,
    includeTreatments) {
  andromeda$targetCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% targetCohortIds) %>%
    dplyr::mutate(type = "target") %>%
    dplyr::mutate(indexYear = as.numeric(format(.data$startDate, "%Y")))

  # Select event cohorts for target cohort and merge with start/end date and
  # index year
  andromeda$eventCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% eventCohortIds) %>%
    dplyr::mutate(type = "event")

  andromeda$exitCohorts <- andromeda$cohortTable %>%
    dplyr::filter(.data$cohortId %in% exitCohortIds) %>%
    dplyr::mutate(type = "exit")

  nRows <- andromeda$exitCohorts %>%
    dplyr::count() %>%
    dplyr::pull()
  
  if (nRows > 0) {
    andromeda$eventCohorts <- andromeda$eventCohorts %>%
      dplyr::union_all(andromeda$exitCohorts)
  }

  andromeda$cohortTable <- dplyr::full_join(
    x = andromeda$eventCohorts,
    y = andromeda$targetCohorts,
    by = "personId"
  )

  # Only keep event cohorts starting (startDate) or ending (endDate) after
  # target cohort start date
  if (includeTreatments == "startDate") {
    andromeda$cohortTable <- andromeda$cohortTable %>%
      dplyr::filter(.data$startDate.y - periodPriorToIndex <= .data$startDate.x) %>%
      dplyr::filter(.data$startDate.x < .data$endDate.y)
  } else if (includeTreatments == "endDate") {
    andromeda$cohortTable <- andromeda$cohortTable %>%
      dplyr::filter(.data$startDate.y - periodPriorToIndex <= .data$endDate.x) %>%
      dplyr::filter(.data$startDate.x < .data$endDate.y) %>%
      dplyr::mutate(
        startDate.x = pmax(.data$startDate.y - periodPriorToIndex, .data$startDate.x)
      )
  } else {
    warning(paste(
      "includeTreatments input incorrect, ",
      "return all event cohorts ('includeTreatments')"
    ))

    andromeda$cohortTable <- andromeda$cohortTable %>%
      dplyr::filter(.data$startDate.y - periodPriorToIndex <= .data$startDate.x) %>%
      dplyr::filter(.data$startDate.x < .data$endDate.y)
  }

  andromeda$cohortTable <- andromeda$cohortTable %>%
    dplyr::select("personId", "indexYear", "cohortId.x", "startDate.x", "endDate.x", "type.x")

  andromeda$cohortTable <- andromeda$cohortTable %>%
    dplyr::rename(
      eventCohortId = "cohortId.x",
      eventStartDate = "startDate.x",
      eventEndDate = "endDate.x",
      type = "type.x"
    )

  # Calculate duration and gap same
  andromeda$cohortTable <- andromeda$cohortTable %>%
    dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)

  andromeda$cohortTable <- andromeda$cohortTable %>%
    dplyr::arrange(.data$eventStartDate, .data$eventEndDate)

  andromeda$cohortTable <- andromeda$cohortTable %>%
    dplyr::group_by(.data$personId, .data$eventCohortId) %>%
    dplyr::mutate(lagVariable = dplyr::lag(.data$eventEndDate)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gapSame = .data$eventStartDate - .data$lagVariable) %>%
    dplyr::select(-"lagVariable")

  andromeda$treatmentHistory <- andromeda$cohortTable
  return(invisible(NULL))
}

#' doEraDuration
#'
#' @param minEraDuration (`integer(1)`)
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
doEraDuration <- function(andromeda, minEraDuration) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::filter(.data$durationEra >= minEraDuration)
  
  nRows <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()
  
  message(sprintf("After minEraDuration: %s", nRows))
  return(invisible(NULL))
}

#' doSplitEventCohorts
#'
#' Splits the treatmentHistory data.frame based on event cohorts into ‘acute’
#' and ‘therapy’ cohorts.
#'
#' @param splitEventCohorts (`character(n)`)
#'
#' @param splitTime (`integer(1)`)
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
doSplitEventCohorts <- function(
    andromeda,
    splitEventCohorts,
    splitTime) {
  if (all(!splitEventCohorts == "")) {
    # Load in labels cohorts
    labels <- andromeda$cohorts %>%
      dplyr::collect()

    # Check if splitEventCohorts == splitTime
    checkmate::assertTRUE(length(splitEventCohorts) == length(splitTime))

    for (c in seq_len(length(splitEventCohorts))) {
      cohort <- splitEventCohorts[c]
      cutoff <- splitTime[c]

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::union_all(
          andromeda$treatmentHistory %>%
            dplyr::filter(.data$eventCohortId == cohort) %>%
            dplyr::filter(.data$durationEra < cutoff) %>%
            dplyr::mutate(eventCohortId = as.integer(paste0(cohort, 1)))
        )

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::union_all(
          andromeda$treatmentHistory %>%
            dplyr::filter(.data$eventCohortId == cohort) %>%
            dplyr::filter(.data$durationEra >= cutoff) %>%
            dplyr::mutate(eventCohortId = as.integer(paste0(cohort, 2)))
        )

      # Add new labels
      original <- labels %>%
        dplyr::filter(.data$cohortId == as.integer(cohort))

      acute <- original
      acute$cohortId <- as.integer(paste0(cohort, 1))
      acute$cohortName <- paste0(acute$cohortName, " (acute)")

      therapy <- original
      therapy$cohortId <- as.integer(paste0(cohort, 2))
      therapy$cohortName <- paste0(therapy$cohortName, " (therapy)")

      labels <- labels %>%
        dplyr::filter(.data$cohortId != as.integer(cohort))

      labels <- rbind(labels, acute, therapy)
    }
  }
  return(invisible(NULL))
}


#' doEraCollapse
#'
#' Updates the treatmentHistory data.frame where if gapSame is smaller than the
#' specified era collapse size (eraCollapseSize) are collapsed
#'
#' @param andromeda (`Andromeda::andromeda()`)
#' @param eraCollapseSize (`integer(1)`)
#'
#' @return (`invisible(NULL)`)
doEraCollapse <- function(andromeda, eraCollapseSize) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    arrange(.data$personId, .data$eventCohortId, .data$eventStartDate, .data$eventEndDate)
  
  rows <- which(andromeda$treatmentHistory %>% select("gapSame") %>% pull() < eraCollapseSize)

  for (r in rev(rows)) {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(eventEndDate = dplyr::case_when(dplyr::row_number() == r - 1 ~ eventEndDate))
  }

  # Remove all rows with gap_same < eraCollapseSize
  if (length(rows) > 0) {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(!dplyr::row_number() %in% rows)
  }

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::select(-"gapSame") %>%
    dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)

  nRows <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()
  
  message(sprintf("After eraCollapseSize: %s", nRows))
  return(invisible(NULL))
}


#' Combine overlapping events into combinations
#'
#' doCombinationWindow is an internal function that combines overlapping events
#' into combination events. It accepts a treatmentHistory dataframe and returns
#' a modified treatmentHistory dataframe. The returned treatmentHistory
#' dataframe always has the property that a person is only in one event cohort,
#' which might be a combination event cohort, at any point time.
#'
#' @param combinationWindow (`integer(1)`)
#' @param minPostCombinationDuration (`integer(1)`)
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
doCombinationWindow <- function(
    andromeda,
    combinationWindow,
    minPostCombinationDuration) {
  time1 <- Sys.time()

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(eventCohortId = as.character(floor(.data$eventCohortId)))

  # Find which rows contain some overlap
  selectRowsCombinationWindow(andromeda)

  # While rows that need modification exist:
  iterations <- 1

  # n <- andromeda$treatmentHistory %>%
  #   summarise(sum = sum(.data$selectedRows), .groups = "drop") %>%
  #   dplyr::pull()

  while (andromeda$treatmentHistory %>%
         dplyr::filter(.data$selectedRows) %>%
         dplyr::count() %>%
         dplyr::pull() != 0) {
    # Which rows have gap previous shorter than combination window OR
    # min(current duration era, previous duration era) -> add column switch
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      mutate(switch = case_when(
        .data$selectedRows == 1 &
          (-.data$gapPrevious < combinationWindow &
            !(-.data$gapPrevious == .data$durationEra |
              -gapPrevious == dplyr::lag(.data$durationEra))) ~ 1,
        .default = 0
      ))

    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] <=
    # treatmentHistory[r, event_end_date] ->
    # add column combination first received, first stopped
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(combinationFRFS = case_when(
        .data$selectedRows == 1 & switch == 0 & dplyr::lag(eventEndDate) <= eventEndDate ~ 1,
        .default = 0
      ))

    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] >
    # treatmentHistory[r, event_end_date] ->
    # add column combination last received, first stopped

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(combinationLRFS = dplyr::case_when(
        .data$selectedRows == 1 & .data$switch == 0 & dplyr::lag(.data$eventEndDate) > .data$eventEndDate ~ 1,
        .default = 0
      ))

    message(sprintf(
      "Selected %s \nout of %s rows\nIteration: %s\nSwitches: %s\nFRFS Combinations: %s\nLRFS Combinations: %s",
    andromeda$treatmentHistory %>%
      dplyr::summarise(sum = sum(.data$selectedRows, na.rm = TRUE)) %>%
      dplyr::pull(),
    andromeda$treatmentHistory %>%
      dplyr::count() %>%
      dplyr::pull(),
    iterations,
    andromeda$treatmentHistory %>%
      dplyr::summarise(sum = sum(!is.na(.data$switch), na.rm = TRUE)) %>%
      dplyr::pull(),
    andromeda$treatmentHistory %>%
      dplyr::summarise(sum = sum(.data$combinationFRFS, na.rm = TRUE)) %>%
      dplyr::pull(),
    andromeda$treatmentHistory %>%
      dplyr::summarise(sum = sum(.data$combinationLRFS, na.rm = TRUE)) %>%
      dplyr::pull()
    ))

    sumSwitchComb <- sum(
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$switch, na.rm = TRUE)) %>%
        dplyr::pull(),
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$combinationFRFS, na.rm = TRUE)) %>%
        dplyr::pull(),
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$combinationLRFS, na.rm = TRUE)) %>%
        dplyr::pull()
    )

    sumSelectedRows <- andromeda$treatmentHistory %>%
      dplyr::summarise(sum = sum(.data$selectedRows, na.rm = TRUE)) %>%
      dplyr::pull()

    if (sumSwitchComb != sumSelectedRows) {
      stop(sprintf("%s does not equal total sum %s", sumSelectedRows, sumSwitchComb))
    }

    # Do transformations for each of the three newly added columns
    # Construct helpers

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId) %>%
      dplyr::mutate(eventStartDateNext = dplyr::lead(.data$eventStartDate))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId) %>%
      dplyr::mutate(eventEndDatePrevious = dplyr::lag(.data$eventEndDate))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId) %>%
      dplyr::mutate(eventEndDateNext = dplyr::lead(.data$eventEndDate))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId) %>%
      dplyr::mutate(eventCohortIdPrevious = dplyr::lag(.data$eventCohortId)) %>%
      dplyr::ungroup()

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(eventEndDate = dplyr::case_when(
        dplyr::lead(.data$switch) == 1 ~ .data$eventStartDateNext,
        .default = .data$eventEndDate
      ))

    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$combinationFRFS == 1)

    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsFRFS_%s", iterations)]] %>%
      dplyr::mutate(eventEndDate = .data$eventEndDatePrevious)

    andromeda[[sprintf("addRowsFRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsFRFS_%s", iterations)]] %>%
      dplyr::mutate(eventCohortId = paste0(.data$eventCohortId, "+", .data$eventCohortIdPrevious))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        eventEndDate = dplyr::case_when(
          dplyr::lead(.data$combinationFRFS) == 1 ~ eventStartDateNext,
          .default = .data$eventEndDate
        ),
        checkDuration = dplyr::case_when(dplyr::lead(.data$combinationFRFS) == 1 ~ 1)
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        eventStartDate = dplyr::case_when(
          .data$combinationFRFS == 1 ~ .data$eventEndDatePrevious,
          .default = .data$eventStartDate
        ),
        checkDuration = dplyr::case_when(
          .data$combinationFRFS == 1 ~ 1,
          .default = .data$checkDuration
        )
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(eventCohortId = dplyr::case_when(
        .data$combinationLRFS == 1 ~ paste0(.data$eventCohortId, "+", .data$eventCohortIdPrevious),
        .default = .data$eventCohortId
      ))

    andromeda[[sprintf("addRowsLRFS_%s", iterations)]] <- andromeda$treatmentHistory %>%
      dplyr::filter(dplyr::lead(.data$combinationLRFS) == 1)

    andromeda[[sprintf("addRowsLRFS_%s", iterations)]] <- andromeda[[sprintf("addRowsLRFS_%s", iterations)]] %>%
      dplyr::mutate(
        eventStartDate = .data$eventEndDateNext,
        checkDuration = 1
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        eventEndDate = dplyr::case_when(
          dplyr::lead(.data$combinationLRFS) == 1 ~ .data$eventStartDateNext,
          .default = .data$eventEndDate
        ),
        checkDuration = dplyr::case_when(
          dplyr::lead(.data$combinationLRFS) == 1 ~ 1,
          .default = .data$checkDuration
        )
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::union_all(andromeda[[sprintf("addRowsFRFS_%s", iterations)]]) %>%
      dplyr::union_all(andromeda[[sprintf("addRowsLRFS_%s", iterations)]]) %>%
      dplyr::mutate(durationEra = .data$eventEndDate - .data$eventStartDate)

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$durationEra >= minPostCombinationDuration | is.na(.data$durationEra))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::select(
        "personId", "indexYear", "eventCohortId",
        "eventStartDate", "eventEndDate", "durationEra", "gapPrevious"
      )

    selectRowsCombinationWindow(andromeda)

    iterations <- iterations + 1
  }

  nRows <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()
  
  message(sprintf("After combinationWindow: %s", nRows))

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    select(-"gapPrevious", -"selectedRows")

  time2 <- Sys.time()
  message(sprintf(
    "Time needed to execute combination window %s",
    difftime(time2, time1, units = 'mins')
  ))

  return(invisible(NULL))
}

#' selectRowsCombinationWindow
#'
#' Help function for doCombinationWindow that selects one overlapping drug era
#' per person to modify in next iteration of the combination window.
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (`invisible(NULL)`)
selectRowsCombinationWindow <- function(andromeda) {
  # Order treatmentHistory by person_id, event_start_date, event_end_date
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::mutate(gapPrevious = .data$eventStartDate - dplyr::lag(.data$eventEndDate)) %>%
    ungroup()

  # Find all rows with gap_previous < 0
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(allRows = ifelse(.data$gapPrevious < 0, dplyr::row_number(), NA))

  # Select one row per iteration for each person
  rows <- andromeda$treatmentHistory %>%
    dplyr::filter(!is.na(.data$allRows)) %>%
    dplyr::group_by(.data$personId) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select("allRows") %>%
    dplyr::pull()

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(selectedRows = dplyr::case_when(
      dplyr::row_number() %in% rows ~ 1,
      .default = 0
    ))

  # treatmentHistory[, ALL_ROWS := NULL]
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::select(-"allRows") %>%
    dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate, .data$eventCohortId)
  return(invisible(NULL))
}


#' doFilterTreatments
#'
#' Updates the treatmentHistory data.frame where the desired event cohorts are
#' maintained for the visualizations
#'
#' @param andromeda (`Andromeda::andromeda()`)
#' @param filterTreatments (`character(1)`)
#'
#' @return (`invisible(NULL)`)
doFilterTreatments <- function(andromeda, filterTreatments) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::arrange(.data$personId, .data$eventStartDate, .data$eventEndDate)

  if (filterTreatments != "All") {
    message("Order the combinations.")

    combi <- grep(
      pattern = "+",
      x = andromeda$treatmentHistory %>%
        dplyr::select("eventCohortId") %>%
        dplyr::pull(), fixed = TRUE
    )

    if (length(combi) > 0) {
      mem <- andromeda$treatmentHistory %>%
        dplyr::collect()

      conceptIds <- strsplit(
        x = mem$eventCohortId[combi],
        split = "+",
        fixed = TRUE
      )

      mem$eventCohortId[combi] <- sapply(
        X = conceptIds,
        FUN = function(x) {
          paste(sort(x), collapse = "+")
        }
      )

      andromeda$treatmentHistory <- mem
      rm("mem")
    }
  }

  if (filterTreatments == "First") {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId, .data$eventCohortId) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  } else if (filterTreatments == "Changes") {
    # Group all rows per person for which previous treatment is same
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::collect() %>%
      dplyr::mutate(group = dplyr::consecutive_id(.data$personId, .data$eventCohortId))

    # Remove all rows with same sequential treatments
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$personId, .data$indexYear, .data$eventCohortId, .data$group) %>%
      dplyr::summarise(
        eventStartDate = min(.data$eventStartDate),
        eventEndDate = max(.data$eventEndDate),
        durationEra = sum(.data$durationEra, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$personId, .data$indexYear, .data$group) %>%
      dplyr::select(-"group")
  }
  nRows <- andromeda$treatmentHistory %>%
    dplyr::count() %>%
    dplyr::pull()

  message(sprintf("After filterTreatments: %s", nRows))
  return(invisible(NULL))
}

#' addLabels
#'
#' Adds back cohort names to concept ids.
#'
#' @param andromeda (`Andromeda::andromeda()`)
#'
#' @return (invisible(NULL))
addLabels <- function(andromeda) {
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    mutate(eventCohortId = as.character(.data$eventCohortId))

  labels <- andromeda$cohorts %>%
    dplyr::collect() %>%
    dplyr::filter(.data$type != "target") %>%
    dplyr::select("cohortId", "cohortName")

  # labels <- labels[labels$cohortType == "event" | labels$cohortType == "exit", c("cohortId", "cohortName")]
  colnames(labels) <- c("eventCohortId", "eventCohortName")

  andromeda$labels <- labels %>%
    mutate(eventCohortId = as.character(.data$eventCohortId))

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    merge(andromeda$labels, all.x = TRUE, by = "eventCohortId")

  mem <- andromeda$treatmentHistory %>%
    dplyr::collect()

  mem$eventCohortName[is.na(mem$eventCohortName)] <- sapply(
    X = mem$eventCohortId[is.na(mem$eventCohortName)],
    FUN = function(x) {
      # Revert search to look for longest concept_ids first

      for (l in seq_len(nrow(labels))) {
        # If treatment occurs twice in a combination (as monotherapy and as part
        # of fixed-combination) -> remove monotherapy occurrence
        if (any(grep(labels$eventCohortName[l], x))) {
          x <- gsub(labels$eventCohortId[l], "+", x)
        } else {
          x <- gsub(labels$eventCohortId[l], labels$eventCohortName[l], x)
        }
      }
      return(x)
    }
  )

  # Filter out + at beginning/end or repetitions
  mem$eventCohortName <- gsub(
    pattern = "(^\\++|\\++$)",
    replacement = "+",
    x = mem$eventCohortName
  )

  andromeda$treatmentHistory <- mem
  rm("mem")
  return(invisible(NULL))
}
