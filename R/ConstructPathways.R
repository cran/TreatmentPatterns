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

  message(glue::glue("Constructing treatment pathways: {settings$studyName}"))

  selectPeople <- andromeda$fullCohorts %>%
    dplyr::filter(.data$cohort_id == targetCohortIds) %>%
    dplyr::select("person_id") %>%
    dplyr::pull()

  andromeda$currentCohorts <- andromeda$fullCohorts %>%
    dplyr::filter(.data$person_id %in% selectPeople)

  if (andromeda$currentCohorts %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull() > 0) {
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
      dplyr::mutate(event_cohort_id = as.character(as.integer(.data$event_cohort_id)))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::union_all(andromeda$exitHistory)

    if (andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% pull() > 0) {
      # Add event_seq number to determine order of treatments in pathway
      message("Adding drug sequence number.")

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date)

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::group_by(.data$person_id) %>%
        dplyr::mutate(event_seq = dplyr::row_number())

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::filter(.data$event_seq <= !!settings$maxPathLength)

      message(glue::glue("After maxPathLength: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))

      # Add event_cohort_name (instead of only event_cohort_id)
      message("Adding concept names.")

      addLabels(andromeda = andromeda)

      # Order the combinations
      message("Ordering the combinations.")

      eventCohortNames <- andromeda$treatmentHistory %>%
        dplyr::select("event_cohort_name") %>%
        dplyr::pull() %>%
        stringr::str_split(pattern = "\\+") %>%
        lapply(FUN = function(x) {
          paste(sort(x), collapse = "+")
        }) %>%
        unlist()

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        collect() %>%
        mutate(event_cohort_name = eventCohortNames)

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        mutate(index_year = floor(.data$index_year / 365.25) + 1970)
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
#'   \item (`numeric()`) person_id
#'   \item (`numeric()`) index_year
#'   \item (`numeric()`) event_cohort_id
#'   \item (`as.Date()`) event_start_date
#'   \item (`as.Date()`) event_end_date
#'   \item (`character()`) type
#'   \item (`difftime()`) duration_era
#'   \item (`difftime()`) gap_same
#' }
createTreatmentHistory <- function(
    andromeda,
    targetCohortIds,
    eventCohortIds,
    exitCohortIds,
    periodPriorToIndex,
    includeTreatments) {
  andromeda$targetCohorts <- andromeda$currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% targetCohortIds) %>%
    dplyr::mutate(type = "target") %>%
    dplyr::mutate(index_year = as.numeric(format(.data$start_date, "%Y")))

  # Select event cohorts for target cohort and merge with start/end date and
  # index year
  andromeda$eventCohorts <- andromeda$currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% eventCohortIds) %>%
    dplyr::mutate(type = "event")

  andromeda$exitCohorts <- andromeda$currentCohorts %>%
    dplyr::filter(.data$cohort_id %in% exitCohortIds) %>%
    dplyr::mutate(type = "exit")

  if (andromeda$exitCohorts %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull() > 0) {
    andromeda$eventCohorts <- andromeda$eventCohorts %>%
      dplyr::union_all(andromeda$exitCohorts)
  }

  andromeda$currentCohorts <- dplyr::full_join(
    x = andromeda$eventCohorts,
    y = andromeda$targetCohorts,
    by = "person_id"
  )

  # Only keep event cohorts starting (startDate) or ending (endDate) after
  # target cohort start date
  if (includeTreatments == "startDate") {
    andromeda$currentCohorts <- andromeda$currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$start_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y)
  } else if (includeTreatments == "endDate") {
    andromeda$currentCohorts <- andromeda$currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$end_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y) %>%
      dplyr::mutate(
        start_date.x = pmax(.data$start_date.y - periodPriorToIndex, .data$start_date.x)
      )
  } else {
    warning(paste(
      "includeTreatments input incorrect, ",
      "return all event cohorts ('includeTreatments')"
    ))

    andromeda$currentCohorts <- andromeda$currentCohorts %>%
      dplyr::filter(.data$start_date.y - periodPriorToIndex <= .data$start_date.x) %>%
      dplyr::filter(.data$start_date.x < .data$end_date.y)
  }

  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::select("person_id", "index_year", "cohort_id.x", "start_date.x", "end_date.x", "type.x")

  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::rename(
      event_cohort_id = "cohort_id.x",
      event_start_date = "start_date.x",
      event_end_date = "end_date.x",
      type = "type.x"
    )

  # Calculate duration and gap same
  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::mutate(duration_era = .data$event_end_date - .data$event_start_date)

  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::arrange(.data$event_start_date, .data$event_end_date)

  andromeda$currentCohorts <- andromeda$currentCohorts %>%
    dplyr::group_by(.data$person_id, .data$event_cohort_id) %>%
    dplyr::mutate(lag_variable = dplyr::lag(.data$event_end_date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gap_same = .data$event_start_date - .data$lag_variable) %>%
    dplyr::select(-"lag_variable")

  andromeda$treatmentHistory <- andromeda$currentCohorts
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
    dplyr::filter(.data$duration_era >= minEraDuration)
  message(glue::glue("After minEraDuration: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
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
    labels <- andromeda$cohorts %>% dplyr::collect()

    # Check if splitEventCohorts == splitTime
    checkmate::assertTRUE(length(splitEventCohorts) == length(splitTime))

    for (c in seq_len(length(splitEventCohorts))) {
      cohort <- splitEventCohorts[c]
      cutoff <- splitTime[c]

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::union_all(
          andromeda$treatmentHistory %>%
            dplyr::filter(.data$event_cohort_id == cohort) %>%
            dplyr::filter(.data$duration_era < cutoff) %>%
            dplyr::mutate(event_cohort_id = as.integer(paste0(cohort, 1)))
        )

      andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
        dplyr::union_all(
          andromeda$treatmentHistory %>%
            dplyr::filter(.data$event_cohort_id == cohort) %>%
            dplyr::filter(.data$duration_era >= cutoff) %>%
            dplyr::mutate(event_cohort_id = as.integer(paste0(cohort, 2)))
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
    arrange(.data$person_id, .data$event_cohort_id, .data$event_start_date, .data$event_end_date)
  rows <- which(andromeda$treatmentHistory %>% select("gap_same") %>% pull() < eraCollapseSize)

  for (r in rev(rows)) {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(event_end_date = dplyr::case_when(dplyr::row_number() == r - 1 ~ event_end_date))
  }

  # Remove all rows with gap_same < eraCollapseSize
  if (length(rows) > 0) {
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(!dplyr::row_number() %in% rows)
  }

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::select(-"gap_same") %>%
    dplyr::mutate(duration_era = .data$event_end_date - .data$event_start_date)

  message(glue::glue("After eraCollapseSize: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
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
    dplyr::mutate(event_cohort_id = as.character(floor(.data$event_cohort_id)))

  # Find which rows contain some overlap
  selectRowsCombinationWindow(andromeda)

  # While rows that need modification exist:
  iterations <- 1

  while (suppressWarnings(andromeda$treatmentHistory %>% summarise(sum = sum(.data$SELECTED_ROWS), .groups = "drop") %>% dplyr::pull() != 0)) {
    # Which rows have gap previous shorter than combination window OR
    # min(current duration era, previous duration era) -> add column switch
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      mutate(switch = case_when(
        .data$SELECTED_ROWS == 1 &
          (-.data$GAP_PREVIOUS < combinationWindow &
            !(-.data$GAP_PREVIOUS == .data$duration_era |
              -GAP_PREVIOUS == dplyr::lag(.data$duration_era))) ~ 1,
        .default = 0
      ))

    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] <=
    # treatmentHistory[r, event_end_date] ->
    # add column combination first received, first stopped
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      mutate(combination_FRFS = case_when(
        .data$SELECTED_ROWS == 1 & switch == 0 & dplyr::lag(event_end_date) <= event_end_date ~ 1,
        .default = 0
      ))

    # For rows selected not in column switch ->
    # if treatmentHistory[r - 1, event_end_date] >
    # treatmentHistory[r, event_end_date] ->
    # add column combination last received, first stopped

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(combination_LRFS = dplyr::case_when(
        .data$SELECTED_ROWS == 1 & .data$switch == 0 & dplyr::lag(.data$event_end_date) > .data$event_end_date ~ 1,
        .default = 0
      ))

    message(glue::glue(
      "Selected {andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$SELECTED_ROWS)) %>% dplyr::pull()} ",
      "out of {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()} rows\n",
      "Iteration: {iterations}\n",
      "Switches: {andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(!is.na(.data$switch))) %>% dplyr::pull()}\n",
      "FRFS Combinations: {andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_FRFS)) %>% dplyr::pull()}\n",
      "LRFS Combinations: {andromeda$treatmentHistory %>% dplyr::summarise(sum = sum(.data$combination_LRFS)) %>% dplyr::pull()}"
    ))

    sumSwitchComb <- sum(
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$switch, na.rm = TRUE)) %>%
        dplyr::pull(),
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$combination_FRFS, na.rm = TRUE)) %>%
        dplyr::pull(),
      andromeda$treatmentHistory %>%
        dplyr::summarise(sum = sum(.data$combination_LRFS, na.rm = TRUE)) %>%
        dplyr::pull()
    )

    sumSelectedRows <- andromeda$treatmentHistory %>%
      dplyr::summarise(sum = sum(.data$SELECTED_ROWS)) %>%
      dplyr::pull()

    if (sumSwitchComb != sumSelectedRows) {
      stop(glue::glue(
        "{sumSelectedRows} does not equal total sum {sumSwitchComb}"
      ))
    }

    # Do transformations for each of the three newly added columns
    # Construct helpers

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_start_date_next = dplyr::lead(.data$event_start_date))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_end_date_previous = dplyr::lag(.data$event_end_date))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_end_date_next = dplyr::lead(.data$event_end_date))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::mutate(event_cohort_id_previous = dplyr::lag(.data$event_cohort_id)) %>%
      dplyr::ungroup()

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(event_end_date = dplyr::case_when(
        dplyr::lead(.data$switch) == 1 ~ .data$event_start_date_next,
        .default = .data$event_end_date
      ))

    andromeda[[glue::glue("addRowsFRFS_{iterations}")]] <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$combination_FRFS == 1)

    andromeda[[glue::glue("addRowsFRFS_{iterations}")]] <- andromeda[[glue::glue("addRowsFRFS_{iterations}")]] %>%
      dplyr::mutate(event_end_date = .data$event_end_date_previous)

    andromeda[[glue::glue("addRowsFRFS_{iterations}")]] <- andromeda[[glue::glue("addRowsFRFS_{iterations}")]] %>%
      dplyr::mutate(event_cohort_id = paste0(.data$event_cohort_id, "+", .data$event_cohort_id_previous))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        event_end_date = dplyr::case_when(
          dplyr::lead(.data$combination_FRFS) == 1 ~ event_start_date_next,
          .default = .data$event_end_date
        ),
        check_duration = dplyr::case_when(dplyr::lead(.data$combination_FRFS) == 1 ~ 1)
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        event_start_date = dplyr::case_when(
          .data$combination_FRFS == 1 ~ .data$event_end_date_previous,
          .default = .data$event_start_date
        ),
        check_duration = dplyr::case_when(
          .data$combination_FRFS == 1 ~ 1,
          .default = .data$check_duration
        )
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(event_cohort_id = dplyr::case_when(
        .data$combination_LRFS == 1 ~ paste0(.data$event_cohort_id, "+", .data$event_cohort_id_previous),
        .default = .data$event_cohort_id
      ))

    andromeda[[glue::glue("addRowsLRFS_{iterations}")]] <- andromeda$treatmentHistory %>%
      dplyr::filter(lead(.data$combination_LRFS) == 1)

    andromeda[[glue::glue("addRowsLRFS_{iterations}")]] <- andromeda[[glue::glue("addRowsLRFS_{iterations}")]] %>%
      dplyr::mutate(
        event_start_date = .data$event_end_date_next,
        check_duration = 1
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::mutate(
        event_end_date = dplyr::case_when(
          dplyr::lead(.data$combination_LRFS) == 1 ~ .data$event_start_date_next,
          .default = .data$event_end_date
        ),
        check_duration = dplyr::case_when(
          dplyr::lead(.data$combination_LRFS) == 1 ~ 1,
          .default = .data$check_duration
        )
      )

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::union_all(andromeda[[glue::glue("addRowsFRFS_{iterations}")]]) %>%
      dplyr::union_all(andromeda[[glue::glue("addRowsLRFS_{iterations}")]]) %>%
      dplyr::mutate(duration_era = .data$event_end_date - .data$event_start_date)

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::filter(.data$duration_era >= minPostCombinationDuration | is.na(.data$duration_era))

    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::select(
        "person_id", "index_year", "event_cohort_id",
        "event_start_date", "event_end_date", "duration_era", "GAP_PREVIOUS"
      )

    selectRowsCombinationWindow(andromeda)

    iterations <- iterations + 1
  }

  message(glue::glue("After combinationWindow: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    select(-"GAP_PREVIOUS", -"SELECTED_ROWS")

  time2 <- Sys.time()
  message(glue::glue(
    "Time needed to execute combination window {difftime(time2, time1, units = 'mins')}"
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
    arrange(.data$person_id, .data$event_start_date, .data$event_end_date)

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::mutate(GAP_PREVIOUS = .data$event_start_date - dplyr::lag(.data$event_end_date)) %>%
    ungroup()

  # Find all rows with gap_previous < 0
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(ALL_ROWS = ifelse(.data$GAP_PREVIOUS < 0, dplyr::row_number(), NA))

  # Select one row per iteration for each person
  rows <- andromeda$treatmentHistory %>%
    dplyr::filter(!is.na(.data$ALL_ROWS)) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select("ALL_ROWS") %>%
    dplyr::pull()

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::mutate(SELECTED_ROWS = dplyr::case_when(
      dplyr::row_number() %in% rows ~ 1,
      .default = 0
    ))

  # treatmentHistory[, ALL_ROWS := NULL]
  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    dplyr::select(-"ALL_ROWS") %>%
    dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date, .data$event_cohort_id)
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
    dplyr::arrange(.data$person_id, .data$event_start_date, .data$event_end_date)

  if (filterTreatments != "All") {
    message("Order the combinations.")

    combi <- grep(
      pattern = "+",
      x = andromeda$treatmentHistory %>%
        dplyr::select("event_cohort_id") %>%
        dplyr::pull(), fixed = TRUE
    )

    if (length(combi) > 0) {
      mem <- andromeda$treatmentHistory %>% dplyr::collect()

      conceptIds <- strsplit(
        x = mem$event_cohort_id[combi],
        split = "+",
        fixed = TRUE
      )

      mem$event_cohort_id[combi] <- sapply(
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
      dplyr::group_by(.data$person_id, .data$event_cohort_id) %>%
      dplyr::filter(dplyr::row_number() == 1) %>%
      dplyr::ungroup()
  } else if (filterTreatments == "Changes") {
    # Group all rows per person for which previous treatment is same
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::collect() %>%
      dplyr::mutate(group = dplyr::consecutive_id(.data$person_id, .data$event_cohort_id))

    # Remove all rows with same sequential treatments
    andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
      dplyr::group_by(.data$person_id, .data$index_year, .data$event_cohort_id, .data$group) %>%
      dplyr::summarise(
        event_start_date = min(.data$event_start_date),
        event_end_date = max(.data$event_end_date),
        duration_era = sum(.data$duration_era),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.data$person_id, .data$index_year, .data$group) %>%
      dplyr::select(-"group")
  }
  message(glue::glue("After filterTreatments: {andromeda$treatmentHistory %>% dplyr::summarise(n = dplyr::n()) %>% dplyr::pull()}"))
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
    mutate(event_cohort_id = as.character(.data$event_cohort_id))

  labels <- andromeda$cohorts %>%
    dplyr::collect() %>%
    dplyr::filter(.data$type != "target") %>%
    dplyr::select("cohortId", "cohortName")

  # labels <- labels[labels$cohortType == "event" | labels$cohortType == "exit", c("cohortId", "cohortName")]
  colnames(labels) <- c("event_cohort_id", "event_cohort_name")

  andromeda$labels <- labels %>%
    mutate(event_cohort_id = as.character(.data$event_cohort_id))

  andromeda$treatmentHistory <- andromeda$treatmentHistory %>%
    merge(andromeda$labels, all.x = TRUE, by = "event_cohort_id")

  mem <- andromeda$treatmentHistory %>% collect()

  mem$event_cohort_name[is.na(mem$event_cohort_name)] <- sapply(
    X = mem$event_cohort_id[is.na(mem$event_cohort_name)],
    FUN = function(x) {
      # Revert search to look for longest concept_ids first

      for (l in seq_len(nrow(labels))) {
        # If treatment occurs twice in a combination (as monotherapy and as part
        # of fixed-combination) -> remove monotherapy occurrence
        if (any(grep(labels$event_cohort_name[l], x))) {
          x <- gsub(labels$event_cohort_id[l], "+", x)
        } else {
          x <- gsub(labels$event_cohort_id[l], labels$event_cohort_name[l], x)
        }
      }
      return(x)
    }
  )

  # Filter out + at beginning/end or repetitions
  mem$event_cohort_name <- gsub(
    pattern = "(^\\++|\\++$)",
    replacement = "+",
    x = mem$event_cohort_name
  )

  andromeda$treatmentHistory <- mem
  rm("mem")
  return(invisible(NULL))
}
