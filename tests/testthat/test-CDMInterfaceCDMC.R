if (ableToRun()) {
  library(testthat)
  library(TreatmentPatterns)
  library(dplyr)
  library(R6)
  library(DBI)
  library(duckdb)

  andromedaSetup <- Andromeda::loadAndromeda(
    fileName = file.path(setupTempDir, "Andromeda")
  )

  test_that("Method: new", {
    expect_true(R6::is.R6(
      TreatmentPatterns:::CDMInterface$new(
        cdm = cdm
      )
    ))
  })

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    cdm = cdm
  )

  test_that("Method: validate", {
    expect_true(R6::is.R6(cdmInterface$validate()))
  })

  andromCDMC <- Andromeda::andromeda()

  test_that("Method: fetchMetadata", {
    cdmInterface$fetchMetadata(andromCDMC)

    metadata <- andromCDMC$metadata %>% collect()

    expect_in(
      c("cdmSourceName", "cdmSourceAbbreviation", "cdmReleaseDate", "vocabularyVersion"),
      names(metadata)
    )

    expect_identical(metadata$rVersion, base::version$version.string)
    expect_identical(metadata$platform, base::version$platform)
    expect_identical(nrow(metadata), 1L)
    expect_identical(ncol(metadata), 8L)
  })

  andromCDMC$treatmentHistory <- andromedaSetup$treatmentHistory %>%
    select(-"age", -"sex")

  test_that("Method: addSex", {
    testthat::skip_on_ci()
    cdmInterface$addSex(andromCDMC)

    sex <- andromCDMC$sex %>% collect()
    TH <- andromCDMC$treatmentHistory %>% collect()

    expect_identical(ncol(sex), 2L)
    expect_identical(nrow(sex), 512L)

    expect_in(c("MALE", "FEMALE"), TH$sex)

    sexes <- TH %>%
      inner_join(sex, by = join_by(personId == personId)) %>%
      select("sex", "conceptName")

    expect_identical(sexes$sex, sexes$conceptName)
  })

  test_that("Method: addAge", {
    testthat::skip_on_ci()
    cdmInterface$addAge(andromCDMC)

    yearOfBirth <- andromCDMC$yearOfBirth %>% collect()
    TH <- andromCDMC$treatmentHistory %>% collect()

    expect_identical(ncol(yearOfBirth), 2L)
    # All people are included besides subset, probably not a problem.
    # expect_identical(nrow(year_of_birth), 512L)

    ages <- TH %>%
      inner_join(yearOfBirth, by = join_by(personId == personId)) %>%
      mutate(ageCheck = .data$indexYear - .data$yearOfBirth) %>%
      select("age", "ageCheck")

    expect_identical(
      ages$age,
      ages$ageCheck
    )
  })

  test_that("Method: fetchCohortTable", {
    # Setup dummy cohort table
    cdm$CohortTable <- cdm$condition_occurrence %>%
      filter(.data$condition_concept_id == 40481087) %>%
      inner_join(cdm$person, by = join_by(person_id == person_id)) %>%
      select("person_id", "condition_start_date", "condition_end_date") %>%
      mutate(cohort_definition_id = 1) %>%
      rename(
        cohort_start_date = "condition_start_date",
        cohort_end_date = "condition_end_date"
      )

    # Update CDM with new dummy data
    cdmInterface <- TreatmentPatterns:::CDMInterface$new(
      cdm = cdm
    )

    # Viral Sinusitis
    res <- cdmInterface$fetchCohortTable(1, cohortTableName = "CohortTable")

    expect_identical(ncol(res), 4L)
    expect_identical(res %>% collect() %>% nrow(), 17268L)

    # Empty
    res <- cdmInterface$fetchCohortTable(23, cohortTableName = "CohortTable")
    expect_identical(ncol(res), 4L)
    expect_identical(res %>% collect() %>% nrow(), 0L)
  })

  Andromeda::close(andromCDMC)
}
