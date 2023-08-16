if (ableToRun()) {
  library(testthat)
  library(TreatmentPatterns)
  library(dplyr)

  andromedaSetup <- Andromeda::loadAndromeda(
    fileName = file.path(setupTempDir, "Andromeda")
  )

  test_that("Method: new", {
    expect_true(R6::is.R6(
      TreatmentPatterns:::CDMInterface$new(
        connectionDetails = connectionDetails,
        cdmSchema = "main",
        resultSchema = "main"
      )
    ))
  })

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )

  test_that("Method: validate", {
    expect_true(R6::is.R6(cdmInterface$validate()))
  })

  andromDBC <- Andromeda::andromeda()

  test_that("Method: fetchMetadata", {
    cdmInterface$fetchMetadata(andromDBC)

    metadata <- andromDBC$metadata %>% collect()

    expect_in(
      c("CDM_SOURCE_NAME", "CDM_SOURCE_ABBREVIATION", "CDM_RELEASE_DATE", "VOCABULARY_VERSION"),
      names(metadata)
    )

    expect_identical(metadata$r_version, base::version$version.string)
    expect_identical(metadata$platform, base::version$platform)
    expect_identical(nrow(metadata), 1L)
    expect_identical(ncol(metadata), 8L)
  })

  andromDBC$treatmentHistory <- andromedaSetup$treatmentHistory %>%
    select(-"age", -"sex")

  test_that("Method: addSex", {
    skip_on_ci()
    cdmInterface$addSex(andromDBC)

    sex <- andromDBC$sex %>% collect()
    TH <- andromDBC$treatmentHistory %>% collect()

    expect_identical(ncol(sex), 2L)
    expect_identical(nrow(sex), 512L)

    expect_in(c("MALE", "FEMALE"), TH$sex)

    sexes <- TH %>%
      inner_join(sex, by = join_by(person_id == PERSON_ID)) %>%
      select("sex", "SEX")

    expect_identical(sexes$sex, sexes$SEX)
  })

  test_that("Method: addAge", {
    skip_on_ci()
    cdmInterface$addAge(andromDBC)

    year_of_birth <- andromDBC$year_of_birth %>% collect()
    TH <- andromDBC$treatmentHistory %>% collect()

    expect_identical(ncol(year_of_birth), 2L)
    expect_identical(nrow(year_of_birth), 512L)

    ages <- TH %>%
      inner_join(year_of_birth, by = join_by(person_id == PERSON_ID)) %>%
      mutate(ageCheck = .data$index_year - .data$YEAR_OF_BIRTH) %>%
      select("age", "ageCheck")

    expect_identical(
      ages$age,
      ages$ageCheck
    )
  })

  test_that("Method: fetchCohortTable", {
    # Viral Sinusitis
    res <- cdmInterface$fetchCohortTable(1, cohortTableName = "CohortTable")

    expect_identical(ncol(res), 4L)
    expect_identical(nrow(res), 2679L)

    # Empty
    res <- cdmInterface$fetchCohortTable(23, cohortTableName = "CohortTable")
    expect_identical(ncol(res), 4L)
    expect_identical(nrow(res), 0L)
  })

  Andromeda::close(andromDBC)
  Andromeda::close(andromedaSetup)
}
