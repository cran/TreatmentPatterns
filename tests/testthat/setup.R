ableToRun <- function() {
  invisible(all(
    require("Eunomia", character.only = TRUE),
    require("CirceR", character.only = TRUE),
    require("CohortGenerator", character.only = TRUE)
  ))
}

if (ableToRun()) {
  library(dplyr)
  library(CDMConnector)
  source(system.file(package = "TreatmentPatterns", "R-scripts", "runCG.R"))

  # Select Viral Sinusitis Cohort
  targetCohorts <- cohortsGenerated %>%
    filter(cohortName == "ViralSinusitis") %>%
    select(cohortId, cohortName)

  # Select everything BUT Viral Sinusitis cohorts
  eventCohorts <- cohortsGenerated %>%
    filter(cohortName != "ViralSinusitis" & cohortName != "Death") %>%
    select(cohortId, cohortName)

  exitCohorts <- cohortsGenerated %>%
    filter(cohortName == "Death") %>%
    select(cohortId, cohortName)

  cohorts <- dplyr::bind_rows(
    targetCohorts %>% mutate(type = "target"),
    eventCohorts %>% mutate(type = "event"),
    exitCohorts %>% mutate(type = "exit")
  )

  andromedaSetup <- TreatmentPatterns::computePathways(
    cohorts = cohorts,
    cohortTableName = "CohortTable",
    connectionDetails = connectionDetails,
    cdmSchema = "main",
    resultSchema = "main"
  )

  setupTempDir <- file.path(tempdir(), "setup")

  if (!dir.exists(setupTempDir)) {
    dir.create(setupTempDir)
  }

  Andromeda::saveAndromeda(
    andromeda = andromedaSetup,
    fileName = file.path(setupTempDir, "Andromeda"),
    maintainConnection = FALSE,
    overwrite = TRUE
  )

  # TreatmentPatterns::export(andromeda, outputPath = setupTempDir)
  # treatmentPathways <- read.csv(file.path(setupTempDir, "treatmentPathways.csv"))
  #
  # pathways <- treatmentPathways %>%
  #   dplyr::filter(.data$sex == "all" & .data$age == "all" & .data$index_year == "all")
  #
  # TreatmentPatterns::createSunburstPlot(
  #   treatmentPathways = pathways,
  #   outputFile = file.path(setupTempDir, "sunburst.html"))
  #
  # TreatmentPatterns::createSankeyDiagram(
  #   treatmentPathways = pathways,
  #   outputFile = file.path(setupTempDir, "sankey.html"))

  withr::local_envvar(
    EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
  )

  CDMConnector::downloadEunomiaData(
    overwrite = TRUE
  )

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  cdm <- cdmFromCon(con, cdmSchema = "main")

  cdm$condition_era %>%
    filter(condition_concept_id == 40481087) %>%
    summarise(n = n()) %>%
    pull() %>%
    message(" Persons with Viral sinusitis")
}
