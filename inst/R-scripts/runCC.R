ableToRunCDMCon <- function() {
  installed <- all(require(DBI), require(duckdb))
  if (installed) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

if (ableToRunCDMCon()) {
  library(CDMConnector)
  
  tempEunomiaDir <- file.path(tempdir(), "Eunomia")
  
  Sys.setenv(EUNOMIA_DATA_FOLDER = tempEunomiaDir)
  downloadEunomiaData()
  
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = eunomia_dir())
  cdm <- cdm_from_con(con, cdm_schema = "main", write_schema = "main")
  
  cohortSet <- CDMConnector::readCohortSet(
    system.file(package = "TreatmentPatterns", "exampleCohorts")
  )
  
  cdm <- CDMConnector::generateCohortSet(
    cdm = cdm,
    cohortSet = cohortSet,
    name = "cohort_table",
    overwrite = TRUE
  )
}
