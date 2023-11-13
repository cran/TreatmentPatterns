withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = testthat::teardown_env(),
  EUNOMIA_DATA_FOLDER = Sys.getenv("EUNOMIA_DATA_FOLDER", unset = tempfile())
)

if (require("Eunomia", character.only = TRUE, quietly = TRUE)) {
  CDMConnector::downloadEunomiaData(overwrite = TRUE)
}
