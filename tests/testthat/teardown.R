if (ableToRun()) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  Andromeda::close(andromedaSetup)
}
