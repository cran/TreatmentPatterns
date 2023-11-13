ableToRun <- function() {
  all(
    require("CirceR", character.only = TRUE, quietly = TRUE),
    require("Eunomia", character.only = TRUE, quietly = TRUE),
    require("CohortGenerator", character.only = TRUE, quietly = TRUE)
  )
}
