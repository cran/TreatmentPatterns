#' createSunburstPlot
#' 
#' New sunburstPlot function
#'
#' @template param_treatmentPathways 
#' @template param_groupCombinations
#' @param ... Paramaters for \link[sunburstR]{sunburst}.
#'
#' @return (`htmlwidget`)
#' @export
#'
#' @examples
#' # Dummy data, typically read from treatmentPathways.csv
#' treatmentPatwhays <- data.frame(
#'   path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
#'            "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
#'   freq = c(206, 6, 14, 48, 221),
#'   sex = rep("all", 5),
#'   age = rep("all", 5),
#'   index_year = rep("all", 5)
#' )
#' 
#' createSunburstPlot(treatmentPatwhays)
createSunburstPlot <- function(treatmentPathways, groupCombinations = FALSE, ...) {
  validateCreateSunburstPlot()
  treatmentPathways <- doGroupCombinations(
    treatmentPathways = treatmentPathways,
    groupCombinations = groupCombinations
  )
  
  sunburstR::sunburst(
    data = treatmentPathways,
    sortFunction = htmlwidgets::JS("function (a, b) {return a.value - b.value;}"),
    ...
  )
}


#' createSunburstPlot2
#' 
#' DEPRECATED New sunburstPlot function
#'
#' @template param_treatmentPathways 
#' @template param_groupCombinations
#' @param ... Paramaters for \link[sunburstR]{sunburst}.
#'
#' @return (`htmlwidget`)
#' @export
#'
#' @examples
#' # Dummy data, typically read from treatmentPathways.csv
#' treatmentPatwhays <- data.frame(
#'   path = c("Acetaminophen", "Acetaminophen-Amoxicillin+Clavulanate",
#'            "Acetaminophen-Aspirin", "Amoxicillin+Clavulanate", "Aspirin"),
#'   freq = c(206, 6, 14, 48, 221),
#'   sex = rep("all", 5),
#'   age = rep("all", 5),
#'   index_year = rep("all", 5)
#' )
#' 
#' createSunburstPlot2(treatmentPatwhays)
createSunburstPlot2 <- function(treatmentPathways, groupCombinations = FALSE, ...) {
  warning(
    "`createSunburstPlot2()` is deprecated, please use `createSunburstPlot()`\n`createSunburstPlot2()` will be removed in 2.7.0"
  )
  TreatmentPatterns::createSunburstPlot(treatmentPathways, groupCombinations, ...)
}


validateCreateSunburstPlot <- function() {
  args <- eval(
    expr = expression(mget(names(formals()))),
    envir = sys.frame(sys.nframe() - 1)
  )

  assertCol <- checkmate::makeAssertCollection()
  checkmate::assertNames(
    x = names(args$treatmentPathways),
    type = "named",
    must.include = c("path", "freq"),
    .var.name = "treatmentPathways"
  )

  checkmate::assertLogical(
    x = args$groupCombinations,
    len = 1,
    null.ok = FALSE,
    add = assertCol,
    .var.name = "groupCombinations"
  )

  checkmate::reportAssertions(assertCol)
}
