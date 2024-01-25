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
  treatmentPathways <- doGroupCombinations(
    treatmentPathways = treatmentPathways,
    groupCombinations = groupCombinations
  )
  
  if (is.null(colors)) {
    colors <- getColorPalette(treatmentPathways)
  }
  
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
  warning("`createSunburstPlot2()` is deprecated, please use `createSunburstPlot()`")
  TreatmentPatterns::createSunburstPlot(treatmentPathways, groupCombinations, ...)
}
