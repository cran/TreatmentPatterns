#' @title SunburstPlot
#' 
#' @include InteractivePlots.R
#' 
#' @description
#' Class to handle the Sunburst plot of TreatmentPatterns.
#' 
#' @export
SunburstPlot <- R6::R6Class(
  classname = "SunburstPlot",
  inherit = InteracitvePlot,
  
  # Private ----
  private = list(
    ## Fields ----
    .jsShowLegend = "
    function(el, x) {
      d3.select(el).select('.sunburst-togglelegend').property('checked', true);
      d3.select(el).select('.sunburst-legend').style('visibility', '');
    }
    ",

    ## Methods ----
    renderPlot = function() {
      shiny::tagList(
        shiny::htmlOutput(shiny::NS(private$.namespace, "sunburst"))
      )
    },
    
    plot = function(input, output, inputHandler) {
      shiny::observeEvent(
        eventExpr = list(
          input$dbSelector,
          input[[private$.ageOption]],
          input[[private$.sexOption]],
          input[[private$.indexYearOption]],
          input[[private$.noneOption]],
          input[[private$.groupCombiOption]]
        ),
        handlerExpr = {
          if (!is.null(inputHandler$reactiveValues$treatmentPathways)) {
            if (nrow(inputHandler$reactiveValues$treatmentPathways) > 0) {
              private$.reactiveValues$filteredData <- private$formatData(
                inputHandler = inputHandler,
                input = input
              )
              sunburstList <- lapply(input$dbSelector, function(name) {
                try({
                  shiny::tagList(
                    shiny::h3(name),
                    htmlwidgets::onRender(
                      TreatmentPatterns::createSunburstPlot(
                        treatmentPathways = private$.reactiveValues$filteredData$treatmentPathways %>%
                          dplyr::filter(.data$db == name),
                        groupCombinations = input[[private$.groupCombiOption]],
                        colors = private$.reactiveValues$filteredData$labels,
                        legend = list(w = 400),
                        withD3 = TRUE
                      ),
                      jsCode = private$.jsShowLegend
                    )
                  )
                })
              })
              output$sunburst <- shiny::renderUI(shiny::tagList(sunburstList))
            }
          }
        })
    }
  )
)
