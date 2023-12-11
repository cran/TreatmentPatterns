#' @title InteractivePlots
#' 
#' @description
#' Class to handle the interactive plots of TreatmentPatterns (Sunburst plot &
#' Sankey diagram)
#' 
#' @export
InteractivePlots <- R6::R6Class(
  classname = "InteractivePlots",
  inherit = Module,
  
  # Public ----
  public = list(
    #' @description
    #' Method to include a \link[shinydashboard]{menuItem} to link to the body.
    #'
    #' @param label (`character(1)`)\cr
    #' Label to show for the `menuItem`.
    #' 
    #' @param tag (`character(1)`)\cr
    #' Tag to use internally in `input`.
    #' 
    #' @return (`menuItem`)
    uiMenu = function(label = "Plots", tag = "plots") {
      shinydashboard::menuItem(
        text = label,
        tabName = tag,
        icon = shiny::icon(lib = "glyphicon", name = "stats")
      )
    },

    #' @description
    #' Method to include a \link[shinydashboard]{tabItem} to include the body.
    #'
    #' @return (`tabItem`)
    uiBody = function() {
      shinydashboard::tabItem(
        tabName = "plots",
        shiny::fluidRow(
          shinydashboard::box(
            width = "100%",
            shiny::column(width = 1, shiny::uiOutput(shiny::NS(private$.namespace, "sexOption"))),
            shiny::column(width = 1, shiny::uiOutput(shiny::NS(private$.namespace, "ageOption"))),
            shiny::column(width = 1, shiny::uiOutput(shiny::NS(private$.namespace, "indexYearOption"))),
            shiny::column(width = 5, shiny::checkboxInput(shiny::NS(private$.namespace, "noneOption"), label = "Exclude empty pathways"))
          )
        ),
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            title = "Sankey Diagram",
            private$plotSankey()
          ),
          shiny::tabPanel(
            title = "Sunburst Plot",
            private$plotSunburst()
          )
        )
      )
    },

    #' @description
    #' Method to handle the back-end.
    #'
    #' @param input (`input`)\cr
    #' Input from the server function.
    #'
    #' @param output (`output`)\cr
    #' Output from the server function.
    #'
    #' @param session (`session`)\cr
    #' Session from the server function.
    #' 
    #' @param inputHandler (`inputHandler`)\cr
    #' \link[TreatmentPatterns]{InputHandler} class.
    #' 
    #' @return (`NULL`)
    server = function(input, output, session, inputHandler) {
      private$setSexOptions(output, session, inputHandler)
      private$setAgeOptions(output, session, inputHandler)
      private$setIndexYearOptions(output, session, inputHandler)
      private$computeSankey(input, output, inputHandler)
      private$computeSunburst(input, output, inputHandler)
    }
  ),
  
  # Private ----
  private = list(
    ## Methods ----
    plotSankey = function() {
      shiny::tagList(
        shiny::htmlOutput(shiny::NS(private$.namespace, "sankey"))
      )
    },
    
    plotSunburst = function() {
      shiny::tagList(
        shiny::htmlOutput(shiny::NS(private$.namespace, "sunburst"))
      )
    },
    
    checkInputOption = function(option) {
      if (is.null(option)) {
        "all"
      } else {
        option
      }
    },
    
    filterData = function(data, input) {
      none <- if (input$noneOption) {
        "None"
      } else {
        ""
      }
      
      data %>%
        dplyr::filter(.data$age == private$checkInputOption(input$ageOption)) %>%
        dplyr::filter(.data$sex == private$checkInputOption(input$sexOption)) %>%
        dplyr::filter(.data$indexYear == private$checkInputOption(input$indexYearOption)) %>%
        dplyr::filter(.data$path != none) %>%
        dplyr::filter(.data$db %in% input$dbSelector)
    },
    
    getLabelColors = function(data) {
      colorPallete <- c(
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
        "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
        "#1fff7f", "#ffff7f", "#2fcfaf", "#df6f2f", "#9f4f6f",
        "#8fcf5f", "#ef3f7f", "#7fff7f", "#bfcfbf", "#1f7fbf",
        "#7fbf4f", "#ff0fef", "#0f2fcf", "#7f2f8f", "#7fbfdf",
        "#6f4fbf", "#7fcf2f", "#ff7fff", "#df2f2f", "#efcfff"
      )

      labels <- data %>%
        dplyr::pull("path") %>%
        unique()

      range <- if (!is.null(labels)) {
        colorPallete[seq_len(length(labels))]
      }
      
      return(list(
        range = range,
        domain = labels
      ))
    },
    
    formatData = function(inputHandler, input) {
      treatmentPathways <- inputHandler$reactiveValues$treatmentPathways %>%
        private$filterData(input = input)
      labels <- private$getLabelColors(treatmentPathways)
      return(list(treatmentPathways = treatmentPathways, labels = labels))
    },
    
    computeSankey = function(input, output, inputHandler) {
      shiny::observeEvent(
        eventExpr = list(
          input$dbSelector,
          input$ageOption,
          input$sexOption,
          input$indexYearOption,
          input$noneOption),
        handlerExpr = {
          if (!is.null(inputHandler$reactiveValues$treatmentPathways)) {
            if (nrow(inputHandler$reactiveValues$treatmentPathways) > 0) {
              data <- private$formatData(
                inputHandler = inputHandler,
                input = input
              )
              sankeyList <- lapply(input$dbSelector, function(name) {
                try({
                  shiny::tagList(
                    shiny::h3(name),
                    TreatmentPatterns::createSankeyDiagram2(
                      treatmentPathways = data$treatmentPathways %>%
                        dplyr::filter(.data$db == name)
                    )
                  )
                })
              })
              output$sankey <- shiny::renderUI(shiny::tagList(sankeyList))
            }
          }
        })
    },
    
    computeSunburst = function(input, output, inputHandler) {
      shiny::observeEvent(
        eventExpr = list(
          input$dbSelector,
          input$ageOption,
          input$sexOption,
          input$indexYearOption,
          input$noneOption),
        handlerExpr = {
        if (!is.null(inputHandler$reactiveValues$treatmentPathways)) {
          if (nrow(inputHandler$reactiveValues$treatmentPathways) > 0) {
            data <- private$formatData(
              inputHandler = inputHandler,
              input = input
            )
            sunburstList <- lapply(input$dbSelector, function(name) {
              try({
                shiny::tagList(
                  shiny::h3(name),
                  TreatmentPatterns::createSunburstPlot2(
                    treatmentPathways = data$treatmentPathways %>%
                      dplyr::filter(.data$db == name),
                    colors = data$labels,
                    legend = list(w = 400)
                  )
                )
              })
            })
            output$sunburst <- shiny::renderUI(shiny::tagList(sunburstList))
          }
        }
      })
    },
    
    setSexOptions = function(output, session, inputHandler) {
      shiny::observeEvent(inputHandler$reactiveValues$treatmentPathways, {
        output$sexOption <- renderUI({
          shiny::selectInput(
            inputId = session$ns("sexOption"),
            label = "Sex",
            choices = unique(inputHandler$reactiveValues$treatmentPathways$sex),
            selected = "all"
          )
        })
      })
    },
    
    setAgeOptions = function(output, session, inputHandler) {
      shiny::observeEvent(inputHandler$reactiveValues$treatmentPathways, {
        output$ageOption <- renderUI({
          shiny::selectInput(
            inputId = session$ns("ageOption"),
            label = "Age",
            choices = unique(inputHandler$reactiveValues$treatmentPathways$age),
            selected = "all"
          )
        })
      })
    },
    
    setIndexYearOptions = function(output, session, inputHandler) {
      shiny::observeEvent(inputHandler$reactiveValues$treatmentPathways, {
        output$indexYearOption <- renderUI({
          shiny::selectInput(
            inputId = session$ns("indexYearOption"),
            label = "Index Year",
            choices = unique(inputHandler$reactiveValues$treatmentPathways$indexYear),
            selected = "all"
          )
        })
      })
    }
  )
)
