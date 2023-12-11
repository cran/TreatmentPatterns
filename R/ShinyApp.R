#' @title ShinyApp
#' 
#' @description
#' R6 ShinyApp class.
#' 
#' @field namespace Namespace of the shiny application.
#' @field inputHandler R6 InputHandler.
#' @field interactivePlots R6 InteractivePlots (Sunburst and Sankey).
#' @field characterizationPlots R6 CharacterizationPlots.
#' 
#' @noRd
ShinyApp <- R6::R6Class(
  classname = "ShinyApp",
  
  # Public ----
  public = list(
    #' @description
    #' Initializer method.
    #'
    #' @param namespace (`character(1)`)
    #'
    #' @return (`invisible(self)`)
    initialize = function(namespace) {
      private$.namespace <- namespace
      private$.inputHandler <- TreatmentPatterns::InputHandler$new(private$.namespace)
      private$.interactivePlots <- TreatmentPatterns::InteractivePlots$new(private$.namespace)
      private$.characterizationPlots <- TreatmentPatterns::CharacterizationPlots$new(private$.namespace)
      return(invisible(self))
    },
    
    #' @description
    #' UI method.
    #' 
    #' @return (`shinyDashboard`)
    ui = function() {
      shinydashboard::dashboardPage(
        self$uiHeader(),
        self$uiMenu(),
        self$uiBody()
      )
    },
    
    #' @description
    #' Header method for the UI.
    #' 
    #' @return (`dashboardHeader`)
    uiHeader = function() {
      shinydashboard::dashboardHeader(title = "Results Explorer")
    },
    
    #' @description
    #' Menu method for the UI.
    #' 
    #' @return (`dashboardSidebar`)
    uiMenu = function() {
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          private$.inputHandler$uiMenu(),
          private$.interactivePlots$uiMenu(),
          private$.characterizationPlots$uiMenu()
        ),
        private$.inputHandler$uiDatabaseSelector()
      )
    },
    
    #' @description
    #' Body method for the UI.
    #' 
    #' @return (`dashboardBody`)
    uiBody = function() {
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          private$.inputHandler$uiBody(),
          private$.interactivePlots$uiBody(),
          private$.characterizationPlots$uiBody()
        )
      )
    },
    
    #' @description
    #' Server method for the backend.
    #' 
    #' @return (`dashboardHeader`)
    server = function(input, output, session) {
      shiny::moduleServer(private$.namespace, function(input, output, session) {
        private$.inputHandler$setDataPath(tag = "uploadField", input = input, path = NULL)
        private$.inputHandler$server(input, output, session)
        private$.interactivePlots$server(input, output, session, private$.inputHandler)
        private$.characterizationPlots$server(input, output, session, private$.inputHandler)
      })
    }
  ),
  
  # Private ----
  private = list(
    ## Fields ----
    .namespace = "",
    .inputHandler = NULL,
    .interactivePlots = NULL,
    .characterizationPlots = NULL,
    
    ## Methods ----
    finalize = function() {}
  ),
  
  # Active ----
  active = list(
    namespace = function() return(private$.namespace),
    inputHandler = function() return(private$.inputHandler),
    interactivePlots = function() return(private$.interactivePlots),
    characterizationPlots = function() return(private$.characterizationPlots)
  )
)


#' launchResultsExplorer
#'
#' Launches the ResultExplorer shinyApp.
#'
#' @return (`shinyApp`)
#'
#' @export
#' 
#' @examples
#' if (interactive()) {
#'   launchResultsExplorer()
#' }
launchResultsExplorer <- function() {
  app <- ShinyApp$new("app")
  shiny::shinyApp(app$ui, app$server)
}
