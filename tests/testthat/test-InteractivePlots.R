# library(testthat)
# library(R6)
# library(shiny)
# library(dplyr)
# library(TreatmentPatterns)
# 
# test_that("InteracivePlots", {
#   skip_if_not(ableToRun()$plotting)
#   sunburst <- SunburstPlot$new("app")
#   sankey <- SankeyDiagram$new("app")
#   expect_true(is.R6(sunburst))
#   expect_true(is.R6(sankey))
# })
# 
# test_that("UI", {
#   skip_if_not(ableToRun()$plotting)
#   sunburst <- SunburstPlot$new("app")
#   sankey <- SankeyDiagram$new("app")
# 
#   expect_s3_class(sunburst$uiBody(), "shiny.tag")
#   expect_s3_class(sunburst$uiMenu(), "shiny.tag")
#   
#   expect_s3_class(sankey$uiBody(), "shiny.tag")
#   expect_s3_class(sankey$uiMenu(), "shiny.tag")
# })
# 
# test_that("server: inputs", {
#   skip_if_not(ableToRun()$plotting)
#   moduleInteractivePlots <- function(id, inputHandler, sunburst, sankey) {
#     moduleServer(id, function(input, output, session) {
#       inputHandler$setDataPath(input = input, path = NULL)
#       
#       path <- system.file(package = "TreatmentPatterns", "DummyOutput", "output.zip")
#       
#       session$setInputs(
#         uploadField = list(
#           datapath = path,
#           name = "output.zip"
#         ),
# 
#         noneOptionSunburstPlot = TRUE,
#         ageOptionSunburstPlot = "all",
#         sexOptionSunburstPlot = "all",
#         indexYearOptionSunburstPlot = "all",
# 
#         noneOptionSankeyDiagram = TRUE,
#         ageOptionSankeyDiagram = "all",
#         sexOptionSankeyDiagram = "all",
#         indexYearOptionSankeyDiagram = "all"
#       )
#       
#       inputHandler$server(input, output, session)
#       sunburst$server(input, output, session, inputHandler)
#       sankey$server(input, output, session, inputHandler)
#       
#       uiOutput(NS(id, "sunburst"))
#       uiOutput(NS(id, "sankey"))
#     })
#   }
#   
#   testServer(
#     app = moduleInteractivePlots,
#     args = list(
#       inputHandler = InputHandler$new("app"),
#       sunburst = SunburstPlot$new("app"),
#       sankey = SankeyDiagram$new("app")), {
#         # Regular inputs
#         session$setInputs(
#           dbSelector = "output.zip",
#           sexOption = "all",
#           ageOption = "all",
#           indexYearOption = "all",
#           noneOption = TRUE
#         )
# 
#         expect_s3_class(output$sunburst$html, "html")
#         expect_s3_class(output$sankey$html, "html")
# 
#         # Sex / Age / IndexYear = NULL
#         session$setInputs(
#           dbSelector = "output.zip",
#           sexOption = NULL,
#           ageOption = NULL,
#           indexYearOption = NULL,
#           noneOption = TRUE
#         )
# 
#         expect_s3_class(output$sunburst$html, "html")
#         expect_s3_class(output$sankey$html, "html")
#     }
#   )
# })
