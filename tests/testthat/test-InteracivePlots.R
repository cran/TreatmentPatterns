library(testthat)
library(R6)
library(shiny)
library(dplyr)
library(TreatmentPatterns)

test_that("InteracivePlots", {
  interactivePlots <- InteractivePlots$new("app")
  expect_true(is.R6(interactivePlots))
})

test_that("UI", {
  interactivePlots <- InteractivePlots$new("app")

  expect_s3_class(interactivePlots$uiBody(), "shiny.tag")
  expect_s3_class(interactivePlots$uiMenu(), "shiny.tag")
})


test_that("server: inputs", {
  moduleInteractivePlots <- function(id, inputHandler, interactivePlots) {
    moduleServer(id, function(input, output, session) {
      inputHandler$setDataPath(input = input, path = NULL)
      
      path <- system.file(package = "TreatmentPatterns", "DummyOutput", "output.zip")
      
      session$setInputs(
        uploadField = list(
          datapath = path,
          name = "output.zip"
        )
      )
      
      inputHandler$server(input, output, session)
      interactivePlots$server(input, output, session, inputHandler)
      
      uiOutput(NS(id, "sunburst"))
      uiOutput(NS(id, "sankey"))
    })
  }
  
  testServer(
    app = moduleInteractivePlots,
    args = list(
      inputHandler = InputHandler$new("app"),
      interactivePlots = InteractivePlots$new("app")), {
        # Regular inputs
        session$setInputs(
          dbSelector = "output.zip",
          sexOption = "all",
          ageOption = "all",
          indexYearOption = "all",
          noneOption = TRUE
        )
        expect_s3_class(output$sunburst$html, "html")
        expect_s3_class(output$sankey$html, "html")
        
        # Sex / Age / IndexYear = NULL
        session$setInputs(
          dbSelector = "output.zip",
          sexOption = NULL,
          ageOption = NULL,
          indexYearOption = NULL,
          noneOption = TRUE
        )
        
        expect_s3_class(output$sunburst$html, "html")
        expect_s3_class(output$sankey$html, "html")
    }
  )
})
