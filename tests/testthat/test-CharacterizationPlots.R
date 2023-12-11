library(testthat)
library(R6)
library(shiny)
library(TreatmentPatterns)

test_that("CharacterizationPlots", {
  characterizationPlots <- CharacterizationPlots$new("app")
  expect_true(is.R6(characterizationPlots))
})

test_that("UI", {
  characterizationPlots <- CharacterizationPlots$new("app")
  
  expect_s3_class(characterizationPlots$uiBody(), "shiny.tag")
  expect_s3_class(characterizationPlots$uiMenu(), "shiny.tag")
})


test_that("server", {
  moduleInteractivePlots <- function(id, inputHandler, characterizationPlots) {
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
      characterizationPlots$server(input, output, session, inputHandler)
      
      uiOutput(NS(id, "charAgePlot"))
      uiOutput(NS(id, "charSexPlot"))
      uiOutput(NS(id, "charIndexYearPlot"))
    })
  }
  
  testServer(
    app = moduleInteractivePlots,
    args = list(
      inputHandler = InputHandler$new("app"),
      characterizationPlots = CharacterizationPlots$new("app")), {
        expect_s3_class(output$charAgePlot$html, "html")
        expect_s3_class(output$charSexPlot$html, "html")
        expect_s3_class(output$charIndexYearPlot$html, "html")
      }
  )
})
