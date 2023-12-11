library(TreatmentPatterns)
library(testthat)
library(R6)

test_that("InputHandler", {
  ## new() / initialize() ----
  inputHandler <- InputHandler$new("app")
  expect_true(is.R6(inputHandler))
  
  ## validate() ----
  expect_true(is.R6(inputHandler$validate()))
  
  ## Fields ----
  expect_identical(inputHandler$namespace, "app")
  expect_s3_class(inputHandler$reactiveValues, "reactivevalues")
})

test_that("UI", {
  inputHandler <- InputHandler$new("app")
  expect_s3_class(inputHandler$uiMenu(), "shiny.tag")
  expect_s3_class(inputHandler$uiBody(), "shiny.tag")
  expect_s3_class(inputHandler$uiDatabaseSelector(), "shiny.tag")
})

test_that("InputHandler: setDataPath()", {
  moduleInputHandler <- function(id, inputHandler) {
    shiny::moduleServer(id, function(input, output, session) {})
  }
  
  shiny::testServer(
    app = moduleInputHandler,
    args = list(inputHandler = InputHandler$new("app")), {
      # input = NULL, path = NULL
      expect_error(
        inputHandler$setDataPath(input = NULL, path = NULL),
        "Cannot assert where data is comming from."
      )
      
      # input = input, path = NULL
      inputHandler$setDataPath(input = input, path = NULL)
      
      path <- system.file(package = "TreatmentPatterns", "DummyOutput", "output.zip")
      
      session$setInputs(
        uploadField = list(
          datapath = path,
          name = "output.zip"
        )
      )
      
      expect_identical(inputHandler$reactiveValues$dataPath, path)
      
      # input = NULL, path = path
      inputHandler$setDataPath(input = NULL, path = path)
      expect_identical(inputHandler$reactiveValues$dataPath, path)
    }
  )
})

test_that("InputHandler: server()", {
  moduleInputHandler <- function(id, inputHandler) {
    shiny::moduleServer(id, function(input, output, session) {
      inputHandler$setDataPath(input = input, path = NULL)
      inputHandler$server(input, output, session)
    })
  }
  
  shiny::testServer(
    app = moduleInputHandler,
    args = list(inputHandler = InputHandler$new("app")), {
      path <- system.file(package = "TreatmentPatterns", "DummyOutput", "output.zip")
      
      session$setInputs(
        uploadField = list(
          datapath = path,
          name = "output.zip"
        )
      )
      
      # Files
      expect_s3_class(inputHandler$reactiveValues$treatmentPathways, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$countsAge, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$countsSex, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$countsYear, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$summaryStatsTherapyDuration, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$metadata, "data.frame")
      
      # Fields
      expect_identical(inputHandler$reactiveValues$dataPath, path)
      expect_identical(inputHandler$reactiveValues$dbNames, basename(path))
    }
  )
})
