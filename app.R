#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(MethylPipeR)
library(performance)
library(see)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    theme = bs_theme(bootswatch = 'superhero'),
    
    titlePanel("MethylPipeR"),

    sidebarLayout(
        sidebarPanel(
            fileInput('trainXs', 'Upload .rds file. Training data matrix/data.frame.',
                      multiple = FALSE, accept = c('.rds')),
            fileInput('trainY', 'Upload .rds file. Training response vector.',
                      multiple = FALSE, accept = c('.rds')),
            fileInput('testXs', 'Upload .rds file. Test data matrix/data.frame.',
                      multiple = FALSE, accept = c('.rds')),
            fileInput('testY', 'Upload .rds file. Test response vector.',
                      multiple = FALSE, accept = c('.rds')),
            checkboxInput('cvCheck', 'Use cross-validation for training set model fitting.'),
            checkboxInput('incrementalCheck', 'Fit incremental model'),
            fileInput('incrementalCovariates', 'Upload .rds or .csv file. Covariates matrix/data.frame for incremental model.',
                      multiple = FALSE, accept = c('.rds')),
            selectInput('modelType', label = 'Select model type (binary/survival/continuous)', choices = c('binary', 'survival', 'continuous')),
            textInput('n_years', label = 'Value of n for n-year risk prediction', value = '10'),
            selectInput('modelMethod', label = 'Select model method (glmnet/bart/rf)', choices = c('glmnet', 'bart', 'rf')),
            actionButton('run', 'Run'),
            shinyjs::hidden(p(id = 'processingText', 'Fitting model...'))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel('Diagnostics', plotOutput('diagnostics', height = '800px')),
                tabPanel('Performance', verbatimTextOutput('console', placeholder = FALSE)),
                tabPanel('Train + Test Performance', verbatimTextOutput('train_test_performance', placeholder = FALSE))
            )
        )
    )
)

server <- function(input, output) {
    
    # Dynamically show or hide text box for specifying n for n-year risk prediction (survival model only)
    observeEvent(input$modelType, {
        if (input$modelType == 'survival') {
            shinyjs::show('n_years')
        } else {
            shinyjs::hide('n_years')
        }
    })
    
    glmFamilyLookup <- list('binary' = 'binomial', 'continuous' = 'gaussian', 'survival' = 'cox')
    
    disableInput <- function() {
        shinyjs::disable('run')
        shinyjs::disable('trainXs')
        shinyjs::disable('trainY')
        shinyjs::disable('testXs')
        shinyjs::disable('testY')
        shinyjs::disable('incrementalCovariates')
        shinyjs::show('processingText')
    }
    
    enableInput <- function() {
        shinyjs::enable('run')
        shinyjs::enable('trainXs')
        shinyjs::enable('trainY')
        shinyjs::enable('testXs')
        shinyjs::enable('testY')
        shinyjs::enable('incrementalCovariates')
        shinyjs::hide('processingText')
    }
    
    mprModel <- reactiveVal(NULL)
    modelReady <- reactiveVal(FALSE)
    testPredictions <- reactiveVal(NULL)
    incrementalModel <- reactiveVal(NULL)
    
    trainXsDF <- reactive({
        req(input$trainXs)
        df <- readRDS(input$trainXs$datapath)
        df
    })
    
    trainYDF <- reactive({
        req(input$trainY)
        df <- readRDS(input$trainY$datapath)
        df
    })
    
    testXsDF <- reactive({
        req(input$testXs)
        df <- readRDS(input$testXs$datapath)
        df
    })
    
    testYDF <- reactive({
        req(input$testY)
        df <- readRDS(input$testY$datapath)
        df
    })
    
    incrementalCovariatesDF <- reactive({
        req(input$incrementalCovariates)
        df <- readRDS(input$incrementalCovariates$datapath)
        df
    })
    
    observeEvent(input$run, {
        disableInput()
        trainXs <- trainXsDF()
        trainY <- trainYDF()
        if (is.null(input$testXs)) {
            testXs <- trainXs
        } else {
            testXs <- testXsDF()
        }
        if (is.null(input$testY)) {
            testY <- trainY
        } else {
            testY <- testYDF()
        }
        if (isolate(input$cvCheck)) {
            mprModel(fitMPRModelCV(type = input$modelType, method = input$modelMethod, trainXs = trainXs, trainY = trainY, alpha = 0))
        } else {
            mprModel(fitMPRModel(type = input$modelType, method = input$modelMethod, trainXs = trainXs, trainY = trainY, alpha = 0))
        }
        if (isolate(input$incrementalCheck)) {
            # browser()
            model <- isolate(mprModel())
            # browser()
            # This will need to be adapted to reflect whether the model actually used CV or not rather than relying on the tickbox
            # It will also need to be adapted for other model methods and types
            if (isolate(input$cvCheck)) {
                testPredictions(predictMPRModel(model, testXs, s = 'lambda.min', type = 'link'))
            } else {
                testPredictions(predictMPRModel(model, testXs, s = model$model$lambda[[1]], type = 'link'))
            }
            # browser()
            incrementalXs <- data.frame(isolate(incrementalCovariatesDF()))
            covColnames <- colnames(incrementalXs)
            score <- isolate(testPredictions())
            incrementalXs$score <- score
            incrementalXs$y <- isolate(testY)
            incrementalModel(fitMPRModelIncremental(incrementalXs, yColname = 'y', covColnames = covColnames, scoreColname = 'score', family = glmFamilyLookup[[input$modelType]]))
        } 
        modelReady(TRUE)
    })
    
    output$console <- renderPrint({
        if (modelReady()) {
            enableInput()
            modelReady(FALSE)
        }
        # summary(mprModel()$model)
        req(incrementalModel())
        model <- isolate(incrementalModel())
        
        # print(model_performance(model$null$model))
        # print()
        # print(model_performance(model$full$model))
        compare_performance(model$full$model, model$null$model)
    })
    
    output$diagnostics <- renderPlot({
        if (modelReady()) {
            enableInput()
            modelReady(FALSE)
        }
        # browser()
        req(incrementalModel())
        incrementalModelResult <- incrementalModel()
        check_model(incrementalModelResult$full$model)
        # plot(compare_performance(incrementalModelResult$full$model, incrementModelResult$null$model))
    })
    
    output$train_test_performance <- renderPrint({
        if (modelReady()) {
            enableInput()
            modelReady(FALSE)
        }
        req(mprModel())
        print(summary(isolate(mprModel()$model)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
