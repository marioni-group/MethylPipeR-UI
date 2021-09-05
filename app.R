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
library(shinyBS)
library(prompter)

source('user_parameter_handling.R')

# Increase max file upload size to 30 GB
options(shiny.maxRequestSize = 30 * 1024^3)

ui <- fluidPage(
    shinyjs::useShinyjs(),
    theme = bs_theme(bootswatch = 'superhero'),
    
    use_prompt(),
    
    titlePanel("MethylPipeR"),

    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel('Data upload and model specification',
                    add_prompt(fileInput('trainXs', 'Upload rds/csv file. Training data matrix/data.frame.',
                                         multiple = FALSE, accept = c('.rds', '.csv')), 
                               position = 'right', message = 'Rows should correspond to individuals in the dataset and columns should correspond to features.'),
                    # bsButton('trainXsHelp', label = '', icon = icon('question'), style = 'info', size = 'extra-small'),
                    # bsTooltip('trainXs', 'Rows should correspond to individuals in the dataset and columns should correspond to features.', 
                    #           placement = 'right', trigger = 'hover', options = list(container = 'body')),
                    fileInput('trainY', 'Upload rds/csv file. Training response vector/matrix/data.frame. Event/response column name must be specified if uploading a matrix/data.frame.',
                              multiple = FALSE, accept = c('.rds', '.csv')),
                    add_prompt(fileInput('testXs', 'Upload rds/csv file. Test data matrix/data.frame.',
                                         multiple = FALSE, accept = c('.rds', '.csv')),
                               position = 'right', message = 'Rows should correspond to individuals in the dataset and columns should correspond to features. The columns in the test data should match the columns in the training data.'),
                    fileInput('testY', 'Upload rds/csv file. Test response vector/matrix/data.frame. Event/response column name must be specified if uploading a matrix/data.frame',
                              multiple = FALSE, accept = c('.rds', '.csv')),
                    checkboxInput('cvCheck', 'Use cross-validation for training set model fitting.'),
                    checkboxInput('incrementalCheck', 'Fit incremental model'),
                    fileInput('incrementalCovariates', 'Upload rds/csv file. Covariates matrix/data.frame for incremental model.',
                              multiple = FALSE, accept = c('.rds', '.csv')),
                    selectInput('modelType', label = 'Select model type (binary/survival/continuous)', choices = c('binary', 'survival', 'continuous')),
                    textInput('n_years', label = 'Value of n for n-year risk prediction', value = '10'),
                    textInput('tte_colname', label = 'Time-to-event column name in Y table', value = 'time_to_event'),
                    textInput('event_colname', label = 'Event/response column name in Y matrix/data.frame. Required if Ys is uploaded as a matrix/data.frame', value = 'Event'),
                    selectInput('modelMethod', label = 'Select model method (glmnet/bart/rf)', choices = c('glmnet', 'bart', 'rf')),
                    actionButton('run', 'Run'),
                    shinyjs::hidden(p(id = 'processingText', 'Fitting model...'))
                ),
                tabPanel('Parameters',
                    textAreaInput('model_fitting_parameters', 'Input model fitting parameters, one per line in the format <parameter> = <value>', rows = 3),
                    textAreaInput('prediction_parameters', 'Input prediction function parameters, one per line in the format <parameter> = <value>', rows = 3)),
                tabPanel('Preprocessing',
                         checkboxInput('tte_threshold_train', 'Perform time-to-event thresholding on training set'),
                         checkboxInput('tte_threshold_test', 'Perform time-to-event thresholding on test set'),
                         textInput('tte_threshold', label = 'Time-to-event threshold', value = '10'))
            )
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

server <- function(input, output, session) {
    # Dynamically show or hide text box for specifying n for n-year risk prediction (survival model only)
    observeEvent(input$modelType, {
        if (input$modelType == 'survival') {
            shinyjs::show('n_years')
            # shinyjs::show('tte_colname')
            # shinyjs::show('event_colname')
        } else {
            shinyjs::hide('n_years')
            # shinyjs::hide('tte_colname')
            # shinyjs::hide('event_colname')
        }
    })
    
    observeEvent(input$modelMethod, {
        updateTextAreaInput(session, 'model_fitting_parameters', value = getModelFittingParameterDefaults(input$modelMethod, input$cvCheck))
        updateTextAreaInput(session, 'prediction_parameters', value = getPredictionParameterDefaults(input$modelMethod))
    })
    observeEvent(input$cvCheck, {
        updateTextAreaInput(session, 'model_fitting_parameters', value = getModelFittingParameterDefaults(input$modelMethod, input$cvCheck))
        updateTextAreaInput(session, 'prediction_parameters', value = getPredictionParameterDefaults(input$modelMethod))
    })
    
    glmFamilyLookup <- list('binary' = 'binomial', 'continuous' = 'gaussian', 'survival' = 'binomial')
    
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
        extension <- tools::file_ext(input$trainXs$datapath)
        
        if (extension == 'csv') {
            df <- read.csv(input$trainXs$datapath)
        } else if (extension == 'rds') {
            df <- readRDS(input$trainXs$datapath)
        }
        df
    })
    
    trainYDF <- reactive({
        # browser()
        req(input$trainY)
        extension <- tools::file_ext(input$trainY$datapath)
        
        if (extension == 'csv') {
            df <- read.csv(input$trainY$datapath)
        } else if (extension == 'rds') {
            df <- readRDS(input$trainY$datapath)
        }
        df
    })
    
    testXsDF <- reactive({
        req(input$testXs)
        extension <- tools::file_ext(input$testXs$datapath)
        
        if (extension == 'csv') {
            df <- read.csv(input$testXs$datapath)
        } else if (extension == 'rds') {
            df <- readRDS(input$testXs$datapath)
        }
        df
    })
    
    testYDF <- reactive({
        req(input$testY)
        extension <- tools::file_ext(input$testY$datapath)
        
        if (extension == 'csv') {
            df <- read.csv(input$testY$datapath)
        } else if (extension == 'rds') {
            df <- readRDS(input$testY$datapath)
        }
        df
    })
    
    incrementalCovariatesDF <- reactive({
        req(input$incrementalCovariates)
        extension <- tools::file_ext(input$incrementalCovariates$datapath)
        
        if (extension == 'csv') {
            df <- read.csv(input$incrementalCovariates$datapath)
        } else if (extension == 'rds') {
            df <- readRDS(input$incrementalCovariates$datapath)
        }
        df
    })
    
    modelFittingParameters <- reactive({
        # browser()
        input$model_fitting_parameters
    })
    
    predictionParameters <- reactive({
        input$prediction_parameters
    })
    
    observeEvent(input$run, {
        # For testing:
        # browser()
        fitFunctionParameters <- processParameterInput(modelFittingParameters())
        predictFunctionParameters <- processParameterInput(predictionParameters())
        
        disableInput()
        trainXs <- trainXsDF()
        trainY <- trainYDF()
        incrementalXs <- NULL
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
        
        # Make sure trainXs and testXs have column names in the same order
        # browser()
        trainXsColnames <- colnames(trainXs)
        if (!is.null(trainXsColnames)) {
            testXs <- testXs[, trainXsColnames]
        }
        # NOTE: if colnames(trainXs) is NULL, columns are assumed to align between trainXs and testXs.
        # TODO: check number of columns are consistent across trainXs and testXs
        
        # browser()
        eventColname <- input$event_colname
        tteColname <- input$tte_colname
        
        # If trainY is a vector, convert to a data.frame with a single column for consistency across model types
        if (is.vector(trainY)) {
            trainY <- data.frame(trainY)
            colnames(trainY) <- c(eventColname)
        }
        # Same for testY
        if (is.vector(testY)) {
            testY <- data.frame(testY)
            colnames(testY) <- c(eventColname)
        }
        
        if (input$tte_threshold_train) {
            # browser()
            tteThresholdResult <- thresholdTTE(trainY, objectsToFilter = list('trainXs' = trainXs), threshold = as.numeric(input$tte_threshold), eventColname = eventColname, tteColname = tteColname)
            trainY <- tteThresholdResult$targetFiltered
            trainXs <- tteThresholdResult$objectsFiltered$trainXs
            tteThresholdResult <- NULL
        }
        
        if (input$tte_threshold_test) {
            if (input$incrementalCheck) {
                tteThresholdResult <- thresholdTTE(testY, objectsToFilter = list('testXs' = testXs, 'covariates' = as.data.frame(incrementalCovariatesDF())), threshold = as.numeric(input$tte_threshold),
                                                   eventColname = eventColname, tteColname = tteColname)
                incrementalXs <- tteThresholdResult$objectsFiltered$covariates
            } else {
                tteThresholdResult <- thresholdTTE(testY, objectsToFilter = list('testXs' = testXs), threshold = as.numeric(input$tte_threshold), eventColname = eventColname, tteColname = tteColname)
            }
            testY <- tteThresholdResult$targetFiltered
            testXs <- tteThresholdResult$objectsFiltered$testXs
            tteThresholdResult <- NULL
        }
        
        addFitMPRModelParameters <- function(ps) {
            ps$type <- input$modelType
            ps$method <- input$modelMethod
            ps$trainXs <- trainXs
            if (input$modelType == 'survival') {
                ps$trainY <- trainY
            } else {
                ps$trainY <- trainY[, eventColname]
            }
            ps$tteColname <- input$tte_colname
            ps$eventColname = input$event_colname
            ps
        }
        # browser()
        fitFunctionParameters <- addFitMPRModelParameters(fitFunctionParameters)
        if (input$cvCheck) {
            fitMPRModelResult <- do.call(fitMPRModelCV, fitFunctionParameters)
        } else {
            fitMPRModelResult <- do.call(fitMPRModel, fitFunctionParameters)
        }
        mprModel(fitMPRModelResult)
        
        
        
        if (input$incrementalCheck) {
            # browser()
            model <- mprModel()
            
            addPredictMPRModelParameters <- function(ps) {
                ps$model <- model
                ps$data <- testXs
                ps
            }
            # browser()
            # This will need to be adapted to reflect whether the model actually used CV or not rather than relying on the tickbox
            # It will also need to be adapted for other model methods and types
            
            predictFunctionParameters <- addPredictMPRModelParameters(predictFunctionParameters)
            predictMPRModelResult <- do.call(predictMPRModel, predictFunctionParameters)
            testPredictions(predictMPRModelResult)
            # if (input$cvCheck) {
            #     # testPredictions(predictMPRModel(model, testXs, s = 'lambda.min', type = 'link'))
            #     
            # } else {
            #     # if (input$modelMethod == 'glmnet') {
            #     #     testPredictions(predictMPRModel(model, testXs, s = model$model$lambda[[1]], type = 'link'))
            #     # } else if (input$modelMethod == 'bart') {
            #     #     testPredictions(predictMPRModel(model, testXs))
            #     # }
            # }
            # browser()
            if (is.null(incrementalXs)) {
                incrementalXs <- data.frame(incrementalCovariatesDF())
            }
            covColnames <- colnames(incrementalXs)
            score <- testPredictions()
            incrementalXs$score <- score
            incrementalXs$y <- testY[, eventColname]
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
        print('Full model (score and covariates) summary:')
        print(summary(model$full$model))
        print('Null model (covariates only) summary:')
        print(summary(model$null$model))
        print(compare_performance(model$full$model, model$null$model))
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
