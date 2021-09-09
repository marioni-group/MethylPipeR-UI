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

sessionStartTimestamp <- format(Sys.time(), '%Y_%m_%d_%H_%M_%S')
sessionLogFolder <- 'C:/Users/s2092119/Documents/PhD/Omics Prediction of Incident Disease/R Package/MethylPipeR-UI_logs/'
sessionLogFileName <- paste0('session_log_', sessionStartTimestamp, '.txt')
sessionLogFilepath <- paste0(sessionLogFolder, sessionLogFileName)

sessionConsoleFilepath <- paste0(sessionLogFolder, 'console_log_', sessionStartTimestamp, '.txt')
sink(sessionConsoleFilepath)

sessionLogFile <- file(sessionLogFilepath)
writeLines(paste0('Starting MethylPipeR-UI session. Timestamp: ', sessionStartTimestamp), sessionLogFile)
close(sessionLogFile)

logLines <- function(...) {
    lineList <- list(...)
    lineList <- lapply(lineList, function(txt) {
        paste0(format(Sys.time(), '[%H:%M:%S] '), txt)
    })
    lineList$file = sessionLogFilepath
    lineList$sep = '\n'
    lineList$append = TRUE
    do.call(cat, lineList)
}

saveMPRModelObject <- function(model) {
    folderPath <- paste0(sessionLogFolder, 'models_', sessionStartTimestamp, '/')
    dir.create(paste0(folderPath))
    filePath <- paste0(folderPath, model$modelType, '_', model$modelMethod, '_', format(Sys.time(), '%Y_%m_%d_%H_%M_%S'), '.rds')
    saveRDS(model, file = filePath)
    logLines(paste0('Saved model in ', filePath))
}

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
                    add_prompt(fileInput('trainY', 'Upload rds/csv file. Training target vector/matrix/data.frame. Event/response column name must be specified if uploading a matrix/data.frame.',
                              multiple = FALSE, accept = c('.rds', '.csv')),
                               position = 'right', message = 'The length of the vector/number of rows in the matrix should be the same as the number of rows in the training data.'),
                    add_prompt(fileInput('testXs', 'Upload rds/csv file. Test data matrix/data.frame.',
                                         multiple = FALSE, accept = c('.rds', '.csv')),
                               position = 'right', message = 'Rows should correspond to individuals in the dataset and columns should correspond to features. The columns in the test data should match the columns in the training data.'),
                    add_prompt(fileInput('testY', 'Upload rds/csv file. Test target vector/matrix/data.frame. Event/response column name must be specified if uploading a matrix/data.frame',
                              multiple = FALSE, accept = c('.rds', '.csv')),
                               position = 'right', message = 'The length of the vector/number of rows in the matrix should be the same as the number of rows in the test data.'),
                    checkboxInput('cvCheck', 'Select hyperparameters using cross-validation for training set model fitting.'),
                    checkboxInput('incrementalCheck', 'Fit incremental model'),
                    fileInput('incrementalCovariates', 'Upload rds/csv file. Covariates matrix/data.frame for incremental model.',
                              multiple = FALSE, accept = c('.rds', '.csv')),
                    selectInput('modelType', label = 'Select model type (binary/survival/continuous)', choices = c('binary', 'survival', 'continuous')),
                    textInput('n_years', label = 'Value of n for n-year risk prediction', value = '10'),
                    textInput('tte_colname', label = 'Time-to-event column name in Y table', value = 'time_to_event'),
                    textInput('event_colname', label = 'Event/target column name in Y matrix/data.frame. Required if Ys is uploaded as a matrix/data.frame', value = 'Event'),
                    selectInput('modelMethod', label = 'Select model method (glmnet/bart/rf)', choices = c('glmnet', 'bart', 'rf')),
                    checkboxInput('save_model', 'Save model object to file'),
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
                tabPanel('Performance', verbatimTextOutput('performance', placeholder = FALSE)),
                tabPanel('Binary Model Metrics', plotOutput('binary_model_metrics', height = '500px'), plotOutput('roc', height = '500px')),
                tabPanel('Console', verbatimTextOutput('console', placeholder = FALSE))
            )
        )
    )
)

server <- function(input, output, session) {
    consoleFile <- reactiveFileReader(1000, session, sessionConsoleFilepath, readLines)
    
    # Dynamically show or hide text box for specifying n for n-year risk prediction (survival model only)
    observeEvent(input$modelType, {
        if (input$modelType == 'survival') {
            shinyjs::show('n_years')
        } else {
            shinyjs::hide('n_years')
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
        input$model_fitting_parameters
    })
    
    predictionParameters <- reactive({
        input$prediction_parameters
    })
    
    observeEvent(input$run, {
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
        trainXsColnames <- colnames(trainXs)
        if (!is.null(trainXsColnames)) {
            testXs <- testXs[, trainXsColnames]
        }
        # NOTE: if colnames(trainXs) is NULL, columns are assumed to align between trainXs and testXs.
        # TODO: check number of columns are consistent across trainXs and testXs
        
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
        logLines('Fitting MPRModel.',
                 paste0('Model type: ', input$modelType),
                 paste0('Model method: ', input$modelMethod),
                 paste0('Using cross-validation for hyperparameter selection: ', input$cvCheck),
                 'Additional fitMPRModel parameters:',
                 capture.output(print(fitFunctionParameters)))
        fitFunctionParameters <- addFitMPRModelParameters(fitFunctionParameters)
        beforeFitTime <- proc.time()
        if (input$cvCheck) {
            fitMPRModelResult <- do.call(fitMPRModelCV, fitFunctionParameters)
        } else {
            fitMPRModelResult <- do.call(fitMPRModel, fitFunctionParameters)
        }
        fitTime <- proc.time() - beforeFitTime
        logLines('MPRModel fitting time:',
                 capture.output(print(fitTime)))
        mprModel(fitMPRModelResult)
        
        if (input$save_model) {
            saveMPRModelObject(fitMPRModelResult)
        }
        
        
        if (input$incrementalCheck) {
            model <- mprModel()
            
            addPredictMPRModelParameters <- function(ps) {
                ps$model <- model
                ps$data <- testXs
                ps
            }
            
            predictFunctionParameters <- addPredictMPRModelParameters(predictFunctionParameters)
            predictMPRModelResult <- do.call(predictMPRModel, predictFunctionParameters)
            testPredictions(predictMPRModelResult)
            
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
    
    output$performance <- renderPrint({
        if (modelReady()) {
            enableInput()
            modelReady(FALSE)
        }

        req(incrementalModel())
        model <- isolate(incrementalModel())
        
        print('Full model (score and covariates) summary:')
        print(summary(model$full$model))
        print('Null model (covariates only) summary:')
        print(summary(model$null$model))
        print(compare_performance('Full Model' = model$full$model, 'Null Model' = model$null$model))
    })
    
    output$diagnostics <- renderPlot({
        if (modelReady()) {
            enableInput()
            modelReady(FALSE)
        }

        req(incrementalModel())
        incrementalModelResult <- incrementalModel()
        if (isolate(input$modelType == 'continuous')) {
            check_model(incrementalModelResult$full$model)
        }
    })
    
    output$binary_model_metrics <- renderPlot({
        if (modelReady()) {
            enableInput()
            modelReady(FALSE)
        }
        
        req(incrementalModel())
        incrementalModelResult <- incrementalModel()
        if (isolate(input$modelType == 'binary')) {
            plotMPRIncrementalModelConfusionMatrix(incrementalModelResult$null$response,
                                                   incrementalModelResult$full$response,
                                                   isolate(testYDF()))
        }
    })
    
    output$roc <- renderPlot({
        if (modelReady()) {
            enableInput()
            modelReady(FALSE)
        }
        
        req(incrementalModel())
        incrementalModelResult <- incrementalModel()
        if (isolate(input$modelType == 'binary')) {
            plotMPRIncrementalModelROC(incrementalModelResult)
        }
    })
    
    session$onSessionEnded(function() {
        sink()
    })
    
    output$console <- renderPrint({
        req(consoleFile())
        consoleFile()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
