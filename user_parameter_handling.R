library(stringi)

processParameterInput <- function(inputText) {
  # browser()
  
  if (inputText == '') {
    list()
  }
  
  # Remove spaces and commas
  inputText <- stri_replace_all_fixed(inputText, ' ', '')
  inputText <- stri_replace_all_fixed(inputText, ',', '')
  
  # Split by newline character
  inputLines <- strsplit(inputText, '\n')[[1]]
  
  # Extract key-value pairs separated by '='
  kvPairs <- lapply(inputLines, function(x) {
    strsplit(x, '=')[[1]]
  })
  
  valueList <- lapply(kvPairs, function(x) {
    type.convert(x[[2]], as.is = TRUE)
  })
  
  names(valueList) <- sapply(kvPairs, function(x) {
    x[[1]]
  })
  
  valueList
}



getModelHyperparameterDefaults <- function(modelMethod, cv) {
  if (cv) {
    
  } else {
    if (modelMethod == 'glmnet') {
      'alpha = 0'
    } else if (modelMethod == 'bart') {
      ''
    } else if (modelMethod == 'rf') {
      ''
    }
  }
}