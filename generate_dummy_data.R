dummyTrainXs <- matrix(runif(100000), nrow = 100)
dummyTrainY <- apply(dummyTrainXs, 1, function(dataRow) {
  3 * dataRow[[1]] - 2 * dataRow[[2]] + 5 * dataRow[[3]] + rnorm(1, 0, 0.1)
})

dummyTestXs <- matrix(runif(100000), nrow = 100)
dummyTestY <- apply(dummyTestXs, 1, function(dataRow) {
  3 * dataRow[[1]] - 2 * dataRow[[2]] + 5 * dataRow[[3]] + rnorm(1, 0, 0.1)
})

dummyTrainYBinary <- as.numeric(plogis(dummyTrainY, location = mean(dummyTrainY)) > 0.5)
dummyTestYBinary <- as.numeric(plogis(dummyTestY, location = mean(dummyTestY)) > 0.5)


dummyCovariates <- matrix(rnorm(300), nrow = 100)

dummyTrainTTE <- runif(100, min = 0, max = 15)
dummyTestTTE <- runif(100, min = 0, max = 15)

dummyTrainSurv <- data.frame('E' = dummyTrainYBinary, 'TTE' = dummyTrainTTE)
dummyTestSurv <- data.frame('E' = dummyTestYBinary, 'TTE' = dummyTestTTE)

saveRDS(dummyTrainXs, 'dummy_data/dummyTrainXs.rds')
saveRDS(dummyTrainY, 'dummy_data/dummyTrainY.rds')
saveRDS(dummyTestXs, 'dummy_data/dummyTestXs.rds')
saveRDS(dummyTestY, 'dummy_data/dummyTestY.rds')
saveRDS(dummyCovariates, 'dummy_data/dummyCovariates.rds')
saveRDS(dummyTestYBinary, 'dummy_data/dummyTrainYBinary.rds')
saveRDS(dummyTestYBinary, 'dummy_data/dummyTestYBinary.rds')
saveRDS(dummyTrainSurv, 'dummy_data/dummyTrainSurv.rds')
saveRDS(dummyTestSurv, 'dummy_data/dummyTestSurv.rds')