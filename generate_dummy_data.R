set.seed(42)
dummyTrainXs <- matrix(runif(1300), nrow = 100)
dummyTrainY <- apply(dummyTrainXs, 1, function(dataRow) {
  3 * dataRow[[1]] - 2 * dataRow[[2]] + 5 * dataRow[[3]] + 0.1 * dataRow[[11]] + 0.2 * dataRow[[12]] - 0.3 * dataRow[[13]] + rnorm(1, 0, 0.1)
})
dummyTrainXs <- dummyTrainXs[, 1:10]

dummyTestXs <- matrix(runif(1300), nrow = 100)
dummyTestY <- apply(dummyTestXs, 1, function(dataRow) {
  3 * dataRow[[1]] - 2 * dataRow[[2]] + 5 * dataRow[[3]] + 0.1 * dataRow[[11]] + 0.2 * dataRow[[12]] - 0.3 * dataRow[[13]] + rnorm(1, 0, 0.1)
})
dummyCovariates <- dummyTestXs[, 11:13]
dummyTestXs <- dummyTestXs[, 1:10]

dummyTrainYProb <- plogis(dummyTrainY, location = mean(dummyTrainY))
dummyTestYProb <- plogis(dummyTestY, location = mean(dummyTestY))

dummyTrainYBinary <- rbinom(n = 100, size = 1, prob = dummyTrainYProb)
dummyTestYBinary <- rbinom(n = 100, size = 1, prob = dummyTestYProb)

# dummyCovariates <- matrix(rnorm(300), nrow = 100)


dummyTrainTTE <- runif(100, min = 0, max = 15)
dummyTestTTE <- runif(100, min = 0, max = 15)

dummyTrainSurv <- data.frame('E' = dummyTrainYBinary, 'TTE' = dummyTrainTTE)
dummyTestSurv <- data.frame('E' = dummyTestYBinary, 'TTE' = dummyTestTTE)

saveRDS(dummyTrainXs, 'simulated_data/trainXs.rds')
saveRDS(dummyTrainY, 'simulated_data/trainYContinuous.rds')
saveRDS(dummyTestXs, 'simulated_data/testXs.rds')
saveRDS(dummyTestY, 'simulated_data/testYContinuous.rds')
saveRDS(dummyCovariates, 'simulated_data/testCovariates.rds')
saveRDS(dummyTrainYBinary, 'simulated_data/trainYBinary.rds')
saveRDS(dummyTestYBinary, 'simulated_data/testYBinary.rds')
saveRDS(dummyTrainSurv, 'simulated_data/trainSurv.rds')
saveRDS(dummyTestSurv, 'simulated_data/testSurv.rds')