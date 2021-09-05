set.seed(42)
dummyTrainXs <- matrix(runif(1000), nrow = 100)
dummyTrainY <- apply(dummyTrainXs, 1, function(dataRow) {
  3 * dataRow[[1]] - 2 * dataRow[[2]] + 5 * dataRow[[3]] + rnorm(1, 0, 0.1)
})

dummyTestXs <- matrix(runif(1000), nrow = 100)
dummyTestY <- apply(dummyTestXs, 1, function(dataRow) {
  3 * dataRow[[1]] - 2 * dataRow[[2]] + 5 * dataRow[[3]] + rnorm(1, 0, 0.1)
})

dummyTrainYProb <- plogis(dummyTrainY, location = mean(dummyTrainY))
dummyTestYProb <- plogis(dummyTestY, location = mean(dummyTestY))

dummyTrainYBinary <- rbinom(n = 100, size = 1, prob = dummyTrainYProb)
dummyTestYBinary <- rbinom(n = 100, size = 1, prob = dummyTestYProb)

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
saveRDS(dummyTrainYBinary, 'dummy_data/dummyTrainYBinary.rds')
saveRDS(dummyTestYBinary, 'dummy_data/dummyTestYBinary.rds')
saveRDS(dummyTrainSurv, 'dummy_data/dummyTrainSurv.rds')
saveRDS(dummyTestSurv, 'dummy_data/dummyTestSurv.rds')