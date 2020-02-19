x <- run_benchmark(rds_file_location)
logistic_regression_results <- x
saveRDS(logistic_regression_results, file = "logistic-regression-results.rds")
x

# readRDS("exponential-gamma-results.rds")
