x <- run_benchmark(rds_file_location)
linear_regression_results <- x
saveRDS(linear_regression_results, file = "linear-regression-results.rds")
x

# readRDS("exponential-gamma-results.rds")
