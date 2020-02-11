x <- run_benchmark(rds_file_location)
poisson_regression_results <- x
saveRDS(poisson_regression_results, file = "poisson-regression-results.rds")
x

# readRDS("exponential-gamma-results.rds")
