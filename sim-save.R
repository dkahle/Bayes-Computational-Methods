x <- run_benchmark(rds_file_location)
hierarchical_logistic_regression_results <- x
saveRDS(hierarchical_logistic_regression_results, 
        file = "hierarchical-logistic-regression-results.rds")

x

# readRDS("exponential-gamma-results.rds")
