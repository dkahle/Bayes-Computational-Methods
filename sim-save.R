x <- run_accuracy_benchmark(true_dist, true_mean, num_iterations)
x <- run_accuracy_benchmark(true_dist, true_mean, num_iterations, nimble = TRUE)
# x <- run_benchmark(rds_file_location, greta_only = TRUE)

saveRDS(x, 
        file = "exponential-gamma-accuracy-nimble.rds")

x

# readRDS("exponential-gamma-results.rds")
