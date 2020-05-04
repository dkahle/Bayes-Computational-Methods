x <- run_benchmark(rds_file_location, stan_only = TRUE, stan_compile = TRUE)
x <- run_benchmark(rds_file_location, greta_only = TRUE)
x <- run_benchmark(rds_file_location)
x <- run_accuracy_benchmark(true_dist, num_iterations, nimble = TRUE)

saveRDS(x, 
        file = "normal-normal-accuracy-nimble.rds")

x

# readRDS("exponential-gamma-results.rds")
