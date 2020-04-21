x <- run_benchmark(rds_file_location, stan_only = TRUE, stan_compile = TRUE)
x <- run_benchmark(rds_file_location)

saveRDS(x, 
        file = "ar-model-results-no-nimble.rds")

x

# readRDS("exponential-gamma-results.rds")
