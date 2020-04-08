x <- run_benchmark(rds_file_location)
cox_exponential_results <- x
saveRDS(cox_exponential_results, 
        file = "cox-exponential-results.rds")

x

# readRDS("exponential-gamma-results.rds")
