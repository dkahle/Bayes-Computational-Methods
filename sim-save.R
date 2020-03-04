x <- run_benchmark(rds_file_location)
ma_model_results <- x
saveRDS(ma_model_results, file = "ma-model-results.rds")
x

# readRDS("exponential-gamma-results.rds")
