x <- run_benchmark(rds_file_location)
hierarchical_full_beta_binomial_results <- x
saveRDS(hierarchical_full_beta_binomial_results, file = "hierarchical-full-beta-binomial-results.rds")
x

# readRDS("exponential-gamma-results.rds")
