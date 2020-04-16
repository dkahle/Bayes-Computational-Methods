x <- run_benchmark(rds_file_location, stan_compile = TRUE, stan_only = TRUE)
poisson_gamma_results_stan <- x
saveRDS(poisson_gamma_results_stan, 
        file = "poisson-gamma-results-stan.rds")

x

# readRDS("exponential-gamma-results.rds")
