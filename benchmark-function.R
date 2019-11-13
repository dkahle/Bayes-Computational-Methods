run_benchmark <- function(rds_file_location, stan_compile = FALSE) {
  if (!stan_compile) {
    bench_results <- mark(
      "jags_fit" = run.jags(
        "model" = jags_model, "data" = jags_data, "monitor" = jags_monitor,
        "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
      ), 
      "bugs_fit" = bugs(
        "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = bugs_monitor,
        "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
        "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
        "useWINE" = T
      ),
      "nimble_fit" = nimbleMCMC(
        "code" = nimble_model, "constants" = nimble_constants, "data" = nimble_data,
        "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains,
        "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
      ), 
      "stan_fit" = stan(
        "file" = stan_file, "data" = stan_data,
        "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
      ),
      "greta_fit" = mcmc(
        "model" = greta_model, "n_samples" = n_iter,
        "warmup" = n_warmup, "chains" = n_chains
      ),
      "check" = FALSE, 
      "iterations" = num_iterations, 
      "filter_gc" = FALSE
      
    )
    
    bench_results
  } else {
    original_location <- rds_file_location
    new_location <- paste0(str_sub(original_location,1,-5),"1.rds")
    file.rename(original_location, new_location)
    
    bench_results_stan_compile <- mark(
      "stan_compile" = stan_model("file" = stan_file),
      "check" = FALSE, 
      "iterations" = num_iterations, 
      "filter_gc" = FALSE
    )
    
    file.rename(new_location, original_location)
    
    bench_results <- mark(
      "jags_fit" = run.jags(
        "model" = jags_model, "data" = jags_data, "monitor" = jags_monitor,
        "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
      ), 
      "bugs_fit" = bugs(
        "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = bugs_monitor,
        "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
        "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
        "useWINE" = T
      ),
      "nimble_fit" = nimbleMCMC(
        "code" = nimble_model, "constants" = nimble_constants, "data" = nimble_data,
        "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains,
        "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
      ), 
      # "stan_compile" = stan_model("file" = stan_file
      # ),
      "stan_fit" = stan(
        "file" = stan_file, "data" = stan_data,
        "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
      ),
      "greta_fit" = mcmc(
        "model" = greta_model, "n_samples" = n_iter,
        "warmup" = n_warmup,"chains" = n_chains
      ),
      "check" = FALSE, 
      "iterations" = num_iterations, 
      "filter_gc" = FALSE
      
    )
    rbind(bench_results,bench_results_stan_compile)
  }
  
  
}
