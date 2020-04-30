library(distr)
library(distrEx)

# true_dist <- distr::Beta(3,9)
# 
# num_iterations <- 3

run_accuracy_benchmark <- function(true_dist, num_iterations, n_iter = 1e4L, n_warmup = 1e3L, nimble = FALSE) {
  if (!nimble) {
    
    one_iteration <- function(iter) {
      jags_fit <- run.jags(
        "model" = jags_model, "data" = jags_data, "monitor" = jags_monitor,
        "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
      )
      bugs_seed <- ((iter - 1) %% 14) + 1
      bugs_fit <- bugs(
        "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = bugs_monitor,
        "inits" = NULL, "n.chains" = n_chains, "n.iter" = (n_iter + n_warmup), "n.burnin" = n_warmup,
        "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
        "useWINE" = T, bugs.seed = bugs_seed
      )
      # nimble_fit <- nimbleMCMC(
      #   "code" = nimble_model, "constants" = nimble_constants, "data" = nimble_data,
      #   "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains,
      #   "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
      # )
      stan_fit <- stan(
        "file" = stan_file, "data" = stan_data,
        "chains" = n_chains, "iter" = (n_iter + n_warmup), "warmup" = n_warmup,
        "control" = list("adapt_delta" = 0.99)
      )
      greta_fit <- mcmc(
        "model" = greta_model, "n_samples" = n_iter,
        "warmup" = n_warmup, "chains" = n_chains
      )
      
      jags_fit_object <- jags_fit$mcmc %>% as.array()
      # dim(jags_fit_object) <- c(dim(jags_fit_object), 1)
      jags_fit_condense <- c(jags_fit_object[,1], 
                             jags_fit_object[,2],
                             jags_fit_object[,3],
                             jags_fit_object[,4])
      bugs_fit_condense <- bugs_fit$sims.matrix[,1]
      stan_fit_object <- stan_fit %>% as.array()
      stan_fit_condense <- c(
        stan_fit_object[,1,1],
        stan_fit_object[,2,1],
        stan_fit_object[,3,1],
        stan_fit_object[,4,1]
      )
      greta_fit_object <- greta_fit %>% as.array()
      greta_fit_condense <- c(greta_fit_object[,1], 
                             greta_fit_object[,2],
                             greta_fit_object[,3],
                             greta_fit_object[,4])
      
      jags_dist <- TotalVarDist(true_dist,jags_fit_condense)
      bugs_dist <- TotalVarDist(true_dist,bugs_fit_condense)
      stan_dist <- TotalVarDist(true_dist,stan_fit_condense)
      greta_dist <- TotalVarDist(true_dist,greta_fit_condense)
      
      tibble(
        iter = iter,
        jags_dist = jags_dist,
        bugs_dist = bugs_dist,
        stan_dist = stan_dist,
        greta_dist = greta_dist
      )
    }
    
    dist_results <- 1:num_iterations %>% 
      map_dfr(one_iteration)
    
    print(skimr::skim(dist_results[-1]))
   
    dist_results
  }
  
  else {
    one_iteration <- function(iter) {
      # jags_fit <- run.jags(
      #   "model" = jags_model, "data" = jags_data, "monitor" = jags_monitor,
      #   "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
      # )
      # bugs_seed <- ((iter - 1) %% 14) + 1
      # bugs_fit <- bugs(
      #   "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = bugs_monitor,
      #   "inits" = NULL, "n.chains" = n_chains, "n.iter" = (n_iter + n_warmup), "n.burnin" = n_warmup,
      #   "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
      #   "useWINE" = T, bugs.seed = bugs_seed
      # )
      nimble_fit <- nimbleMCMC(
        "code" = nimble_model, "constants" = nimble_constants, "data" = nimble_data,
        "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains,
        "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
      )
      # stan_fit <- stan(
      #   "file" = stan_file, "data" = stan_data,
      #   "chains" = n_chains, "iter" = (n_iter + n_warmup), "warmup" = n_warmup,
      #   "control" = list("adapt_delta" = 0.99)
      # )
      # greta_fit <- mcmc(
      #   "model" = greta_model, "n_samples" = n_iter,
      #   "warmup" = n_warmup, "chains" = n_chains
      # )
      # 
      # jags_fit_object <- jags_fit$mcmc %>% as.array()
      # # dim(jags_fit_object) <- c(dim(jags_fit_object), 1)
      # jags_fit_condense <- c(jags_fit_object[,1], 
      #                        jags_fit_object[,2],
      #                        jags_fit_object[,3],
      #                        jags_fit_object[,4])
      # bugs_fit_condense <- bugs_fit$sims.matrix[,1]
      # stan_fit_object <- stan_fit %>% as.array()
      # stan_fit_condense <- c(
      #   stan_fit_object[,1,1],
      #   stan_fit_object[,2,1],
      #   stan_fit_object[,3,1],
      #   stan_fit_object[,4,1]
      # )
      # greta_fit_object <- greta_fit %>% as.array()
      # greta_fit_condense <- c(greta_fit_object[,1], 
      #                         greta_fit_object[,2],
      #                         greta_fit_object[,3],
      #                         greta_fit_object[,4])
      

      nimble_fit_object <- nimble_fit$samples %>% as.array()
      nimble_fit_condense <- c(
        nimble_fit_object$chain1[,1],
        nimble_fit_object$chain2[,1],
        nimble_fit_object$chain3[,1],
        nimble_fit_object$chain4[,1]
      )
      
      
      # jags_dist <- TotalVarDist(true_dist,jags_fit_condense)
      # bugs_dist <- TotalVarDist(true_dist,bugs_fit_condense)
      # stan_dist <- TotalVarDist(true_dist,stan_fit_condense)
      # greta_dist <- TotalVarDist(true_dist,greta_fit_condense)
      nimble_dist <- TotalVarDist(true_dist,nimble_fit_condense)
      
      tibble(
        iter = iter,
        nimble_dist = nimble_dist
      )
    }
    
    dist_results <- 1:num_iterations %>% 
      map_dfr(one_iteration)
    
    print(skimr::skim(dist_results[-1]))
    
    dist_results
  }
  
  
}
