## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("bench")
library("here")

## source R scripts
################################################################################

jags_script <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma-jags.R")
bugs_script <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma-bugs.R")
nimble_script <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma-nimble.R")
stan_script <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma-stan.R")

source(jags_script, local = TRUE)
source(bugs_script, local = TRUE)
source(nimble_script, local = TRUE)
source(stan_script, local = TRUE)

## benchmarking
###################################################################################


rds_file <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma.rds")
file.remove(rds_file)

rstan_options(auto_write = FALSE)

bench_results <- mark(
  "stan_fit" = stan(
    "file" = stan_file, "data" = stan_data,
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
  ),
  "check" = FALSE, 
  "iterations" = 1, 
  "filter_gc" = FALSE
  
)


bench_results <- mark(
  "jags_fit" = run.jags(
    "model" = jags_model, "data" = jags_data, "monitor" = monitor,
    "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
  ),
  "bugs_fit" = bugs(
    "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = monitor,
    "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
    "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
    "useWINE" = T
  ),
  "nimble_fit" = nimbleMCMC(
    "code" = nimble_model, "data" = nimble_data,
    "inits" = nimble_inits, "monitors" = monitors, "nchains" = n_chains,
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  ),
  "stan_fit" = stan(
    "file" = stan_file, "data" = stan_data,
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
  ),
  
  iterations = 1
)

bench_results <- mark(
  "jags_fit" = run.jags(
    "model" = jags_model, "data" = jags_data, "monitor" = monitor,
    "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
  ), 
  "bugs_fit" = bugs(
    "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = monitor,
    "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
    "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
    "useWINE" = T
  ),
  "nimble_fit" = nimbleMCMC(
    "code" = nimble_model, "data" = nimble_data,
    "inits" = nimble_inits, "monitors" = monitors, "nchains" = n_chains,
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  ), 
  "stan_fit" = stan(
    "file" = stan_file, "data" = stan_data,
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup, 
    "save_dso" = FALSE
  ),
  "check" = FALSE, 
  "iterations" = 1, 
  "filter_gc" = FALSE
  
)
bench_results

bench_results[,2:9]

dat <- data.frame(x = runif(100, 1, 1000), y=runif(10, 1, 1000))
mark(
  min_time = .1,
  
  "1" = dat[dat$x > 500, ],
  "2" = dat[which(dat$x > 500), ],
  "3" = subset(dat, x > 500)
)

rstan_options(auto_write = FALSE)
bench_results <- mark(
  "stan_fit" = stan(
    "file" = stan_file, "data" = stan_data, 
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup, 
    "save_dso" = FALSE
  ),
  iterations = 1, 
  filter_gc = FALSE
)
bench_results
