## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("bench")
library("here")
library("rjags"); library("runjags")
library("R2OpenBUGS")
library("nimble")
library("rstan"); 


## JAGS Code
################################################################################

lambda <- 1/2 # exponential lambda
n <- 10       # sample size
set.seed(1)
(y <- rexp(n, lambda))
jags_data <- list(
  "y" = y,
  "N" = n
)
jags_model <- "
  model{
    for (i in 1:N) {
      y[i] ~ dexp(lambda)
    }
    lambda ~ dgamma(1,1)
  }
"
jags_monitor <- c("lambda")

## BUGS Code
################################################################################

lambda <- 1/2 # exponential lambda
n <- 10       # sample size
set.seed(1)
(y <- rexp(n, lambda))
bugs_data <- list(
  "y" = y,
  "N" = n
)
bugs_model <- function() {
  for (i in 1:N) {
    y[i] ~ dexp(lambda)
  }
  lambda ~ dgamma(1,1)
}
bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)
bugs_monitor <- "lambda"

if (getwd() == "/Users/evanmiyakawa/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods") {
  WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
  WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
  OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~
} else {
  WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
  WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
  OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"
}


## Nimble Code
################################################################################

lambda <- 1/2 # exponential lambda
n <- 10       # sample size
set.seed(1)
(y <- rexp(n, lambda))
nimble_data <- list(
  "y" = y
)
nimble_constants <- list(
  "N" = n
)
nimble_model <- nimbleCode({
  for (i in 1:N) {
    y[i] ~ dexp(lambda)
  }
  lambda ~ dgamma(1,1)
})
nimble_monitor = c("lambda")
nimble_inits <- list(
  "lambda" = rgamma(1,1,1)
)


## STAN Code
################################################################################

lambda <- 1/2 # exponential lambda
n <- 10       # sample size
set.seed(1)
(y <- rexp(n, lambda))
stan_data <- list(
  "y" = y,
  "N" = n
)
stan_file <- here("simple models", "continuous", "exponential", "gamma-prior", "exponential-gamma.stan")
# file.show(stan_file)

## Set Parameters
################################################################################
n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

## benchmarking
###################################################################################

num_iterations <- 1 # Number of times you want to benchmark each model

## This version doesn't include STAN compile time

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
    "code" = nimble_model, "data" = nimble_data, "constants" = nimble_constants,
    "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains,
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  ), 
  "stan_fit" = stan(
    "file" = stan_file, "data" = stan_data,
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
  ),
  "check" = FALSE, 
  "iterations" = num_iterations, 
  "filter_gc" = FALSE
  
)

bench_results

## Include STAN compile time - rename stored compile file to force recompile
rds_file_location <- here("simple models", "continuous", "exponential", "gamma-prior")
file.rename(paste0(rds_file_location, "/exponential-gamma.rds"), 
            paste0(rds_file_location, "/exponential-gamma1.rds"))

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
    "code" = nimble_model, "data" = nimble_data, "constants" = nimble_constants,
    "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains,
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  ), 
  "stan_fit" = stan(
    "file" = stan_file, "data" = stan_data,
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
  ),
  "check" = FALSE, 
  "iterations" = 1, 
  "filter_gc" = FALSE
  
)

bench_results

## Rename back to original name
file.rename(paste0(rds_file_location, "/exponential-gamma1.rds"), 
            paste0(rds_file_location, "/exponential-gamma.rds"))
