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

mu <- 1    # normal mu
tau <- 1/2 # normal tau
n <- 10    # sample size
set.seed(1)
(y <- rnorm(n, mu, 1 / sqrt(tau)))
jags_data <- list(
  "y" = y,
  "mu" = mu,
  "N" = n
)
jags_model <- "
  model{
    for (i in 1:N) {
      y[i] ~ dnorm(mu, tau)
    }
    tau ~ dgamma(1, 3)
  }
"
jags_monitor <- c("tau")

## BUGS Code
################################################################################

mu <- 1    # normal mu
tau <- 1/2 # normal tau
n <- 10    # sample size
set.seed(1)
(y <- rnorm(n, mu, 1 / sqrt(tau)))
bugs_data <- list(
  "y" = y,
  "mu" = mu,
  "N" = n
)
bugs_model <- function() {
  for (i in 1:N) {
    y[i] ~ dnorm(mu, tau)
  }
  tau ~ dgamma(1,3)
}
bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)
bugs_monitor <- c("tau")

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

mu <- 1    # normal mu
tau <- 1/2 # normal tau
n <- 10    # sample size
set.seed(1)
(y <- rnorm(n, mu, sigma))
nimble_data <- list(
  "y" = y
)
nimble_constants <- list(
  "mu" = mu,
  "N" = n
)
nimble_model <- nimbleCode({
  for (i in 1:N) {
    y[i] ~ dnorm(mu, 1 / tau)
  }
  tau ~ dgamma(1,3)
})
nimble_monitor = c("tau")
nimble_inits <- list(
  "tau" = rgamma(1,1,3)
)


## STAN Code
################################################################################

mu <- 1    # normal mu
tau <- 1/2 # normal tau
n <- 10    # sample size
set.seed(1)
(y <- rnorm(n, mu, 1 / sqrt(tau)))
stan_data <- list(
  "y" = y,
  "mu" = mu,
  "N" = n
)
stan_file <- here("simple models", "continuous", "normal", "gamma-prior-tau", "normal-gamma-tau.stan")
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
rds_file_location <- here("simple models", "continuous", "normal", "gamma-prior-tau")
file.rename(paste0(rds_file_location, "/normal-gamma-tau.rds"), 
            paste0(rds_file_location, "/normal-gamma-tau1.rds"))

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
file.rename(paste0(rds_file_location, "/normal-gamma-tau1.rds"), 
            paste0(rds_file_location, "/normal-gamma-tau.rds"))
