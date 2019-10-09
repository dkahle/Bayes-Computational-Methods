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
sigma <- 2 # normal sigma
n <- 10    # sample size
set.seed(1)
(y <- rnorm(n, mu, sigma))
jags_data <- list(
  "y" = y,
  "tau" = 1 / sigma ^2,
  "N" = n
)
jags_model <- "
  model{
    for (i in 1:N) {
      y[i] ~ dnorm(mu, 1 / tau)
    }
    mu ~ dnorm(0, 1)
  }
"
jags_monitor <- c("mu")

## BUGS Code
################################################################################

mu <- 1    # normal mu
sigma <- 2 # normal sigma
n <- 10    # sample size
set.seed(1)
(y <- rnorm(n, mu, sigma))
bugs_data <- list(
  "y" = y,
  "tau" = 1 / sigma ^2,
  "N" = n
)
bugs_model <- function() {
  for (i in 1:N) {
    y[i] ~ dnorm(mu, sigma_sq)
  }
  sigma_sq <- 1 / tau
  mu ~ dnorm(0, 1)
}
bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)
bugs_monitor <- "mu"

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
sigma <- 2 # normal sigma
n <- 10    # sample size
set.seed(1)
(y <- rnorm(n, mu, sigma))
nimble_data <- list(
  "y" = y
)
nimble_constants <- list(
  "tau" = 1 / sigma ^2,
  "N" = n
)
nimble_model <- nimbleCode({
  for (i in 1:N) {
    y[i] ~ dnorm(mu, 1 / tau)
  }
  mu ~ dnorm(0,1)
})
nimble_monitor = c("mu")
nimble_inits <- list(
  "mu" = rnorm(1,0,1)
)

## STAN Code
################################################################################

n <- 10L # binomial n
p <- .25 # binomial p
set.seed(1)
(y <- rbinom(1, n, p))
stan_data <- list(
  "n" = n,
  "y" = y
)
stan_file <- here("simple models", "continuous", "normal", "normal-prior", "normal-normal.stan")
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
rds_file_location <- here("simple models", "continuous", "normal", "normal-prior")
file.rename(paste0(rds_file_location, "/normal-normal.rds"), 
            paste0(rds_file_location, "/normal-normal1.rds"))

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
file.rename(paste0(rds_file_location, "/normal-normal1.rds"), 
            paste0(rds_file_location, "/normal-normal.rds"))

