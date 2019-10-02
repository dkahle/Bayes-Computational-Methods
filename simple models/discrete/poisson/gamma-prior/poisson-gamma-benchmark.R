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

theta <- 5 # poisson theta
set.seed(1)
(y <- rpois(1, theta))
jags_data <- list(
  "y" = y
)
jags_model <- "
  model{
    y ~ dpois(theta)
    theta ~ dgamma(3,1)
  }
"
jags_monitor <- c("theta")

## BUGS Code
################################################################################

theta <- 5 # poisson theta
set.seed(1)
(y <- rpois(1, theta))
bugs_data <- list(
  "y" = y
)
bugs_model <- function() {
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
}
bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)
bugs_monitor <- "theta"

WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"
# WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
# WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
# OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~

## Nimble Code
################################################################################

theta <- 5 # poisson theta
set.seed(1)
(y <- rpois(1, theta))
nimble_data <- list(
  "y" = y
)
nimble_model <- nimbleCode({
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
})
nimble_monitor <- c("theta")
nimble_inits <- list(
  "theta" = rgamma(1,3,1)
)

## STAN Code
################################################################################

theta <- 5 # poisson theta
set.seed(1)
(y <- rpois(1, theta))
stan_data <- list(
  "theta" = theta,
  "y" = y
)
stan_file <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma.stan")
# file.show(stan_file)

## Set Parameters
################################################################################
n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

## benchmarking
###################################################################################

## Don't include STAN compile time

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
    "code" = nimble_model, "data" = nimble_data,
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

## Include STAN compile time - rename stored compile file to force recompile
rds_file_location <- here("simple models", "discrete", "poisson", "gamma-prior")
file.rename(paste0(rds_file_location, "/poisson-gamma.rds"), 
            paste0(rds_file_location, "/poisson-gamma1.rds"))

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
    "code" = nimble_model, "data" = nimble_data,
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
file.rename(paste0(rds_file_location, "/poisson-gamma1.rds"), 
            paste0(rds_file_location, "/poisson-gamma.rds"))
