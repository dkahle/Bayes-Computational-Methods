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

n <- 10L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient
set.seed(1)
(x <- rnorm(n, 5, 1)) # observed x values
theta_0 <- rnorm(n,alpha,0.5) + rnorm(n,beta,0.5) * x 
(theta <- exp(theta_0) / (1 + exp(theta_0))) # generated values of bernoulli theta
(y <- rbinom(n,1,theta))
jags_data <- list(
  "n" = n,
  "y" = y, 
  "x" = x
)
jags_model <- "
  model{
    for (i in 1:n) {
      logit(theta[i]) <- alpha + beta * x[i]
      y[i] ~ dbern(theta[i])
    }
    alpha ~ dnorm(0,(1 / 1000^2))
    beta ~ dnorm(0,(1 / 1000^2))
  }
"
jags_monitor <- c("alpha", "beta")

## BUGS Code
################################################################################

n <- 10L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient
set.seed(1)
(x <- rnorm(n, 5, 1)) # observed x values
theta_0 <- rnorm(n,alpha,0.5) + rnorm(n,beta,0.5) * x 
(theta <- exp(theta_0) / (1 + exp(theta_0))) # generated values of bernoulli theta
(y <- rbinom(n,1,theta))
bugs_data <- list(
  "n" = n,
  "y" = y, 
  "x" = x, 
  "tau" = (1 / 1000)^2
)
bugs_model <- function() {
  for (i in 1:n) {
    logit(theta[i]) <- alpha + beta * x[i]
    y[i] ~ dbern(theta[i])
  }
  alpha ~ dnorm(0,tau)
  beta ~ dnorm(0,tau)
}
bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)
bugs_monitor <- c("alpha", "beta")

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

n <- 10L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient
set.seed(1)
(x <- rnorm(n, 5, 1)) # observed x values
theta_0 <- rnorm(n,alpha,0.5) + rnorm(n,beta,0.5) * x 
(theta <- exp(theta_0) / (1 + exp(theta_0))) # generated values of bernoulli theta
(y <- rbinom(n,1,theta))
nimble_data <- list(
  "n" = n,
  "y" = y, 
  "x" = x,
)
nimble_model <- nimbleCode({
  for (i in 1:n) {
    logit(theta[i]) <- alpha + beta * x[i]
    y[i] ~ dbern(theta[i])
  }
  alpha ~ dnorm(0,(1 / 1000^2))
  beta ~ dnorm(0,(1 / 1000^2))
})
nimble_monitor = c("alpha", "beta")
nimble_inits <- list(
  "alpha" = rnorm(1,0,(1 / 1000^2)),
  "beta" = rnorm(1,0,(1 / 1000^2))
)


## STAN Code
################################################################################

n <- 10L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient
set.seed(1)
(x <- rnorm(n, 5, 1)) # observed x values
theta_0 <- rnorm(n,alpha,0.5) + rnorm(n,beta,0.5) * x 
(theta <- exp(theta_0) / (1 + exp(theta_0))) # generated values of bernoulli theta
(y <- rbinom(n,1,theta))
stan_data <- list(
  "n" = n,
  "y" = y, 
  "x" = x
)
stan_file <- here("regression-models", "logistic-regression", "logistic-regression.stan")
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
    "code" = nimble_model, "data" = nimble_data,
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
file.rename(paste0(rds_file_location, "/exponential-gamma1.rds"), 
            paste0(rds_file_location, "/exponential-gamma.rds"))
