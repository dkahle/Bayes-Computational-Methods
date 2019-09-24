## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")



## generate/specify data
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
  "mu" = mu
  "N" = n
)


## specify jags model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:N) {
    y[i] ~ dnorm(mu, 1 / tau)
  }
  tau ~ dgamma(1,3)
})
monitors = c("tau")


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "tau" = rgamma(1,1,3)
)

nimble_fit <- nimbleMCMC(
  "code" = nimble_model, "data" = nimble_data, "constants" = nimble_constants,
  "inits" = nimble_inits, "monitors" = monitors, "nchains" = n_chains, 
  "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
)


## assess fit
################################################################################

nimble_fit$summary$all.chains



## assess convergence issues 
###################################################################################


## benchmarking
###################################################################################

bench_results <- mark(
  nimble_fit <- nimbleMCMC(
    "code" = nimble_model, "data" = nimble_data, 
    "inits" = nimble_inits, "monitors" = monitors, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  ),
  iterations = 3
)
bench_results[1,2:9]


