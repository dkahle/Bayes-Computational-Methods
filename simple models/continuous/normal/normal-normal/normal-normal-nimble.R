## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")



## generate/specify data
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


## specify jags model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:N) {
    y[i] ~ dnorm(mu, 1 / tau)
  }
  mu ~ dnorm(0,1)
})
monitors = c("mu")


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "mu" = rnorm(1,0,1)
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



