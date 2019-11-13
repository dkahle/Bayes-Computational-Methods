## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")



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


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:N) {
    y[i] ~ dnorm(mu, 1 / tau)
  }
  mu ~ dnorm(0,1)
})
nimble_monitor = c("mu")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "mu" = rnorm(1)
)


## fit model
################################################################################
if (is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]])) {
  
  nimble_fit <- nimbleMCMC(
    "code" = nimble_model, "constants" = nimble_constants, "data" = nimble_data,
    "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  
  
  ## assess fit
  ################################################################################
  
  nimble_fit$summary$all.chains
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
}  


