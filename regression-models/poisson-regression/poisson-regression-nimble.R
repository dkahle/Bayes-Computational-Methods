## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")



## generate/specify data
################################################################################

n <- 20L # sample size
alpha <- 2 # intercept
beta <- 2 # single coefficient

set.seed(1)

(x <- runif(n, 0, 10)) # observed x values
lambda <- alpha + beta * x 
(y <- rpois(n,lambda))

nimble_data <- list(
  "y" = y, 
  "x" = x
)

nimble_constants <- list(
  "n" = n
)


## specify jags model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:n) {
    log(lambda[i]) <- alpha + beta * x[i]
    y[i] ~ dpois(lambda[i])
  }
  alpha ~ dnorm(0, 1 / (100 ^ 2))
  beta ~ dnorm(0, 1 / (100 ^ 2))
})

nimble_monitor <- c("alpha", "beta")

## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "alpha" = rnorm(1,0,(1 / 1000^2)),
  "beta" = rnorm(1,0,(1 / 1000^2))
)


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  nimble_fit <- nimbleMCMC(
    "code" = nimble_model, "data" = nimble_data, "constants" = nimble_constants,
    "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  
  
  ## assess fit
  ################################################################################
  
  nimble_fit$summary$all.chains
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
}  



