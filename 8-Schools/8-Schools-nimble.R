## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("here")



## generate/specify data
################################################################################

J <- 8
y <- c(28,  8, -3,  7, -1,  1, 18, 12)
sigma <- c(15, 10, 16, 11,  9, 11, 10, 18)

nimble_data <- list(
  "y" = y, 
  "sigma" = sigma
)

nimble_constants <- list(
  "J" = J
)


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:J) {
    eta[i] ~ dnorm(0,1)
    sigma[i] ~ dunif(0,1000)
    theta[i] <- mu + tau * eta[i]
    y[i] ~ dnorm(theta[i], (1 / (sigma[i] ^ 2)))
  }
  tau ~ dunif(0,1000)
  mu ~ dnorm(0, 0.0001)
})

nimble_monitors <- c("mu", "tau", "eta", "theta")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "mu" = rnorm(1,0,100),
  "tau" = runif(1,0,1000),
  "eta" = rnorm(J,0,1000)
)


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  nimble_fit <- nimbleMCMC(
    "code" = nimble_model, "data" = nimble_data, "constants" = nimble_constants,
    "inits" = nimble_inits, "monitors" = nimble_monitors, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  
  
  ## assess fit
  ################################################################################
  
  nimble_fit$summary$all.chains
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
}  



