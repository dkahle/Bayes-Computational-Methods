## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("MASS")



## generate/specify data
################################################################################

n <- 500L # sample size
alpha <- -5 # intercept
m <- 50 # number of coefficients
sigma <- 3 # standard deviation

set.seed(1)

beta <- rnorm(m) # single coefficient

(x <- mvrnorm(n, rep(5, m), diag(sigma, m))) # observed x values
y_hat <- as.numeric(alpha + x %*% beta)
(y <- rnorm(n,y_hat,sigma))

nimble_data <- list(
  "y" = y, 
  "x" = x
)

nimble_constants <- list(
  "N" = n,
  "M" = m
)


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:N) {
    y_hat[i] <- alpha + (x[i,1:M] %*% beta[1:M,1])[1,1]
    y[i] ~ dnorm(y_hat[i], tau)
  }
  alpha ~ dnorm(0,0.0001)
  for (m in 1:M) {
    beta[m,1] ~ dnorm(0,0.0001)
  }
  tau ~ T(dnorm(0,0.0001), 0,)
  sigma <- sqrt(1 / tau)
})
nimble_monitor = c("alpha", "beta", "sigma")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "alpha" = rnorm(1,0,1),
  "beta" = matrix(rnorm(m,0,1), ncol = 1),
  "tau" = 1 / (abs(rnorm(1,0,1)) ^ 2)
)


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
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


