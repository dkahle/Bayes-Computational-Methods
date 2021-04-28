## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")



## generate/specify data
################################################################################

n <- 10000L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient
sigma <- 3 # standard deviation

set.seed(1)

(x <- rnorm(n, 5, 4)) # observed x values
y_hat <- alpha + beta * x
(y <- rnorm(n,y_hat,sigma))

nimble_data <- list(
  "y" = y,
  "x" = x
)

nimble_constants <- list(
  "N" = n
)


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:N) {
    y_hat[i] <- alpha + beta * x[i]
    y[i] ~ dnorm(y_hat[i], tau)
  }
  alpha ~ dnorm(0,0.0001)
  beta ~ dnorm(0,0.0001)
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
  "alpha" = rnorm(1,0,1000),
  "beta" = rnorm(1,0,1000),
  "tau" = 1 / (abs(rnorm(1,0,1000)) ^ 2)
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
  
  nimble_fit_vector <- nimble_fit$samples %>% as_vector()
  nimble_fit_object <- array(nimble_fit_vector, 
                             dim = c(length(nimble_fit_vector) / n_chains, n_chains))
  dim(nimble_fit_object) <- c(dim(nimble_fit_object), 1)
  dimnames(nimble_fit_object) <- list(
    "iterations" = NULL, 
    "chains" = 1:n_chains, 
    "parameters" = nimble_monitor
  )
  
  
  nimble_fit_object %>% mcmc_areas()
  nimble_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  nimble_fit_object %>% mcmc_acf_bar()
  nimble_fit_object %>% mcmc_trace()
  nimble_fit_object %>% mcmc_hist_by_chain()
  
  
}  


