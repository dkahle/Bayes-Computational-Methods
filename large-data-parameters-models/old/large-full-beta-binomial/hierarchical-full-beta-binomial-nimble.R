## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("here")



## generate/specify data
################################################################################

alpha <- 2 #beta distribution alpha
beta <- 1 #beta distribution beta
N <- 500 # sample size

set.seed(1)

(n <- rbinom(N, 20, 0.5))
(theta <- rbeta(N, alpha, beta))
(y <- rbinom(N, n, theta))


nimble_data <- list(
  "y" = y,
  "n" = n
)

nimble_constants <- list(
  "N" = N
)


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:N) {
    theta[i] ~ dbeta(alpha, beta)
    y[i] ~ dbin(theta[i], n[i])
  }
  alpha ~ dgamma(1,1)
  beta ~ dgamma(1,1)
})

nimble_monitor <- c("theta", "alpha", "beta")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "theta" = rbeta(N,2,1),
  "alpha" = rgamma(1,1,1),
  "beta" = rgamma(1,1,1)
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



