## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("here")



## generate/specify data
################################################################################

N <- 8 # sample size
y <- c(10, 9, 21, 4, 5, 21, 1, 1) # data
t <- c(47.6, 53.4, 44.4, 45, 51.1, 58, 55.3, 43.7) # known constants

nimble_data <- list(
  "y" = y
)

nimble_constants <- list(
  "N" = N,
  "t" = t
)


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:N) {
    theta[i] ~ dgamma(alpha, beta) # prior on the thetas
    lambda[i] <- theta[i] * t[i] # define Poisson rate
    y[i] ~ dpois(lambda[i]) # likelihood
  }
  alpha ~ dexp(1) # hyperprior parameters
  beta ~ dgamma(0.1, 1.0)
})

nimble_monitor <- c("theta")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

get_nimble_initial_values <- function() {
  alpha <- rexp(N, 1)
  beta <- rgamma(N, 0.1, 1)
  theta <- rgamma(N, alpha, 1 / beta)
  list(theta = theta)
}

nimble_inits <- get_nimble_initial_values
nimble_inits <- list(
  theta =  rep(1, 8)
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



