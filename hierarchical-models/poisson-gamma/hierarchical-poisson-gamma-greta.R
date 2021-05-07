## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
library("bayesplot")
library("bench")
library("here")


## generate/specify data
################################################################################

N <- 8 # sample size
y <- c(10, 9, 21, 4, 5, 21, 1, 1) # data
t <- c(47.6, 53.4, 44.4, 45, 51.1, 58, 55.3, 43.7) # known constants

y <- as_data(y)
t <- as_data(t)


## specify greta model
################################################################################

alpha <- exponential(1)
beta <- gamma(0.1, 1)
theta <- gamma(alpha, beta, dim = c(N,1))
distribution(y) <- poisson(t * theta, dim = c(N,1))

greta_model <- model(theta, alpha, beta)

plot(greta_model)

## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  
  greta_fit <- mcmc(
    "model" = greta_model, 
    "n_samples" = n_iter,
    "warmup" = n_warmup,
    "chains" = n_chains
  )
  
  
  
  ## assess fit
  ################################################################################
  
  summary(greta_fit)
  
  greta_fit_object <- greta_fit %>% as.array()
  dim(greta_fit_object) <- c(dim(greta_fit_object), 1)
  dimnames(greta_fit_object) <- list(
    "iterations" = NULL, 
    "chains" = 1:n_chains, 
    "parameters" = c("theta")
  )
  
  
  greta_fit_object %>% mcmc_areas()
  greta_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  greta_fit_object %>% mcmc_acf_bar()
  greta_fit_object %>% mcmc_trace()
  greta_fit_object %>% mcmc_hist_by_chain()
  
}

