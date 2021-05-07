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

alpha <- 2 #beta distribution alpha
beta <- 1 #beta distribution beta
N <- 10 # sample size

set.seed(1)

(n <- rbinom(N, 20, 0.5))
(theta <- rbeta(N, alpha, beta))
(y <- rbinom(N, n, theta))

n <- as_data(n)
y <- as_data(y)

## specify greta model
################################################################################

alpha <- gamma(1,1)
beta <- gamma(1,1)
theta <- beta(alpha,beta, dim = c(N,1))
distribution(y) <- binomial(n, theta, dim = c(N,1))

greta_model <- model(theta, alpha, beta)

plot(greta_model)

## configure model settings
################################################################################

n_chains <- 4
n_iter <- 1e4L
n_warmup <- 1e3L



## fit model
################################################################################

source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  
  greta_fit <- mcmc(
    "model" = greta_model, "n_samples" = n_iter,
    "warmup" = n_warmup, "chains" = n_chains
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

