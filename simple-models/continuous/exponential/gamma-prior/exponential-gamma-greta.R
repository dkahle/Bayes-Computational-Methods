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

lambda <- 1/2 # exponential lambda
n <- 10       # sample size

set.seed(1)

(y <- rexp(n, lambda))

y <- as_data(y)
n <- as_data(n)

## specify greta model
################################################################################

lambda <- gamma(1,1)

distribution(y) <- exponential(lambda)

greta_model <- model(lambda)

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
  
}

