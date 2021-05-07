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

n <- 1000L # sample size
alpha <- -5 # intercept
num_betas <- 200
beta <- rnorm(num_betas, 1, 5) # single coefficient
sigma <- 3 # standard deviation

set.seed(1)

(x <- mvrnorm(n, mu = rep(5, num_betas), Sigma = diag(4, num_betas))) # observed x values
y_hat <- (alpha + x %*% beta) %>% as.double()
(y <- rnorm(n,y_hat,sigma))

x <- as_data(x)
y <- as_data(y)



## specify greta model
################################################################################

alpha <- normal(0, 1000)
beta <- normal(0, 1000, dim = c(num_betas, 1))
sigma <- normal(0,1000, truncation = c(0,Inf))

distribution(y) <- normal(alpha + x %*% beta, sigma)

greta_model <- model(alpha, beta, sigma)

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

