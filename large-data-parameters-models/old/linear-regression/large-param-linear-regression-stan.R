## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)
library("bayesplot")
library("bench")
library("MASS")



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

stan_data <- list(
  "N" = n,
  "y" = y,
  "x" = x,
  "M" = num_betas
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("large-data-parameters", "linear-regression", "large-param-linear-regression.stan")

# file.show(stan_file)



## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  stan_fit <- stan(
    "file" = stan_file, "data" = stan_data, 
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
  )
  
  
  
  
  ## assess fit
  ################################################################################
  
  summary(stan_fit)$summary
  get_posterior_mean(stan_fit)
  stan_dens(stan_fit) + theme_bw()
  stan_fit %>% as.array() %>% mcmc_areas()
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
  stan_fit %>% as.array() %>% mcmc_acf_bar()
  stan_fit %>% as.array() %>% mcmc_trace()
  stan_fit %>% as.array() %>% mcmc_hist_by_chain()
  
  
  # see each chain
  stan_fit %>% rstan::extract(permuted = FALSE, inc_warmup = TRUE)
  
}


