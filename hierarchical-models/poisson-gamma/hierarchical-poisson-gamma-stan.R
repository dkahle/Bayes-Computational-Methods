## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)
library("bench")
library("bayesplot")

## generate/specify data
################################################################################

N <- 8 # sample size
y <- c(10, 9, 21, 4, 5, 21, 1, 1) # data
t <- c(47.6, 53.4, 44.4, 45, 51.1, 58, 55.3, 43.7) # known constants

stan_data <- list(
  "y" = y,
  "N" = N,
  "t" = t
)

## specify stan model
################################################################################

# read it in from file
stan_file <- here("hierarchical-models", "poisson-gamma", "hierarchical-poisson-gamma.stan")

# file.show(stan_file)



## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

# n_warmup <- 10000
# n_iter <- 40000


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



