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

alpha <- 2 #beta distribution alpha
beta <- 1 #beta distribution beta
N <- 10 # sample size

set.seed(1)

(n <- rbinom(N, 20, 0.5))
(theta <- rbeta(N, alpha, beta))
(y <- rbinom(N, n, theta))


stan_data <- list(
  "y" = y,
  "N" = N,
  "n" = n
)

## specify stan model
################################################################################

# read it in from file
stan_file <- here("hierarchical-models", "full-beta-binomial", "hierarchical-full-beta-binomial.stan")

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



