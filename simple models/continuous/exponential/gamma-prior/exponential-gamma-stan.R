## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)
library("bayesplot")
library("bench")



## generate/specify data
################################################################################

lambda <- 1/2 # exponential lambda
n <- 10       # sample size

set.seed(1)

(y <- rexp(n, lambda))

stan_data <- list(
  "y" = y,
  "N" = n
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("simple models", "continuous", "exponential", "gamma-prior", "exponential-gamma.stan")

# file.show(stan_file)



## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


## fit model
################################################################################
if (is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]])) {
  
  stan_fit <- stan(
    "file" = stan_file, "data" = stan_data, 
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
  )
  
  
  
  
  ## assess fit
  ################################################################################
  
  summary(stan_fit)$summary
  get_posterior_mean(stan_fit)
  stan_dens(stan_fit) + theme_bw()
  stan_fit %>% as.array() %>% bayesplot::mcmc_dens()
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
  stan_fit %>% as.array() %>% mcmc_acf_bar()
  stan_fit %>% as.array() %>% mcmc_pairs()
  stan_fit %>% as.array() %>% mcmc_trace()
  
  # see each chain
  stan_fit %>% rstan::extract(permuted = FALSE, inc_warmup = TRUE)
  
}




