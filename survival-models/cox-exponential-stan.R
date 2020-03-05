## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)
library("bench")
library("bayesplot")
library("survival")

## generate/specify data
################################################################################

data(veteran) # load spatial data set



t <- veteran$time # observed / censored event times
x <- veteran$trt # single covariate
not_censored <- veteran$status # indicator variable for those who weren't censored
is_censored <- 1 - not_censored # indicator variable for those who were censored
N <- length(t)

# Here we create variables that will allow us to handle censored data in the bayes model
num_uncensored <- sum(not_censored)
num_censored <- length(t) - num_uncensored
start_censored <- num_uncensored + 1
t <- t[order(is_censored)]
x <- x[order(is_censored)]
t_censor <- t
t[start_censored:N] <- NA

stan_data <- list(
  "t" = t,
  "t_censor" = t_censor,
  "x" = x,
  "num_uncensored" = num_uncensored,
  "start_censored" = num_uncensored + 1,
  "N" = length(t)
)

stan_data <- list(
  "t" = t[1:num_uncensored],
  "t_censor" = t_censor[(num_uncensored + 1):length(t)],
  "x" = x,
  "num_uncensored" = num_uncensored,
  "num_censored" = length(t) - num_uncensored,
  "N" = length(t)
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("survival-models", "cox-exponential.stan")

# file.show(stan_file)



## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


t_inits <- stan_data$t_censor + 1
stan_inits <- replicate(4,list("t_c" = t_inits), simplify = FALSE)

## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  stan_fit <- stan(
    "file" = stan_file, "data" = stan_data, "init" = stan_inits,
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup, 
    "control" = list("adapt_delta" = 0.99)
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



