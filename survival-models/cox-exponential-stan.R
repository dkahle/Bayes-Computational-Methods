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
start_censored <- num_uncensored + 1
num_censored <- sum(is_censored)
t <- t[order(is_censored)]
x <- x[order(is_censored)]
t_censor <- t

t_censor <- t[start_censored:N]
t <- t[1:num_uncensored]
x_c <- x[start_censored:N]
x <- x[1:num_uncensored]

stan_data <- list(
  "t" = t,
  "t_censor" = t_censor,
  "x" = x,
  "x_c" = x_c,
  "num_uncensored" = num_uncensored,
  "num_censored" = num_censored
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


t_inits <- t_censor + 1
stan_inits <- list(
  list("t_u" = t_inits),
  list("t_u" = t_inits),
  list("t_u" = t_inits),
  list("t_u" = t_inits)
)
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



