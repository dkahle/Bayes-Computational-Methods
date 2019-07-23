## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)

## generate/specify data
################################################################################

theta <- 5 # poisson theta

set.seed(1)

(y <- rpois(1, theta))

stan_data <- list(
  "theta" = theta,
  "y" = y
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma.stan")
file.show(stan_file)



## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

stan_fit <- stan(
  "file" = stan_file, "data" = stan_data, 
  "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
)




## assess fit
################################################################################

str(stan_fit, 3)


get_posterior_mean(stan_fit)

stan_fit %>% rstan::extract(permuted = FALSE, inc_warmup = TRUE)
stan_fit %>% as.array() %>% bayesplot::mcmc_dens()






