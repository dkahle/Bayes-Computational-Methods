## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)
library("bayesplot")



## generate/specify data
################################################################################

n <- 10L # binomial n
p <- .25 # binomial p

set.seed(1)

(y <- rbinom(1, n, p))

stan_data <- list(
  "n" = n,
  "y" = y
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("simple models", "continuous", "normal", "normal-prior", "normal-normal.stan")
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











library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_file <- here("simple models", "continuous", "normal", "normal-normal","normal-normal.stan")

data <- list(J = 10, y = c(3,  2, 5,  1, -2, 3, 2, 8, -4, 6), tau = 0.5)

fit <- stan(file = stan_file, data = data, warmup = 1000,
            iter = 11000, chains = 4)
print(fit)
