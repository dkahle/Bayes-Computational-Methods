## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)



## generate/specify data
################################################################################

n <- 10L # binomial n
p <- .25 # binomial p

set.seed(1)

(y <- rbinom(1, n, p))

stan_data <- list(
  "n" = n,
  "p" = p,
  "y" = y
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("simple models", "discrete", "binomial", "beta-prior", "beta-binomial.stan")
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
stan_dens(fit) + theme_bw()
stan_fit %>% as.array() %>% bayesplot::mcmc_dens()



## assess convergence issues 
###################################################################################

fit %>% as.array() %>% mcmc_acf_bar()
fit %>% as.array() %>% mcmc_pairs()
fit %>% as.array() %>% mcmc_trace()

# see each chain
stan_fit %>% rstan::extract(permuted = FALSE, inc_warmup = TRUE)

