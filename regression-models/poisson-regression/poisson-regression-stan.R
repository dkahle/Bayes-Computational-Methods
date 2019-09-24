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

n <- 10L # sample size
alpha <- 5 # intercept
beta <- 1 # single coefficient

set.seed(1)

(x <- rnorm(n, 5, 1)) # observed x values
lambda <- rnorm(n,alpha,0.5) + rnorm(n,beta,0.5) * x 
(y <- rpois(n,lambda))


stan_data <- list(
  "N" = n,
  "y" = y, 
  "x" = x
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("regression-models", "poisson-regression", "poisson-regression.stan")
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

str(stan_fit, 2)

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



## benchmarking
###################################################################################


bench_results <- mark(
  stan_fit <- stan(
    "file" = stan_file, "data" = stan_data, 
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
  ),
  iterations = 3
)
bench_results[1,2:9]

