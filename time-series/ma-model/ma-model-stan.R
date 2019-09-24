## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)
library("bayesplot")



## generate/specify data
################################################################################

t <- 20L # sample size

set.seed(1)
mu <- rcauchy(1,0,2.5)
sigma <- rcauchy(1,0,2.5)
theta <- rcauchy(2,0,2.5)

arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          sd = sqrt(0.1796))

y <- rnorm(20)

stan_data <- list(
  "T" = t,
  "y" = y
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("time-series", "ma-model", "ma-model.stan")
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

