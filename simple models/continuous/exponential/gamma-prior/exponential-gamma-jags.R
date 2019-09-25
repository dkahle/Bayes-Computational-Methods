## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")
library("bayesplot")
library("bench")



## generate/specify data
################################################################################

lambda <- 1/2 # exponential lambda
n <- 10       # sample size

set.seed(1)

(y <- rexp(n, lambda))

jags_data <- list(
  "y" = y,
  "N" = n
)


## specify jags model
################################################################################

jags_model <- "
  model{
    for (i in 1:N) {
      y[i] ~ dexp(theta)
    }
    lambda ~ dgamma(1,1)
  }
"

monitor <- c("lambda")


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

jags_fit <- run.jags(
  "model" = jags_model, "data" = jags_data, "monitor" = monitor, 
  "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
) 



## assess fit
################################################################################

jags_fit

jags_fit_object <- jags_fit$mcmc %>% as.array()
dim(jags_fit_object) <- c(dim(jags_fit_object), 1)
dimnames(jags_fit_object) <- list(
  "iterations" = NULL, 
  "chains" = 1:n_chains, 
  "parameters" = monitor
)


jags_fit_object %>% bayesplot::mcmc_dens()


## assess convergence issues 
###################################################################################

jags_fit_object %>% mcmc_acf_bar()
jags_fit_object %>% mcmc_trace()



## benchmarking
###################################################################################


bench_results <- mark(
  run.jags(
    "model" = jags_model, "data" = jags_data, "monitor" = monitor, 
    "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
  ),
  iterations = 3
)
bench_results[1,2:9]

