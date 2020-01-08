library(tidyverse)
library(rjags)
library(runjags)

# data 

theta <- 0.8
n <- 20
(y <- rbinom(n, 1, theta))

jags_data <- list(
  y = y, 
  n = n
)

# model

jags_model <- "
  model{
    for (i in 1:n) {
      y[i] ~ dbern(theta)
    }
    theta ~ dbeta(1,1)
  }
"

jags_monitor <- "theta"

# sample

run.jags(jags_model, jags_monitor, jags_data, n.chains = 4, burnin = 1000,
         sample = 10000)


library(nimble)

nimble_data <- list(
  y = y
)

nimble_constants <- list(
  n = n
)

nimble_model <- nimbleCode({
  for (i in 1:n) {
    y[i] ~ dbern(theta)
  }
  theta ~ dbeta(1,1)
})



nimble_inits <- list(
  "theta" = rbeta(1,1,1)
)

nimble_monitor <- "theta"

nim <- nimbleMCMC(nimble_model, constants = nimble_constants, data = nimble_data, 
           inits = nimble_inits, niter = 10000, nburnin = 1000, nchains = 4, monitors = nimble_monitor, 
           summary = TRUE)



library(rstan)
library(here)

stan_data <- list(
  y = y, 
  n = n
)

stan_file <- here("example-model-implementation.stan")

st <- stan(stan_file, data = stan_data, chains = 4, iter = 10000, warmup = 1000)


library(greta)

y <- as_data(y)

theta <- beta(1,1)
distribution(y) <- poisson(theta)
greta_model <- model(theta)
plot(greta_model)

mcmc(greta_model, n_samples = 10000, warmup = 1000, chains = 4)
