## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")



## generate/specify data
################################################################################

mu <- 1    # normal mu
sigma <- 2 # normal sigma
n <- 10    # sample size

set.seed(1)

(y <- rnorm(n, mu, sigma))

jags_data <- list(
  "y" = y,
  "tau" = 1 / sigma ^2,
  "N" = n
)


## specify jags model
################################################################################

jags_model <- "
  model{
    for (i in 1:N) {
      y[i] ~ dnorm(mu, 1 / tau)
    }
    mu ~ dnorm(0, 1)
  }
"

monitor <- c("mu")


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



## assess convergence issues 
###################################################################################


