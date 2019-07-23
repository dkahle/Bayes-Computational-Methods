## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")



## generate/specify data
################################################################################

theta <- 5 # poisson theta

set.seed(1)

(y <- rpois(1, theta))

jags_data <- list(
  "theta" = theta,
  "y" = y
)


## specify jags model
################################################################################

jags_model <- "
  model{
    y ~ dpois(theta)
    theta ~ dgamma(3,1)
  }
"

monitor <- c("theta")
inits <- list(list(theta = 0.5))


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

jags_fit <- run.jags(
  "model" = jags_model, "data" = jags_data, inits = inits, "monitor" = monitor, 
  "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
) 



## assess fit
################################################################################

jags_fit



## assess convergence issues 
###################################################################################
















library(tidyverse)
library(rjags)
library(runjags)

model <- "
  model{
    y ~ dbin(theta,n)
    theta ~ dbeta(1,1)
  }
"

data <- list(y = 2, n = 10)
inits <- list(list(theta = 0.5))
monitor <- c("theta")

results <- run.jags(model, data = data, monitor = monitor, 
                    burnin = 1000, sample = 10000, n.chains = 4)

results












library(tidyverse)

library(rjags)
library(runjags)

model <- "
  model{
    y ~ dpois(theta)
    theta ~ dgamma(3,1)
  }
"

data <- list(y = 5)
inits <- list(list(theta = 0.5))
monitor <- c("theta")

results <- run.jags(model, data = data, monitor = monitor, inits = inits, 
                    burnin = 1000, sample = 10000, n.chains = 4)

results
