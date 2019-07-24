## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")



## generate/specify data
################################################################################

theta <- 5 # poisson theta

set.seed(1)

(y <- rpois(1, theta))

nimble_data <- list(
  "y" = y
)


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
})

monitors <- c("theta")


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "theta" = rgamma(1,3,1)
)

nimble_fit <- nimbleMCMC(
  "code" = nimble_model, "data" = nimble_data, 
  "inits" = nimble_inits, "monitors" = monitors, "nchains" = n_chains, 
  "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
)


## assess fit
################################################################################

nimble_fit$summary$all.chains



## assess convergence issues 
###################################################################################















library(nimble)

rCode <- nimbleCode({
  for (i in 1:10) {
    y[i] ~ dnorm(mu, 1 / tau)
  }
  mu ~ dnorm(0,1)
})

data <- list(y = c(3,  2, 5,  1, -2, 3, 2, 8, -4, 6), tau = 0.5)
inits <- list(mu = 0)
monitors <- c("mu")

results <- nimbleMCMC(rCode, data = data, inits = inits, monitors = monitors, 
                      nchains = 4, niter = 11000, nburnin = 1000, summary = TRUE)
results$summary$all.chains









library(nimble)

rCode <- nimbleCode({
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
})

data <- list(y = 5)
inits <- list(theta = 0.5)

results <- nimbleMCMC(rCode, data = data, inits = inits, monitors = c("theta"), 
                      nchains = 4, niter = 11000, nburnin = 1000, summary = TRUE)
results$summary$all.chains
