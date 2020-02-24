## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")



## generate/specify data
################################################################################

n <- 30L # sample size
d <- 2L
l <- 3L
ll <- sapply(1:l, function(x) rep(x,n / l)) %>% as.numeric()

set.seed(1)
(x <- matrix(rnorm(n * d), nrow = n, byrow = TRUE))
(y <- rbinom(n, 1, 0.5))

nimble_data <- list(
  "y" = y, 
  "x" = x
)

nimble_constants <- list(
  "N" = n, 
  "D" = d,
  "L" = l,
  "ll" = ll
)


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (d in 1:D) {
    mu[d] ~ dnorm(0, 1 / (100 ^ 2))
    tau[d] ~ T(dnorm(0,0.0001), 0,)
    for (l in 1:L) {
      beta[l,d] ~ dnorm(mu[d], tau[d])
    }
  }
  for (n in 1:N) {
    for (d in 1:D) {
      temp[n,d] <- x[n,d] * beta[ll[n],d]
    }
    temp_sum[n] <- temp[n,1] + temp[n,2] 
    logit(theta[n]) <- temp_sum[n]
    y[n] ~ dbern(theta[n])
  }
})
nimble_monitor <- c("mu", "beta", "theta", "tau")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "mu" = rnorm(d,0,10),
  "theta" = rbeta(n,1,1),
  "tau" = rep(1 / (abs(rnorm(1,0,10)) ^ 2), d),
  "beta" = matrix(rnorm(l * d,0,10), nrow = l, ncol = d)
)


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  nimble_fit <- nimbleMCMC(
    "code" = nimble_model, "constants" = nimble_constants, "data" = nimble_data,
    "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  
  
  ## assess fit
  ################################################################################
  
  nimble_fit$summary$all.chains
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
}  


