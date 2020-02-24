## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")
library("bayesplot")
library("bench")
library("here")



## generate/specify data
################################################################################

n <- 30L # sample size
d <- 2L
l <- 3L
ll <- sapply(1:l, function(x) rep(x,n / l)) %>% as.numeric()

set.seed(1)
(x <- matrix(rnorm(n * d), nrow = n, byrow = TRUE))
(y <- rbinom(n, 1, 0.5))

jags_data <- list(
  "N" = n,
  "D" = d,
  "L" = l,
  "ll" = ll,
  "y" = y, 
  "x" = x
)




## specify jags model
################################################################################

jags_model <- "
  model{
    for (d in 1:D) {
      mu[d] ~ dnorm(0, 1 / (100 ^ 2))
      tau[d] ~ dnorm(0,0.0001)  I(0, )
      for (l in 1:L) {
        beta[l,d] ~ dnorm(mu[d], tau[d])
      }
    }
    for (n in 1:N) {
      logit(theta[n]) <- x[n,] %*% beta[ll[n],]
      y[n] ~ dbern(theta[n])
    }
  }
"

jags_monitor <- c("mu", "beta", "theta", "tau")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L



## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  
  jags_fit <- run.jags(
    "model" = jags_model, "data" = jags_data, "monitor" = jags_monitor, 
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
    "parameters" = jags_monitor
  )
  
  
  jags_fit_object %>% bayesplot::mcmc_dens()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  jags_fit_object %>% mcmc_acf_bar()
  jags_fit_object %>% mcmc_trace()
  
  
  
  
}

