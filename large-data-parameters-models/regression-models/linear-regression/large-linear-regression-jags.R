## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")
library("bayesplot")
library("bench")
library("here")
library("MASS")



## generate/specify data
################################################################################

n <- 500L # sample size
alpha <- -5 # intercept
m <- 50 # number of coefficients
sigma <- 3 # standard deviation

set.seed(1)

beta <- rnorm(m) # single coefficient

(x <- mvrnorm(n, rep(5, m), diag(sigma, m))) # observed x values
y_hat <- as.numeric(alpha + x %*% beta)
(y <- rnorm(n,y_hat,sigma))

jags_data <- list(
  "N" = n,
  "M" = m,
  "y" = y,
  "x" = x
)


## specify jags model
################################################################################

jags_model <- "
  model{
    for (i in 1:N) {
      y_hat[i] <- alpha + x[i,] %*% beta
      y[i] ~ dnorm(y_hat[i], tau)
    }
    alpha ~ dnorm(0,0.0001)
    for (i in 1:M) {
      beta[i] ~ dnorm(0,0.0001)
    }
    tau ~ dnorm(0,0.0001)  I(0, )
    sigma <- sqrt(1 / tau)
  }
"

jags_monitor <- c("alpha", "beta", "sigma")


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
  
  
  jags_fit_object %>% mcmc_areas()
  jags_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  jags_fit_object %>% mcmc_acf_bar()
  jags_fit_object %>% mcmc_trace()
  jags_fit_object %>% mcmc_hist_by_chain()
  
  
  
  
}

