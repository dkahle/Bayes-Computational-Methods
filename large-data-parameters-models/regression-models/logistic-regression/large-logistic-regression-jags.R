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
alpha <- 0 # intercept
m <- 10 # number of coefficients

set.seed(1)

beta <- rnorm(m, 0, 0.1) # single coefficient

(x <- mvrnorm(n, rep(0, m), diag(0.5, m))) # observed x values

i <- 1
gen_theta_0 <- function(i) {
  rnorm(1, alpha, 0.01) + sum(x %*% mvrnorm(1, beta, diag(0.01, m)))
}

(theta_0 <- 1:n %>% map_dbl(gen_theta_0))

# theta_0 <- rnorm(n,alpha,0.5) + mvrnorm(1,beta, diag(m, 0.5)) %*% x 

(theta <- exp(theta_0) / (1 + exp(theta_0))) # generated values of bernoulli theta
(y <- rbinom(n,1,theta))


jags_data <- list(
  "n" = n,
  "m" = m,
  "y" = y, 
  "x" = x
)



## specify jags model
################################################################################

jags_model <- "
  model{
    for (i in 1:n) {
      logit(theta[i]) <- alpha + x[i,] %*% beta
      y[i] ~ dbern(theta[i])
    }
    alpha ~ dnorm(0,(1 / 1000^2))
    for (i in 1:m) {
      beta[i] ~ dnorm(0,(1 / 1000^2))
    }
  }
"

jags_monitor <- c("alpha", "beta")


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
    "parameters" = monitor
  )
  
  
  jags_fit_object %>% mcmc_areas()
  jags_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  jags_fit_object %>% mcmc_acf_bar()
  jags_fit_object %>% mcmc_trace()
  jags_fit_object %>% mcmc_hist_by_chain()
  
  
  
  
}
