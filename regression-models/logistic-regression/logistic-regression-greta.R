## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
library("bayesplot")
library("bench")
library("here")


## generate/specify data
################################################################################

n <- 10L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient

set.seed(1)

x <- (rnorm(n, 5, 1)) # observed x values
theta_0 <- rnorm(n,alpha,0.5) + rnorm(n,beta,0.5) * x 
theta <- exp(theta_0) / (1 + exp(theta_0)) # generated values of bernoulli theta
y <- (rbinom(n,1,theta))

x <- as_data(x)
y <- as_data(y)



## specify greta model
################################################################################

p <- beta(1,1)
distribution(y) <- bernoulli(p)

greta_model <- model(p)

plot(greta_model)

## configure model settings
################################################################################

n_chains <- 4
n_iter <- 1e4L
n_warmup <- 1e3L



## fit model
################################################################################

source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  
  greta_fit <- mcmc(
    "model" = greta_model, "n_samples" = n_iter,
    "warmup" = n_warmup, "chains" = n_chains
  )
  
  
  
  ## assess fit
  ################################################################################
  
  summary(greta_fit)
  
}

