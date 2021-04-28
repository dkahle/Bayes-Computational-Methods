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

n <- 10000L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient
sigma <- 3 # standard deviation

set.seed(1)

(x <- rnorm(n, 5, 4)) # observed x values
y_hat <- alpha + beta * x
(y <- rnorm(n,y_hat,sigma))

x <- as_data(x)
y <- as_data(y)



## specify greta model
################################################################################

alpha <- normal(0, 1000)
beta <- normal(0, 1000)
sigma <- normal(0,1000, truncation = c(0,Inf))

distribution(y) <- normal(alpha + beta * x, sigma)

greta_model <- model(alpha, beta, sigma)

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

