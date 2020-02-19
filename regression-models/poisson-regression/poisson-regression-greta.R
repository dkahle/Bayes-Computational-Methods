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

n <- 20L # sample size
alpha <- 2 # intercept
beta <- 2 # single coefficient

set.seed(1)

(x <- runif(n, 0, 10)) # observed x values
lambda <- alpha + beta * x 
(y <- rpois(n,lambda))

x <- as_data(x)
y <- as_data(y)



## specify greta model
################################################################################

alpha <- normal(0, 1000)
beta <- normal(0, 1000)

lambda <- exp(alpha + beta * x)
# distribution(y) <- poisson(log(alpha + beta * x))
distribution(y) <- poisson(lambda)

greta_model <- model(alpha, beta, lambda)

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

