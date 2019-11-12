## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
library("bayesplot")
library("bench")


## generate/specify data
################################################################################

theta <- 5 # poisson theta

set.seed(1)

(y <- rpois(1, theta))
y <- as_data(y)

# 
# jags_data <- list(
#   "y" = y
# )


## specify greta model
################################################################################

theta <- gamma(3,1)
distribution(y) <- poisson(theta)

greta_model <- model(theta)

plot(greta_model)

## configure model settings
################################################################################

n_chains <- 4
n_iter <- 1e4L
n_warmup <- 1e3L


## fit model
################################################################################
if (is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]])) {
  
  
  greta_fit <- mcmc(
    "model" = greta_model, "n_samples" = n_iter,
    "warmup" = n_warmup, "chains" = n_chains
  )
  
  
  
  ## assess fit
  ################################################################################
  
  summary(greta_fit)
  
}

