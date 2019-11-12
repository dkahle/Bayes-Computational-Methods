## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
library("bayesplot")
library("bench")


## generate/specify data
################################################################################

p <- .25 # binomial p
n <-  10 # binomial n

set.seed(1)

(y <- rbinom(1, n, p))
y <- as_data(y)
n <- as_data(n)


## specify greta model
################################################################################

p <- beta(1,1)

distribution(y) <- binomial(n,p)

greta_model <- model(p)

plot(greta_model)

## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


## fit model
################################################################################
if (is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]])) {
  
  
  greta_fit <- mcmc(
    "model" = greta_model, 
    "n_samples" = n_iter,
    "warmup" = n_warmup,
    "chains" = n_chains
  )
  
  
  
  ## assess fit
  ################################################################################
  
  summary(greta_fit)
  
}

