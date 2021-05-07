## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
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

x <- as_data(x)
y <- as_data(y)



## specify greta model
################################################################################



alpha <- normal(0, 1000)
beta <- normal(0, 1000, dim = c(m))

theta_0 <- alpha + x %*% beta
theta <- exp(theta_0) / (1 + exp(theta_0))
distribution(y) <- bernoulli(ilogit(theta))

greta_model <- model(alpha, beta)

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
  
  greta_fit_object <- greta_fit %>% as.array()
  dim(greta_fit_object) <- c(dim(greta_fit_object), 1)
  dimnames(greta_fit_object) <- list(
    "iterations" = NULL, 
    "chains" = 1:n_chains, 
    "parameters" = c("theta")
  )
  
  
  greta_fit_object %>% mcmc_areas()
  greta_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  greta_fit_object %>% mcmc_acf_bar()
  greta_fit_object %>% mcmc_trace()
  greta_fit_object %>% mcmc_hist_by_chain()
  
}

