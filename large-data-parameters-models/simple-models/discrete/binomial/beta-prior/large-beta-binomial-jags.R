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

p <- .25 # binomial p
n <-  10 # binomial n
N <-  1e5 # sample size

set.seed(1)

(y <- rbinom(N, n, p))

jags_data <- list(
  "y" = y,
  "n" = n,
  "N" = N
)



## specify jags model
################################################################################

jags_model <- "
  model{
    for (i in 1:N) {
      y[i] ~ dbin(p,n)
    }
    p ~ dbeta(1,1)
  }
"

jags_monitor <- c("p")


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

