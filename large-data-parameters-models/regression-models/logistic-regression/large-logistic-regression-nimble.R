## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
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


nimble_data <- list(
  "y" = y, 
  "x" = x
)

nimble_constants <- list(
  "n" = n,
  "m" = m
)




## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:n) {
    logit(theta[i]) <- alpha + (x[i,1:m] %*% beta[1:m,1])[1,1]
    y[i] ~ dbern(theta[i])
  }
  alpha ~ dnorm(0,0.0001)
  for (i in 1:m) {
    beta[i,1] ~ dnorm(0,0.0001)
  }
  
})

nimble_monitor <- c("alpha", "beta")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "alpha" = rnorm(1,0,1),
  "beta" = matrix(rnorm(m,0,1), ncol = 1)
)


## fit model
################################################################################
if (is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]])) {
  
  nimble_fit <- nimbleMCMC(

    "code" = nimble_model, "data" = nimble_data, "constants" = nimble_constants,
    "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  
  
  ## assess fit
  ################################################################################
  
  nimble_fit$summary$all.chains
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
}  





