## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")



## generate/specify data
################################################################################

n <- 10L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient

set.seed(1)

(x <- rnorm(n, 5, 1)) # observed x values
theta_0 <- rnorm(n,alpha,0.5) + rnorm(n,beta,0.5) * x 
(theta <- exp(theta_0) / (1 + exp(theta_0))) # generated values of bernoulli theta
(y <- rbinom(n,1,theta))


nimble_data <- list(
  "n" = n,
  "y" = y, 
  "x" = x,
)




## specify jags model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:n) {
    logit(theta[i]) <- alpha + beta * x[i]
    y[i] ~ dbern(theta[i])
  }
  alpha ~ dnorm(0,(1 / 1000^2))
  beta ~ dnorm(0,(1 / 1000^2))
})

monitors = c("alpha", "beta")


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- rep(
  list(
    "alpha" = rnorm(1,0,(1 / 1000^2)),
    "beta" = rnorm(1,0,(1 / 1000^2))
  ), 
  times = n_chains
)

nimble_inits <- list(
  "alpha" = rnorm(1,0,(1 / 1000^2)),
  "beta" = rnorm(1,0,(1 / 1000^2))
)
  

nimble_fit <- nimbleMCMC(
  "code" = nimble_model, "data" = nimble_data, "inits" = nimble_inits, 
  "monitors" = monitors, "nchains" = n_chains, "niter" = n_iter, 
  "nburnin" = n_warmup, "summary" = TRUE
)


## assess fit
################################################################################

nimble_fit$summary$all.chains

str(nimble_fit$samples)
str(nimble_fit$samples %>% as.array())

## assess convergence issues 
###################################################################################






