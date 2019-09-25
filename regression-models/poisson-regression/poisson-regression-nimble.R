## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")



## generate/specify data
################################################################################

n <- 20L # sample size
alpha <- 2 # intercept
beta <- 2 # single coefficient

set.seed(1)

(x <- runif(n, 0, 10)) # observed x values
lambda <- alpha + beta * x 
(y <- rpois(n,lambda))

nimble_data <- list(
  "n" = n,
  "y" = y, 
  "x" = x
)




## specify jags model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:n) {
    log(lambda[i]) <- alpha + beta * x[i]
    y[i] ~ dpois(lambda[i])
  }
  alpha ~ dnorm(0, 1 / (100 ^ 2))
  beta ~ dnorm(0, 1 / (100 ^ 2))
})

monitors = c("alpha", "beta")


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

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



## benchmarking
###################################################################################

bench_results <- mark(
  nimble_fit <- nimbleMCMC(
    "code" = nimble_model, "data" = nimble_data, 
    "inits" = nimble_inits, "monitors" = monitors, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  ),
  iterations = 3
)
bench_results[1,2:9]




