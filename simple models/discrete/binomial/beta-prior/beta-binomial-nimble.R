## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")



## generate/specify data
################################################################################

p <- .25 # binomial p
n <-  10 # binomial n

set.seed(1)

(y <- rbinom(1, n, p))

nimble_data <- list(
  "y" = y,
  "n" = n
)



## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  y ~ dbin(p,n)
  p ~ dbeta(1,1)
})

monitors = c("p")


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "p" = rbeta(1,1,1)
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



