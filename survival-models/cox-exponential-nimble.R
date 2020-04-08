## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("survival")



## generate/specify data
################################################################################

data(veteran) # load spatial data set

# t <- veteran$time # observed / censored event times
# x <- veteran$trt # single covariate
# not_censored <- veteran$status # indicator variable for those who weren't censored
# not_censored <- rep(1, length(not_censored))
# is_censored <- 1 - not_censored # indicator variable for those who were censored
# 
# # Here we create variables that will allow us to handle censored data in the bayes model
# t_censor <- t + not_censored
# t[not_censored == 0] <- NA
# 
# 
# jags_data <- list(
#   "t" = t,
#   "x" = x,
#   "t_censor" = t_censor,
#   "is_censored" = is_censored,
#   "N" = length(t)
# )

t <- veteran$time # observed / censored event times
x <- veteran$trt # single covariate
not_censored <- veteran$status # indicator variable for those who weren't censored
is_censored <- 1 - not_censored # indicator variable for those who were censored
N <- length(t)

# Here we create variables that will allow us to handle censored data in the bayes model
num_uncensored <- sum(not_censored)
start_censored <- num_uncensored + 1
t <- t[order(is_censored)]
x <- x[order(is_censored)]
t_censor <- t
t[start_censored:N] <- NA
t_censor[1:num_uncensored] <- 0
censored <- rep(0, length(t))
censored[start_censored:N] <- 1

nimble_data <- list(
  "t" = t,
  "t_censor" = t_censor,
  "x" = x,
  "censored" = censored
)

nimble_constants <- list(
  "num_uncensored" = num_uncensored,
  "start_censored" = num_uncensored + 1,
  "N" = length(t)
)


## specify jags model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:num_uncensored) {
    lambda[i] <- exp(beta * x[i])
    t[i] ~ dexp(lambda[i])
  }
  for (i in start_censored:N) {
    censored[i] ~ dinterval(t[i], t_censor[i])
    lambda[i] <- exp(beta * x[i])
    t[i] ~ dexp(lambda[i])
  }
  beta ~ dnorm(0,0.0001)
})

nimble_monitor <- c("beta")

## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

t_inits <- t_censor + 1
t_inits[1:num_uncensored] <- NA
nimble_inits <- list(
  "beta" = rnorm(1,0,1),
  "t" = t_inits
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



