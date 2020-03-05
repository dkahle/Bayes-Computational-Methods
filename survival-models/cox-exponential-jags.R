## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")
library("bayesplot")
library("bench")
library("here")
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

jags_data <- list(
  "t" = t,
  "t_censor" = t_censor,
  "x" = x,
  "num_uncensored" = num_uncensored,
  "start_censored" = num_uncensored + 1,
  "N" = length(t)
)



## specify jags model
################################################################################

# jags_model <- "
#   model{
#     for (i in 1:N) {
#       is_censored[i] ~ dinterval(t[i], t_censor[i])
#       lambda[i] <- exp(beta * x[i])
#       t[i] ~ dexp(lambda[i])
#     }
#     beta ~ dnorm(0,0.0001)
#   }
# "

jags_model <- "
  model{
    for (i in 1:num_uncensored) {
      lambda[i] <- exp(beta * x[i])
      t[i] ~ dexp(lambda[i])
    }
    for (i in start_censored:N) {
      lambda[i] <- exp(beta * x[i])
      t[i] ~ dexp(lambda[i]) T(t_censor[i],)
    }
    beta ~ dnorm(0,0.0001)
  }
"

jags_monitor <- c("beta")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

t_inits <- t_censor + 1
t_inits[1:num_uncensored] <- NA
jags_inits <- list("t" = t_inits)


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  
  jags_fit <- run.jags(
    "model" = jags_model, "data" = jags_data, "monitor" = jags_monitor, "inits" = jags_inits,
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
  
  
  jags_fit_object %>% bayesplot::mcmc_dens()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  jags_fit_object %>% mcmc_acf_bar()
  jags_fit_object %>% mcmc_trace()
  
  
  
  
}

