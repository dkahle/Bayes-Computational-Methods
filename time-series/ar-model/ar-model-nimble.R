## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("here")



## generate/specify data
################################################################################

N <- 200L # sample size
alpha <- 0 # ar(1) mean
beta <- c(-0.6) # ar(1) coefficient
sigma <- 1 # ar(1) standard deviation


set.seed(1)
(y <- arima.sim(model = list(ar = beta), sd = sigma, n = N)  %>% as.numeric())

t <- 1:length(y)
ggplot(data.frame(t = t, y = y), aes(t,y)) + geom_line()


nimble_data <- list(
  "y" = y
)

nimble_constants <- list(
  "N" = N
)


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  for (i in 2:N) {
    y_hat[i] <- alpha + beta * y[i-1]
    y[i] ~ dnorm(y_hat[i], tau)
  }
  alpha ~ dnorm(0,0.0001)
  beta ~ dnorm(0,0.0001)
  tau ~ T(dnorm(0,0.0001),0,)
  sigma <- sqrt(1 / tau)
})

nimble_monitor <- c("alpha", "beta", "sigma")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "alpha" = rnorm(1,0,1000),
  "beta" = rnorm(1,0,1000),
  "tau" = rnorm(1,0,0.001) %>% abs()
)


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
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



