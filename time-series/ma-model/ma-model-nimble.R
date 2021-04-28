## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("here")



## generate/specify data
################################################################################

N <- 50 # sample size
theta <- c(0.6)
sigma <- 1

set.seed(1)
(y <- arima.sim(model = list(ma = theta), sd = sigma, n = N) %>% as.numeric())

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
  epsilon[1] <- y[1] - mu
  for (i in 2:N) {
    epsilon[i] <- y[i] - mu - theta * epsilon[i - 1]
    y_hat[i] <- mu + theta * epsilon[i - 1]
    y[i] ~ dnorm(y_hat[i], tau)
  }
  mu ~ dnorm(0,0.0001)
  theta ~ dnorm(0,0.0001)
  tau ~ T(dnorm(0,0.0001),0,)
  sigma <- sqrt(1 / tau)
})

nimble_monitor <- c("mu", "theta", "sigma")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "mu" = rnorm(1,0,1),
  "theta" = rnorm(1,0,1),
  "tau" = rnorm(1,0,1) %>% abs()
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
  
  nimble_fit_vector <- nimble_fit$samples %>% as_vector()
  nimble_fit_object <- array(nimble_fit_vector, 
                             dim = c(length(nimble_fit_vector) / n_chains, n_chains))
  dim(nimble_fit_object) <- c(dim(nimble_fit_object), 1)
  dimnames(nimble_fit_object) <- list(
    "iterations" = NULL, 
    "chains" = 1:n_chains, 
    "parameters" = nimble_monitor
  )
  
  
  nimble_fit_object %>% mcmc_areas()
  nimble_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  nimble_fit_object %>% mcmc_acf_bar()
  nimble_fit_object %>% mcmc_trace()
  nimble_fit_object %>% mcmc_hist_by_chain()
  
  
}  



