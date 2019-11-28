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

N <- 200L # sample size
alpha <- 0 # ar(1) mean
beta <- c(-0.6) # ar(1) coefficient
sigma <- 1 # ar(1) standard deviation


set.seed(1)
(y <- arima.sim(model = list(ar = beta), sd = sigma, n = N)) %>% as.numeric()

t <- 1:length(y)
ggplot(data.frame(t = t, y = y), aes(t,y)) + geom_line()


jags_data <- list(
  "N" = N,
  "y" = y
)


## specify jags model
################################################################################

jags_model <- "
  model{
    for (i in 2:N) {
      y[i] ~ dnorm(alpha + beta * y[i-1], tau)
    }
    alpha ~ dnorm(0,0.0001)
    beta ~ dnorm(0,0.0001)
    tau ~ dnorm(0,0.0001)  I(0, )
    sigma <- sqrt(1 / tau)
  }
"

jags_monitor <- c("alpha", "beta", "sigma")


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
    "parameters" = jags_monitor
  )
  
  
  jags_fit_object %>% bayesplot::mcmc_dens()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  jags_fit_object %>% mcmc_acf_bar()
  jags_fit_object %>% mcmc_trace()
  
  
  
  
}

