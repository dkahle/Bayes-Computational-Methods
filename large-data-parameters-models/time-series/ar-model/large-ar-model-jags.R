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

N <- 1e4L # sample size
alpha <- 0 # ar(1) mean
M <- 6 #Number of coefficients
beta <- c(-0.8, -0.4, 0.2, 0.15, 0.05, -0.03) # ar(6) coefficients
sigma <- rnorm(length(beta), 1, sd = 0.2) # ar(1) standard deviation


set.seed(1)
(y <- arima.sim(model = list(ar = beta), sd = sigma, n = N)  %>% as.numeric())
y <- c(
  rep(0, length(beta)),
  y
)

t <- 1:length(y)
ggplot(data.frame(t = t, y = y), aes(t,y)) + geom_line()


jags_data <- list(
  "N" = length(y),
  "M" = M,
  "y" = y
)


## specify jags model
################################################################################

### try step function instead of ifelse

jags_model <- "
  model{
    for (i in (M+1):N) {
      y_hat[i] <- alpha + sum(beta[1:M] * y[(i-M):(i-1)])
      y[i] ~ dnorm(y_hat[i], tau)
    }
    alpha ~ dnorm(0,0.0001)
    for (j in 1:M) {
      beta[j] ~ dnorm(0,0.0001)
    }
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
  
  
  jags_fit_object %>% mcmc_areas()
  jags_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  jags_fit_object %>% mcmc_acf_bar()
  jags_fit_object %>% mcmc_trace()
  jags_fit_object %>% mcmc_hist_by_chain()
  
  
  
  
}

