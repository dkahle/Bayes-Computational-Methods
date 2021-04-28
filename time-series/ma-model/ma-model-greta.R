## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
library("bayesplot")
library("bench")
library("here")


## generate/specify data
################################################################################

N <- 50L # sample size
theta <- c(0.6)
sigma <- 1

set.seed(1)
(y <- arima.sim(model = list(ma = theta), sd = sigma, n = N) %>% as.numeric())

# epsilon <- y
epsilon <- y * 1.01

t <- 1:length(y)
# ggplot(data.frame(t = t, y = y), aes(t,y)) + geom_line()

y_1 <- y[1]
y_2 <- y[2:length(y)]

y <- as_data(y)
y_1 <- as_data(y_1) # first element
y_2 <- as_data(y_2)


## specify greta model
################################################################################

mu <- normal(0,1000)
theta <- normal(0,1000)
sigma <- normal(0,1000, truncation = c(0,Inf))

epsilon <- greta_array(dim = c(N,1))
epsilon[1] <- 0
epsilon[2] <- y[1] - mu
for (i in 3:N) {
  epsilon[i] <- y[i] - mu - theta * epsilon[i-1]
}

distribution(y) <- normal(mu + theta * epsilon, sigma)

greta_model <- model(mu, theta, sigma)

# plot(greta_model)

## configure model settings
################################################################################

n_chains <- 4
n_iter <- 1e4L
n_warmup <- 1e3L



## fit model
################################################################################

source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  
  greta_fit <- mcmc(
    "model" = greta_model, "n_samples" = n_iter,
    "warmup" = n_warmup, "chains" = n_chains
  )
  
  
  
  ## assess fit
  ################################################################################
  
  summary(greta_fit)
  
  greta_fit_object <- greta_fit %>% as.array()
  dim(greta_fit_object) <- c(dim(greta_fit_object), 1)
  dimnames(greta_fit_object) <- list(
    "iterations" = NULL, 
    "chains" = 1:n_chains, 
    "parameters" = c("theta")
  )
  
  
  greta_fit_object %>% mcmc_areas()
  greta_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  greta_fit_object %>% mcmc_acf_bar()
  greta_fit_object %>% mcmc_trace()
  greta_fit_object %>% mcmc_hist_by_chain()
  
}

