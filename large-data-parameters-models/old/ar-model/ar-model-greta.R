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

N <- 200L # sample size
alpha <- 0 # ar(1) mean
beta <- c(-0.6) # ar(1) coefficient
sigma <- 1 # ar(1) standard deviation


set.seed(1)
(y <- arima.sim(model = list(ar = beta), sd = sigma, n = N) %>% as.numeric())

t <- 1:length(y)
ggplot(data.frame(t = t, y = y), aes(t,y)) + geom_line()

y_1 <- y[1:(length(y) - 1)] # data excluding the last point
z_1 <- y[2:length(y)] # data excluding the first point

y <- as_data(y)
y_1 <- as_data(y_1) # 
z_1 <- as_data(z_1)


## specify greta model
################################################################################

alpha <- normal(0,1000)
beta <- normal(0,1000)
sigma <- normal(0,1000, truncation = c(0,Inf))
distribution(y) <- normal(alpha + beta * y, sigma)
distribution(z_1) <- normal(alpha + beta * y_1, sigma)

greta_model <- model(alpha, beta, sigma)

plot(greta_model)

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

