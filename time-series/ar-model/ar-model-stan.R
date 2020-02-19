## load required packages and set basic options
################################################################################

library("here")
library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rstan"); rstan_options(auto_write = TRUE)
library("bayesplot")
library("bench")



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


stan_data <- list(
  "N" = N,
  "y" = y
)



## specify stan model
################################################################################

# read it in from file
stan_file <- here("time-series", "ar-model", "ar-model.stan")
# file.show(stan_file)

## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


## fit model
################################################################################
if (is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]])) {
  
  stan_fit <- stan(
    "file" = stan_file, "data" = stan_data, 
    "chains" = n_chains, "iter" = n_iter, "warmup" = n_warmup
  )
  
  
  
  
  ## assess fit
  ################################################################################
  
  summary(stan_fit)$summary
  get_posterior_mean(stan_fit)
  stan_dens(stan_fit) + theme_bw()
  stan_fit %>% as.array() %>% bayesplot::mcmc_dens()
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
  stan_fit %>% as.array() %>% mcmc_acf_bar()
  stan_fit %>% as.array() %>% mcmc_pairs()
  stan_fit %>% as.array() %>% mcmc_trace()
  
  # see each chain
  stan_fit %>% rstan::extract(permuted = FALSE, inc_warmup = TRUE)
  
}


