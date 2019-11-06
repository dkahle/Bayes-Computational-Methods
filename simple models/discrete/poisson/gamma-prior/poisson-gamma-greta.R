## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
library("bayesplot")
library("bench")

install_tensorflow(
  version = "1.14.0",
  extra_packages = "tensorflow-probability"
)

install_tensorflow(
  version = "1.14.0"
)

install_tensorflow(method = "conda")
reticulate::conda_install("r-tensorflow", "tensorflow-probability", pip = TRUE)
install_tensorflow(extra_packages = "tensorflow-probability")
# nose, tornado

## generate/specify data
################################################################################

theta <- 5 # poisson theta

set.seed(1)

(y <- rpois(1, theta))

jags_data <- list(
  "y" = y
)


## specify jags model
################################################################################

jags_model <- "
  model{
    y ~ dpois(theta)
    theta ~ dgamma(3,1)
  }
"

jags_monitor <- c("theta")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


## fit model
################################################################################
if (is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]])) {
  
  
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
    "parameters" = monitor
  )
  
  
  jags_fit_object %>% bayesplot::mcmc_dens()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  jags_fit_object %>% mcmc_acf_bar()
  jags_fit_object %>% mcmc_trace()
  
  
  
  
}

