## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("here")



## generate/specify data
################################################################################

theta <- 5 # poisson theta

set.seed(1)

(y <- rpois(1, theta))

nimble_data <- list(
  "y" = y
)

nimble_constants <- list()


## specify nimble model
################################################################################

nimble_model <- nimbleCode({
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
})

nimble_monitor <- c("theta")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "theta" = rgamma(1,3,1)
)


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  ## All-in-one method # 16 seconds
  ##############################
  tictoc::tic()
  nimble_fit <- nimbleMCMC(
    "code" = nimble_model, "data" = nimble_data, "constants" = nimble_constants,
    "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  tictoc::toc()
  
  ## Step-by-step method allowing for saving objects
  ##############################
  
  nimble_model <- nimbleModel(
    "code" = nimble_model, 
    "data" = nimble_data, 
    "constants" = nimble_constants,
    "inits" = nimble_inits
  )
  
  nimble_MCMC <- buildMCMC(nimble_model)
  nimble_compile <- compileNimble(nimble_model)
  nimble_compile_mcmc <- compileNimble(nimble_MCMC, "project" = nimble_compile)
  
  file_location <- here("simple-models", "discrete", "poisson", "gamma-prior", "nimble.RDS")
  saveRDS(nimble_compile_mcmc, file = file_location, compress = F)
  
  
  nimble_compile_mcmc <- readRDS(file_location)
  
  # run with compiled mcmc object
  nimble_fit <- runMCMC(
    "mcmc" = nimble_compile_mcmc,
    "inits" = nimble_inits, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  
  # run without compiled object # 344 seconds (20x slower than compiling)
  tictoc::tic()
  nimble_fit <- runMCMC(
    "mcmc" = nimble_MCMC,
    "inits" = nimble_inits, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  tictoc::toc()
  
  
  
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
  


