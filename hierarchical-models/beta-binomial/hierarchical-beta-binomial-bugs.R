## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("R2OpenBUGS")
library("bench")
library("here")



## generate/specify data
################################################################################

alpha <- 2 #beta distribution alpha
beta <- 1 #beta distribution beta
N <- 10 # sample size

set.seed(1)

(n <- rbinom(N, 20, 0.5))
(theta <- rbeta(N, alpha, beta))
(y <- rbinom(N, n, theta))


bugs_data <- list(
  "y" = y,
  "N" = N,
  "n" = n
)



## specify bugs model
################################################################################

bugs_model <- function() {
  for (i in 1:N) {
    theta[i] ~ dbeta(2,1)
    y[i] ~ dbin(theta[i], n[i])
  }
}


bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)

bugs_monitor <- "theta"


## Specify path to WINE if using WINE 
###################################################################################

if (getwd() == "/Users/evanmiyakawa/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods") {
  WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
  WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
  OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~
} else {
  WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
  WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
  OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"
}


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  bugs_fit <- bugs(
    "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = bugs_monitor, 
    "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
    "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
    "useWINE" = T
  )
  
  
  
  ## assess fit
  ################################################################################
  
  bugs_fit$summary
  
  bugs_fit_object <- bugs_fit$sims.array[,,1]
  dim(bugs_fit_object) <- c(dim(bugs_fit_object), 1)
  dimnames(bugs_fit_object) <- list(
    "iterations" = NULL, 
    "chains" = 1:n_chains, 
    "parameters" = bugs_monitor
  )
  
  
  bugs_fit_object %>% mcmc_areas()
  bugs_fit_object %>% mcmc_intervals()
  
  
  ## assess convergence issues 
  ###################################################################################
  
  bugs_fit_object %>% mcmc_acf_bar()
  bugs_fit_object %>% mcmc_trace()
  bugs_fit_object %>% mcmc_hist_by_chain()
  
}

