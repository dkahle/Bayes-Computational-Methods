## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("R2OpenBUGS")
library("bench")
library("here")



## generate/specify data
################################################################################

n <- 30L # sample size
d <- 2L
l <- 2L
ll <- sapply(1:l, function(x) rep(x,n / l)) %>% as.numeric()

set.seed(1)
(x <- matrix(rnorm(n * d), nrow = n, byrow = TRUE))
(y <- rbinom(n, 1, 0.5))

bugs_data <- list(
  "N" = n,
  "D" = d,
  "L" = l,
  "ll" = ll,
  "y" = y, 
  "x" = x
)



## specify bugs model
################################################################################

bugs_model <- function() {
  for (d in 1:D) {
    mu[d] ~ dnorm(0, 0.0001)
    tau[d] ~ dnorm(0,0.0001) %_% I(0,)
    for (l in 1:L) {
      beta[l,d] ~ dnorm(mu[d], tau[d])
    }
  }
  for (n in 1:N) {
    for (d in 1:D) {
      temp[n,d] <- x[n,d] * beta[ll[n],d]
    }
    temp_sum[n] <- temp[n,1] + temp[n,2] 
    logit(theta[n]) <- temp_sum[n]
    y[n] ~ dbern(theta[n])
  }
}


bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)

bugs_monitor <- c("mu", "beta", "theta", "tau")


## Specify path to WINE if using WINE 
###################################################################################

if (getwd() == "/Users/evanmiyakawa/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods") {
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
    "useWINE" = T, debug = T
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



