## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("bench")
library("here")
library("rjags"); library("runjags")
library("R2OpenBUGS")
library("nimble")
library("rstan"); rstan_options(auto_write = FALSE)


## JAGS Code
################################################################################

theta <- 5 # poisson theta
set.seed(1)
(y <- rpois(1, theta))
jags_data <- list(
  "y" = y
)
jags_model <- "
  model{
    y ~ dpois(theta)
    theta ~ dgamma(3,1)
  }
"
jags_monitor <- c("theta")

## BUGS Code
################################################################################

theta <- 5 # poisson theta
set.seed(1)
(y <- rpois(1, theta))
bugs_data <- list(
  "y" = y
)
bugs_model <- function() {
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
}
bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)
bugs_monitor <- "theta"

if (getwd() == "/Users/evanmiyakawa/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods") {
  WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
  WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
  OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~
} else {
  WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
  WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
  OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"
}
## Nimble Code
################################################################################

theta <- 5 # poisson theta
set.seed(1)
(y <- rpois(1, theta))
nimble_data <- list(
  "y" = y
)
nimble_model <- nimbleCode({
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
})
nimble_monitor <- c("theta")
nimble_inits <- list(
  "theta" = rgamma(1,3,1)
)

## STAN Code
################################################################################

theta <- 5 # poisson theta
set.seed(1)
(y <- rpois(1, theta))
stan_data <- list(
  "theta" = theta,
  "y" = y
)
stan_file <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma.stan")
# file.show(stan_file)

## Set Parameters
################################################################################
n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

## benchmarking
###################################################################################

num_iterations <- 1 # Number of times you want to benchmark each model\

rds_file_location <- here("simple models", "discrete", "poisson", "gamma-prior", "poisson-gamma.rds")

source(here("benchmark-function.R"))

# choose to include stan compilation time with argument "stan_compile = TRUE"
run_benchmark(rds_file_location)
run_benchmark(rds_file_location, stan_compile = TRUE)




