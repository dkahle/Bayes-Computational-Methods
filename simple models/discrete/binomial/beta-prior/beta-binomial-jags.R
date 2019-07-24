## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")



## generate/specify data
################################################################################

p <- .25 # binomial p
n <-  10 # binomial n

set.seed(1)

(y <- rbinom(1, n, p))

jags_data <- list(
  "y" = y,
  "n" = n
)



## specify jags model
################################################################################

jags_model <- "
  model{
    y ~ dbin(theta,n)
    p ~ dbeta(1,1)
  }
"

monitor <- c("p")


## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

jags_fit <- run.jags(
  "model" = jags_model, "data" = jags_data, "monitor" = monitor, 
  "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
) 



## assess fit
################################################################################

jags_fit



## assess convergence issues 
###################################################################################






