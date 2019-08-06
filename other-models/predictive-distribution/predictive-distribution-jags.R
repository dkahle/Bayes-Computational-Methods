## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")
library("bayesplot")



## generate/specify data
################################################################################

n1 <- 100
theta1 <- 0.5
n2 <- 100
theta2 <- 0.5
m1 <- 100
m2 <- 100


set.seed(1)

(y1 <- rbinom(1, n1, theta1))
(y2 <- rbinom(1, n2, theta2))
    

jags_data <- list(
  "y1" = y1,
  "y2" = y2,
  "n1" = n1, 
  "n2" = n2, 
  "m1" = m1,
  "m2" = m2
)



## specify jags model
################################################################################

jags_model <- "
  model{
    y1 ~ dbin(theta1, n1)
    y2 ~ dbin(theta2, n2)
    theta1 ~ dbeta(1, 1)
    theta2 ~ dbeta(1, 1)
    prob1 <- step(theta1-theta2)
    y1tilde ~ dbin(theta1, m1)
    y2tilde ~ dbin(theta2, m2)
    prob2 <- step(y1tilde - y2tilde - 11)
  }
"

monitor <- c("theta1", "theta2", "prob1", "prob2", "y1tilde", "y2tilde")


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





