## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")
library("bayesplot")
library("bench")



## generate/specify data
################################################################################

n <- 20L # sample size
alpha <- 2 # intercept
beta <- 2 # single coefficient

set.seed(1)

(x <- runif(n, 0, 10)) # observed x values
lambda <- alpha + beta * x 
(y <- rpois(n,lambda))

jags_data <- list(
  "n" = n,
  "y" = y, 
  "x" = x
)



## specify jags model
################################################################################

jags_model <- "
  model{
    for (i in 1:n) {
      log(lambda[i]) <- alpha + beta * x[i]
      y[i] ~ dpois(lambda[i])
    }
    alpha ~ dnorm(0, 1 / (100 ^ 2))
    beta ~ dnorm(0, 1 / (100 ^ 2))
  }
"

monitor <- c("alpha", "beta")


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


## benchmarking
###################################################################################


bench_results <- mark(
  run.jags(
    "model" = jags_model, "data" = jags_data, "monitor" = monitor, 
    "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
  ),
  iterations = 3
)
bench_results[1,2:9]



