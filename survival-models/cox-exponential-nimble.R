## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("nimble")
library("bench")
library("survival")



## generate/specify data
################################################################################

data(veteran)
vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))
vet <- filter(vet, status == 1)

y <- vet$time
trt <- vet$trt %>% as.numeric() - 1

nimble_data <- list(
  "t" = y,
  "x" = trt
)

nimble_constants <- list(
  "N" = length(y)
)


## specify jags model
################################################################################

nimble_model <- nimbleCode({
  for (i in 1:N) {
    lambda[i] <- exp(beta * x[i])
    t[i] ~ dexp(lambda[i])
  }
  beta ~ dnorm(0,0.0001)
})

nimble_monitor <- c("beta")

## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

nimble_inits <- list(
  "beta" = rnorm(1,0,1)
)


## fit model
################################################################################
if (is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]])) {
  
  nimble_fit <- nimbleMCMC(
    "code" = nimble_model, "data" = nimble_data, "constants" = nimble_constants,
    "inits" = nimble_inits, "monitors" = nimble_monitor, "nchains" = n_chains, 
    "niter" = n_iter, "nburnin" = n_warmup, "summary" = TRUE
  )
  
  
  ## assess fit
  ################################################################################
  
  nimble_fit$summary$all.chains
  
  
  
  ## assess convergence issues 
  ###################################################################################
  
}  



