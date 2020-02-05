## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
library("bayesplot")
library("bench")
library("here")
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

t <- as_data(y)
x <- as_data(trt)
N <- as_data(length(y))



## specify greta model
################################################################################

beta <- normal(0,1000)
lambda <- exp(beta * x)
distribution(t) <- exponential(lambda)

greta_model <- model(beta)

plot(greta_model)

## configure model settings
################################################################################

n_chains <- 4
n_iter <- 1e4L
n_warmup <- 1e3L



## fit model
################################################################################

source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  
  greta_fit <- mcmc(
    "model" = greta_model, "n_samples" = n_iter,
    "warmup" = n_warmup, "chains" = n_chains
  )
  
  
  
  ## assess fit
  ################################################################################
  
  summary(greta_fit)
  
}

