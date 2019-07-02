library(tidyverse)
setwd("~/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods/Bernoulli")

library(rjags)
library(runjags)

model <- "
  model{
    y ~ dbern(theta)
    theta ~ dbeta(1,1)
  }
"

data <- list(y = 1)
inits <- list(list(theta = 0.5))
monitor <- c("theta")

results <- run.jags(model, data = data, monitor = monitor, inits = inits, 
                    burnin = 1000, sample = 10000, n.chains = 4)

results
