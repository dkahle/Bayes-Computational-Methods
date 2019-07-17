library(tidyverse)
setwd("~/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods/Poisson-Gamma")

library(rjags)
library(runjags)

model <- "
  model{
    y ~ dpois(theta)
    theta ~ dgamma(3,1)
  }
"

data <- list(y = 5)
inits <- list(list(theta = 0.5))
monitor <- c("theta")

results <- run.jags(model, data = data, monitor = monitor, inits = inits, 
                    burnin = 1000, sample = 10000, n.chains = 4)

results
