library(tidyverse)
library(rjags)
library(runjags)

model <- "
  model{
    y ~ dbin(theta,10)
    theta ~ dbeta(1,1)
  }
"

data <- list(y = 2)
inits <- list(list(theta = 0.5))
monitor <- c("theta")

results <- run.jags(model, data = data, monitor = monitor, inits = inits, 
                    burnin = 1000, sample = 10000, n.chains = 4)

results
