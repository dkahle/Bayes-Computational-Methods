library(tidyverse)
library(here)
setwd("~/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods/8-Schools")

library(rjags)
library(runjags)

# Jags uses tau = 1 / sigma^2
model <- "
  model{
    for (i in 1:J) {
      eta[i] ~ dnorm(0,1)
      theta[i] <- mu + tau * eta[i]
      y[i] ~ dnorm(theta[i], sigma[i])
    }
    mu ~ dnorm(0,0.0001)
    tau ~ dunif(0,1000)
  }
"

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
# inits <- list(list(theta = 0.5))
monitor <- c("mu", "tau", "eta", "theta")

results <- run.jags(model, data = schools_dat, monitor = monitor, 
                    burnin = 1000, sample = 10000, n.chains = 4)

results
