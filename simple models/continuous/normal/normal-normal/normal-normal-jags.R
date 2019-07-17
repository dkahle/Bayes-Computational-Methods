library(tidyverse)
library(rjags)
library(runjags)

# Jags uses tau = 1 / sigma^2

model <- "
  model{
    for (i in 1:J) {
      y[i] ~ dnorm(mu, 1 / tau)
    }
    mu ~ dnorm(0, 1)
  }
"

data <- list(J = 10, y = c(3,  2, 5,  1, -2, 3, 2, 8, -4, 6), tau = 0.5)
# inits <- list(list(theta = 0.5))
monitor <- c("mu")

results <- run.jags(model, data = data, monitor = monitor, 
                    burnin = 1000, sample = 10000, thin = 1, n.chains = 4)

results
