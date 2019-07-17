library(nimble)

rCode <- nimbleCode({
  for (i in 1:10) {
    y[i] ~ dnorm(mu, 1 / tau)
  }
  mu ~ dnorm(0,1)
})

data <- list(y = c(3,  2, 5,  1, -2, 3, 2, 8, -4, 6), tau = 0.5)
inits <- list(mu = 0)
monitors <- c("mu")

results <- nimbleMCMC(rCode, data = data, inits = inits, monitors = monitors, 
                      nchains = 4, niter = 11000, nburnin = 1000, summary = TRUE)
results$summary$all.chains
