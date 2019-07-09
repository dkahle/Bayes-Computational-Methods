library(nimble)

rCode <- nimbleCode({
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
})

data <- list(y = 5)
inits <- list(theta = 0.5)

results <- nimbleMCMC(rCode, data = data, inits = inits, monitors = c("theta"), 
                      nchains = 4, niter = 11000, nburnin = 1000, summary = TRUE)
results$summary$all.chains
