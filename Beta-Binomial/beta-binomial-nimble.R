library(nimble)

rCode <- nimbleCode({
  y ~ dbin(theta,10)
  theta ~ dbeta(1,1)
})

data <- list(y = 2)
inits <- list(theta = 0.5)

results <- nimbleMCMC(rCode, data = data, inits = inits, monitors = c("theta"), 
                      nchains = 4, niter = 11000, nburnin = 1000, summary = TRUE)
results$summary$all.chains
