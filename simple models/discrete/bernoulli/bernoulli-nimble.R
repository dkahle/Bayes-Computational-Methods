library(nimble)

rCode <- nimbleCode({
  y ~ dbern(theta)
  theta ~ dbeta(1,1)
})

data <- list(y = 1)
inits <- list(theta = 0.5)

results <- nimbleMCMC(rCode, data = data, inits = inits, monitors = c("theta"), 
                      nchains = 4, niter = 11000, nburnin = 1000, summary = TRUE)
results$summary
