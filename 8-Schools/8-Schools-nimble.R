library(nimble)

rCode <- nimbleCode({
  for (i in 1:8) {
    eta[i] ~ dnorm(0,1)
    theta[i] <- mu + tau * eta[i]
    y[i] ~ dnorm(theta[i], sigma[i])
  }
  mu ~ dunif(-10000,10000)
  tau ~ dunif(0,1000)
})

schools_dat <- list(y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
inits <- list(theta = 0, mu = 0, tau = 5, eta = 5)

results <- nimbleMCMC(rCode, data = schools_dat, inits = inits, monitors = c("mu", "tau", "eta", "theta"), 
                      nchains = 4, niter = 500000, nburnin = 5000, thin = 1, summary = TRUE)
results
