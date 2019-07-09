library(R2OpenBUGS)
setwd("~/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods/8-Schools")

model <- function() {
  for (i in 1:8) {
    eta[i] ~ dnorm(0,1)
    theta[i] <- mu + tau * eta[i]
    y[i] ~ dnorm(theta[i], sigma[i])
  }
  mu ~ dunif(-10000,10000)
  tau ~ dunif(0,1000)
}c11

schools_dat <- list(y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
# inits <- list((list(theta = 1)))
parameters <- c("mu", "tau", "eta", "theta")

model.file <- file.path(tempdir(), "model.txt")
write.model(model, model.file)

WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"

# WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
# WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
# OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~

b_model <- bugs(data, inits=NULL, parameters, model.file, n.chains = 1,
                n.iter = 10000, n.burnin = 1000, OpenBUGS.pgm=OpenBUGS.pgm, 
                WINE=WINE, WINEPATH=WINEPATH,useWINE=T, debug = TRUE)
b_model$summary
