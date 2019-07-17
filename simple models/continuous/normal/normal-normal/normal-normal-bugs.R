library(R2OpenBUGS)
library(here)

model <- function() {
  for (i in 1:10) {
    y[i] ~ dnorm(mu, sigma_sq)
  }
  sigma_sq <- 1 / tau
  mu ~ dnorm(0, 1)
}

data <- data <- list(y = c(3,  2, 5,  1, -2, 3, 2, 8, -4, 6), tau = 0.5)
inits <- list((list(mu = 0)))
parameters <- c("mu")

model.file <- file.path(tempdir(), "model.txt")
write.model(model, model.file)

WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"
# WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
# WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
# OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~

b_model <- bugs(data, inits, parameters, model.file, n.chains = 1,
                n.iter = 10000, n.burnin = 1000, OpenBUGS.pgm=OpenBUGS.pgm, 
                WINE=WINE, WINEPATH=WINEPATH,useWINE=T)
b_model$summary


