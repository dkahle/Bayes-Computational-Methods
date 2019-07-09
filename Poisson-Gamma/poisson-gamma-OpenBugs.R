library(R2OpenBUGS)
setwd("~/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods/Poisson-Gamma")

model <- function() {
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
}

data <- list(y = 5)
inits <- list((list(theta = 0.5)))
parameters <- c("theta")

model.file <- file.path(tempdir(), "model.txt")
write.model(model, model.file)

# WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
# WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
# OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"
WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~

b_model <- bugs(data, inits, parameters, model.file, n.chains = 1,
                n.iter = 10000, n.burnin = 1000, OpenBUGS.pgm=OpenBUGS.pgm, 
                WINE=WINE, WINEPATH=WINEPATH,useWINE=T)
b_model$summary
