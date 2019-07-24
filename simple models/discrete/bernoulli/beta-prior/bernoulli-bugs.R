## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("R2OpenBUGS")



## generate/specify data
################################################################################

p <- .25 # bernoulli p

set.seed(1)

(y <- rbinom(1, 1, p))

bugs_data <- list(
  "y" = y
)


## specify bugs model
################################################################################

bugs_model <- function() {
  y ~ dbin(p,1)
  p ~ dbeta(1,1)
}


bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)

monitor <- "p"


## Specify path to WINE if using WINE 
###################################################################################

# WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
# WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
# OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"
WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~




## fit model
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


bugs_fit <- bugs(
  "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = monitor, 
  "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
  "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
  "useWINE" = T
)



## assess fit
################################################################################

bugs_fit$summary


## assess convergence issues 
###################################################################################


















library(R2OpenBUGS)
library(here)

model <- function() {
  y ~ dbin(theta,1)
  theta ~ dbeta(1,1)
}

data <- list(y = 1)
inits <- list((list(theta = 0.5)))
parameters <- c("theta")

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
