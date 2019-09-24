## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("R2OpenBUGS")
library("bench")



## generate/specify data
################################################################################

mu <- 1    # normal mu
tau <- 1/2 # normal tau
n <- 10    # sample size

set.seed(1)

(y <- rnorm(n, mu, sigma))

bugs_data <- list(
  "y" = y,
  "mu" = mu,
  "N" = n
)


## specify bugs model
################################################################################

bugs_model <- function() {
  for (i in 1:N) {
    y[i] ~ dnorm(mu, tau)
  }
  tau ~ dgamma(1,3)
}


bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)

monitor <- c("tau")


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



## benchmarking
###################################################################################

bench_results <- mark(
  bugs_fit <- bugs(
    "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = monitor, 
    "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
    "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
    "useWINE" = T
  ),
  iterations = 3
)
bench_results[1,2:9]


