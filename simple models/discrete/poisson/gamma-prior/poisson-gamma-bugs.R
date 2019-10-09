## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("R2OpenBUGS")
library("bench")



## generate/specify data
################################################################################

theta <- 5 # poisson theta

set.seed(1)

(y <- rpois(1, theta))

bugs_data <- list(
  "y" = y
)


## specify bugs model
################################################################################

bugs_model <- function() {
  y ~ dpois(theta)
  theta ~ dgamma(3,1)
}



bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)

monitor <- "theta"


## Specify path to WINE if using WINE 
###################################################################################

if (getwd() == "/Users/evanmiyakawa/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods") {
  WINE="/usr/local/Cellar/wine/4.0.1/bin/wine"
  WINEPATH="/usr/local/Cellar/wine/4.0.1/bin/winepath"
  OpenBUGS.pgm="/Users/evanmiyakawa/OpenBugs/OpenBUGS323/OpenBUGS.exe" #~
} else {
  WINE="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/wine"
  WINEPATH="/Users/evan_miyakawa1/Cellar/wine/4.0.1/bin/winepath"
  OpenBUGS.pgm="/Users/evan_miyakawa1/OpenBugs/OpenBUGS323/OpenBUGS.exe"
}



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

# bench_results <- mark(
#   bugs_fit <- bugs(
#     "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = monitor, 
#     "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
#     "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
#     "useWINE" = T
#   ),
#   iterations = 3
# )
# bench_results[1,2:9]



