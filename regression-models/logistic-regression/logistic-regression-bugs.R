## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("R2OpenBUGS")
library("bench")



## generate/specify data
################################################################################

n <- 10L # sample size
alpha <- -5 # intercept
beta <- 1 # single coefficient

set.seed(1)

(x <- rnorm(n, 5, 1)) # observed x values
theta_0 <- rnorm(n,alpha,0.5) + rnorm(n,beta,0.5) * x 
(theta <- exp(theta_0) / (1 + exp(theta_0))) # generated values of bernoulli theta
(y <- rbinom(n,1,theta))


bugs_data <- list(
  "n" = n,
  "y" = y, 
  "x" = x, 
  "tau" = (1 / 1000)^2
)




## specify bugs model
################################################################################

bugs_model <- function() {
  for (i in 1:n) {
    logit(theta[i]) <- alpha + beta * x[i]
    y[i] ~ dbern(theta[i])
  }
  alpha ~ dnorm(0,tau)
  beta ~ dnorm(0,tau)
}


bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)

monitor <- c("alpha", "beta")


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

n_chains <- 2L
n_iter <- 1e3L
n_warmup <- 1e2L


bugs_fit <- bugs(
  "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = monitor, 
  "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
  "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
  "useWINE" = T, debug = TRUE
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



