## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("R2OpenBUGS")
library("bench")
library("here")
library("survival")



## generate/specify data
################################################################################

data(veteran) # load spatial data set

# t <- veteran$time # observed / censored event times
# x <- veteran$trt # single covariate
# not_censored <- veteran$status # indicator variable for those who weren't censored
# not_censored <- rep(1, length(not_censored))
# is_censored <- 1 - not_censored # indicator variable for those who were censored
# 
# # Here we create variables that will allow us to handle censored data in the bayes model
# t_censor <- t + not_censored
# t[not_censored == 0] <- NA
# 
# 
# jags_data <- list(
#   "t" = t,
#   "x" = x,
#   "t_censor" = t_censor,
#   "is_censored" = is_censored,
#   "N" = length(t)
# )

t <- veteran$time # observed / censored event times
x <- veteran$trt # single covariate
not_censored <- veteran$status # indicator variable for those who weren't censored
is_censored <- 1 - not_censored # indicator variable for those who were censored
N <- length(t)

# Here we create variables that will allow us to handle censored data in the bayes model
num_uncensored <- sum(not_censored)
start_censored <- num_uncensored + 1
t <- t[order(is_censored)]
x <- x[order(is_censored)]
t_censor <- t
t_censor[1:num_uncensored] <- 0
t[start_censored:N] <- NA
censored <- rep(0, length(t))
censored[start_censored:N] <- 1

bugs_data <- list(
  "t" = t,
  "t_censor" = t_censor,
  "x" = x,
  "num_uncensored" = num_uncensored,
  "start_censored" = num_uncensored + 1,
  "N" = length(t),
  "censored" = censored
)



## specify bugs model
################################################################################



bugs_model <- function() {
  for (i in 1:num_uncensored) {
    lambda[i] <- exp(beta * x[i])
    t[i] ~ dexp(lambda[i])
  }
  for (i in start_censored:N) {
    # censored[i] ~ dinterval(t[i], t_censor[i])
    lambda[i] <- exp(beta * x[i])
    t[i] ~ dexp(lambda[i]) %_% I(t_censor[i],)
  }
  beta ~ dnorm(0,0.0001)
}


bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)

bugs_monitor <- "beta"


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


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

t_inits <- t_censor + 1
t_inits[1:num_uncensored] <- NA
bugs_inits <- replicate(4,list("t" = t_inits), simplify = FALSE)


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  bugs_fit <- bugs(
    "model.file" = bugs.file, "data" = bugs_data,  "parameters.to.save" = bugs_monitor, 
    "inits" = bugs_inits, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
    "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
    "useWINE" = T, debug = F
  )
  
  
  
  ## assess fit
  ################################################################################
  
  bugs_fit$summary
  
  
  ## assess convergence issues 
  ###################################################################################
  
}








