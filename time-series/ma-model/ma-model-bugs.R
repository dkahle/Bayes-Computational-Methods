## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("R2OpenBUGS")
library("bench")
library("here")



## generate/specify data
################################################################################

N <- 50 # sample size
theta <- c(0.6)
sigma <- 1

set.seed(1)
(y <- arima.sim(model = list(ma = theta), sd = sigma, n = N) %>% as.numeric())

t <- 1:length(y)
ggplot(data.frame(t = t, y = y), aes(t,y)) + geom_line()


bugs_data <- list(
  "N" = N,
  "y" = y
)



## specify bugs model
################################################################################

bugs_model <- function() {
  epsilon[1] <- y[1] - mu
  for (i in 2:N) {
    epsilon[i] <- y[i] - mu - theta * epsilon[i - 1]
    y_hat[i] <- mu + theta * epsilon [i - 1]
    y[i] ~ dnorm(y_hat[i], tau)
  }
  mu ~ dnorm(0,0.0001)
  theta ~ dnorm(0,0.0001)
  tau ~ dnorm(0,0.0001) %_% I(0, )
  sigma <- sqrt(1 / tau)
}


bugs.file <- file.path(tempdir(), "model.txt")
write.model(bugs_model, bugs.file)

bugs_monitor <- c("mu", "theta", "sigma")


## Specify path to WINE if using WINE 
###################################################################################

if (getwd() == "/Users/evanmiyakawa/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods") {
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


## fit model
################################################################################
source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  bugs_fit <- bugs(
    "model.file" = bugs.file, "data" = bugs_data, "parameters.to.save" = bugs_monitor, 
    "inits" = NULL, "n.chains" = n_chains, "n.iter" = n_iter, "n.burnin" = n_warmup,
    "OpenBUGS.pgm" = OpenBUGS.pgm, "WINE" = WINE, "WINEPATH" = WINEPATH,
    "useWINE" = T, debug = TRUE
  )
  
  
  
  ## assess fit
  ################################################################################
  
  bugs_fit$summary
  
  
  ## assess convergence issues 
  ###################################################################################
  
}



