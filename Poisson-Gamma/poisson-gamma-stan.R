library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
setwd("~/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods/Poisson-Gamma")

data <- list(y=5)

fit <- stan(file = 'poisson-gamma.stan', data = data, warmup = 1000,
            iter = 11000, chains = 4)
print(fit)
