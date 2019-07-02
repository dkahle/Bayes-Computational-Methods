library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
setwd("~/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods/Betrnoulli")

data <- list(y=1)

fit <- stan(file = 'bernoulli.stan', data = data, 
            iter = 11000, warmup = 1000, chains = 4)
print(fit)
