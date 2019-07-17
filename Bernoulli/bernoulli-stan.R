library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_file <- here("Bernoulli", "bernoulli.stan")

data <- list(y=1)

fit <- stan(file = stan_file, data = data, 
            iter = 11000, warmup = 1000, chains = 4)
print(fit)
