library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_file <- here("poisson-gamma", "poisson-gamma.stan")

data <- list(y=5)

fit <- stan(file = stan_file, data = data, warmup = 1000,
            iter = 11000, chains = 4)
print(fit)
