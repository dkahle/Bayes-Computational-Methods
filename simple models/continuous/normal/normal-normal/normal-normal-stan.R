library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_file <- here("simple models", "continuous", "normal", "normal-normal","normal-normal.stan")

data <- list(J = 10, y = c(3,  2, 5,  1, -2, 3, 2, 8, -4, 6), tau = 0.5)

fit <- stan(file = stan_file, data = data, warmup = 1000,
            iter = 11000, chains = 4)
print(fit)
