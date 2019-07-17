library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_file <- here("beta-binomial", "beta-binomial.stan")

data <- list(y=2)

fit <- stan(file = 'beta-binomial.stan', data = data, 
            iter = 1000, chains = 4)
print(fit)
