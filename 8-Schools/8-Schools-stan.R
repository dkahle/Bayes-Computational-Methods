library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
setwd("~/hubiC/Git Projects/Bayes-Computational-Methods/Bayes-Computational-Methods/8-Schools")

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = '8-Schools.stan', data = schools_dat, warmup = 1000,
                iter = 11000, chains = 4)
print(fit)


fit_old <- stan(file = '8-Schools-old.stan', data = schools_dat, warmup = 1000,
            iter = 11000, chains = 4)
print(fit_old)
