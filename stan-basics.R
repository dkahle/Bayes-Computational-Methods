# in this file we show how to run a simple linear regression
# model with stan

library("tidyverse"); theme_set(theme_bw())
library("rstan"); options(mc.cores = parallel::detectCores())

## generate fake regression data
############################################################

set.seed(1)
n <- 11
tb <- data_frame(
  x = seq(0, 1, length.out = n),
  y = 3 - 2*x + rnorm(n, 0, sd = .5)
)  

ggplot(tb, aes(x, y)) + geom_point()


## write code for the simple linear regression model
############################################################

stan_code <- "
data {
  int<lower=0> n;
  vector[n] x;
  vector[n] y;
}

parameters{
  real beta0;	
  real beta1;
  real<lower=0> sigma;
} 

model{
  for(k in 1:n) {
    y[k] ~ normal(beta0 + beta1*x[k], sigma);
  }
  beta0 ~ normal(0, 100); // removing this...
  beta1 ~ normal(0, 100); // ... and this uses a flat prior
  sigma ~ cauchy(0, 2);   // positivity enforced by parameter
}
"

stan_data <- c(tb, "n" = n)
stan_data %>% glimpse()

## compile code and sample
############################################################

n_chains <- 4
n_iter <- 1e4
n_warmup <- 500

fit <- stan(
  model_code = stan_code, data = stan_data, 
  chains = n_chains, iter = n_iter, warmup = n_warmup
)



## basic summaries
############################################################

methods(class = "stanfit")

quartz()

show(fit)
#        mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# beta0  2.87    0.00 0.28  2.31  2.70  2.87  3.05  3.44 12091    1
# beta1 -1.49    0.00 0.48 -2.45 -1.78 -1.48 -1.19 -0.55 11777    1
# sigma  0.48    0.00 0.13  0.29  0.38  0.45  0.54  0.80 13329    1
# lp__   2.66    0.01 1.42 -0.99  2.01  3.03  3.70  4.29 10145    1

summary(fit)
get_posterior_mean(fit)

bayesplot::rhat(fit)
bayesplot::neff_ratio(fit)

plot(fit)
pairs(fit)
# fit %>% as.array() %>% mcmc_pairs()
traceplot(fit, inc_warmup = TRUE)

get_num_upars(fit)
get_stanmodel(fit)


## plots with rstan
############################################################

stan_dens(fit)
stan_dens(fit) + theme_bw()
stan_diag(fit)
stan_ess(fit)
stan_hist(fit)
stan_mcse(fit)
stan_plot(fit)
stan_trace(fit, inc_warmup = TRUE)




## plots with bayesplot
############################################################
# note: these mix the chains

library("bayesplot"); theme_set(theme_bw())
ls("package:bayesplot")
apropos("mcmc_")

# color_scheme_set(scales::brewer_pal("qual")(6))
# color_scheme_view()

fit %>% as.array() %>% mcmc_acf()
fit %>% as.array() %>% mcmc_acf_bar()

fit %>% as.array() %>% mcmc_areas()
fit %>% as.array() %>% mcmc_areas_ridges()

fit %>% as.array() %>% mcmc_combo()

fit %>% as.array() %>% mcmc_dens()
fit %>% as.array() %>% mcmc_dens_overlay()
fit %>% as.array() %>% mcmc_dens_chains()

fit %>% as.array() %>% mcmc_hex(pars = c("beta0", "beta1"))
fit %>% as.array() %>% mcmc_scatter(pars = c("beta0", "beta1"))

fit %>% as.array() %>% mcmc_hist()
fit %>% as.array() %>% mcmc_hist_by_chain()

fit %>% as.array() %>% mcmc_intervals()

fit %>% neff_ratio()
fit %>% neff_ratio() %>% mcmc_neff()
fit %>% neff_ratio() %>% mcmc_neff_hist()

fit %>% as.array() %>% mcmc_pairs()

fit %>% as.array() %>% mcmc_parcoord()

fit %>% as.array() %>% `[`(,,1:3) %>% 
  mcmc_recover_hist(true = c(beta0 = 3, beta1 = -2, sigma = .5))

fit %>% as.array() %>% `[`(,,1:3) %>% 
  mcmc_recover_intervals(true = c(beta0 = 3, beta1 = -2, sigma = .5))

fit %>% as.array() %>% `[`(,,1:3) %>% 
  mcmc_recover_scatter(true = c(beta0 = 3, beta1 = -2, sigma = .5))

fit %>% rhat() %>% mcmc_rhat()
fit %>% rhat() %>% mcmc_rhat_hist()

fit %>% as.array() %>% mcmc_trace()
fit %>% as.array() %>% mcmc_trace_highlight()

fit %>% as.array() %>% mcmc_violin()

# ggplot2 styling works
fit %>% as.array() %>% 
  mcmc_areas(pars = c("beta0", "beta1", "sigma")) +
  scale_y_discrete(labels = expression(beta[0], beta[1], sigma))

# note: for convergence, use traceplot(fit, inc_warmup = TRUE)
# or, manually:
fit %>% 
  extract(
    pars = c("beta0", "beta1", "sigma"),
    permuted = FALSE, inc_warmup = TRUE
  ) %>% 
  mcmc_trace()

# for posterior lines:
# posterior lines
fit %>% 
  extract(pars = c("beta0", "beta1")) %>% 
  as_data_frame() %>% 
  sample_n(100) %>% {
    ggplot(tb, aes(x, y)) +
      geom_abline(
        aes(intercept = beta0, slope = beta1),
        color = "purple", alpha = .2, data = .
      ) +
      geom_point()
  }

## extracting samples
############################################################

getMethod(extract, "stanfit")

fit %>% extract() %>% str()
fit %>% extract(pars = c("beta0", "beta1")) %>% str()
fit %>% extract(permuted = FALSE) %>% str()
fit %>% extract(permuted = FALSE, inc_warmup = TRUE) %>% str()
# note that permuted = FALSE is required for teh above

rstan:::as.array.stanfit

fit %>% as.matrix() %>% str()
fit %>% as.array() %>% str()
fit %>% as.data.frame() %>% str()

# if you want all obs in data frame format:
fit %>% 
  extract(permuted = FALSE, inc_warmup = TRUE) %>% 
  array_branch(2L) %>% 
  map(~ as_data_frame(.x) %>% set_names(names(fit))) %>% 
  bind_rows() %>% 
  mutate(
    iter = rep(1:n_iter, n_chains),
    chain = factor(rep(1:n_chains, each = n_iter))
  )



## vectorized model statement
############################################################

stan_code <- "
data{
  int<lower=0> n;
  vector[n] x;
  vector[n] y;
}

parameters{
  real beta0;
  real beta1;
  real<lower=0> sigma;
} 

model{
  y ~ normal(beta0 + beta1*x, sigma);
  beta0 ~ normal(0, 100);
  beta1 ~ normal(0, 100);  
  sigma ~ cauchy(0, 2);
}
"

fit <- stan(
  model_code = stan_code, data = stan_data, 
  chains = n_chains, iter = n_iter, warmup = n_warmup
)

show(fit)
#        mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# beta0  2.87    0.00 0.28  2.30  2.70  2.87  3.04  3.43 12716    1
# beta1 -1.48    0.00 0.47 -2.44 -1.77 -1.48 -1.19 -0.54 12850    1
# sigma  0.48    0.00 0.14  0.29  0.38  0.45  0.54  0.81 13028    1
# lp__   2.67    0.01 1.44 -1.07  2.03  3.05  3.72  4.30  9360    1

fit %>% extract(permuted = FALSE, inc_warmup = TRUE) %>% str()







## vectorized model statement with transformed parameters
############################################################
# this the same as above but does store the intermediate 
# conditional mean

stan_code <- "
data {
  int<lower=0> n;
  vector[n] x;
  vector[n] y;
}

parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
} 

transformed parameters {
  vector[n] mu;
  mu = beta0 + beta1*x;
}

model {
  y ~ normal(mu, sigma);
  beta0 ~ normal(0, 100);
  beta1 ~ normal(0, 100);  
  sigma ~ cauchy(0, 2);
}
"

fit <- stan(
  model_code = stan_code, data = stan_data, 
  chains = n_chains, iter = n_iter, warmup = n_warmup
)

show(fit)

fit %>% extract(permuted = FALSE, inc_warmup = TRUE) %>% str()

#         mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# beta0   2.87    0.00 0.28  2.32  2.70  2.87  3.04  3.42 14507    1
# beta1  -1.48    0.00 0.47 -2.42 -1.77 -1.48 -1.19 -0.53 14355    1
# sigma   0.48    0.00 0.14  0.29  0.38  0.45  0.54  0.81 12687    1
# mu[1]   2.87    0.00 0.28  2.32  2.70  2.87  3.04  3.42 14507    1
# mu[2]   2.72    0.00 0.24  2.24  2.57  2.72  2.87  3.20 15265    1
# mu[3]   2.57    0.00 0.20  2.16  2.45  2.57  2.70  2.98 16762    1
# mu[4]   2.42    0.00 0.17  2.07  2.32  2.43  2.53  2.78 19887    1
# mu[5]   2.28    0.00 0.16  1.97  2.18  2.28  2.37  2.59 26478    1
# mu[6]   2.13    0.00 0.15  1.83  2.04  2.13  2.22  2.42 38000    1
# mu[7]   1.98    0.00 0.16  1.67  1.88  1.98  2.08  2.29 38000    1
# mu[8]   1.83    0.00 0.18  1.48  1.72  1.83  1.94  2.19 38000    1
# mu[9]   1.68    0.00 0.21  1.27  1.56  1.68  1.81  2.10 28850    1
# mu[10]  1.54    0.00 0.24  1.05  1.39  1.54  1.69  2.02 24452    1
# mu[11]  1.39    0.00 0.28  0.83  1.22  1.39  1.56  1.94 21768    1
# lp__    2.68    0.01 1.42 -1.02  2.04  3.05  3.72  4.30  9951    1







## same model using model.frame
############################################################

y <- tb$y
(X <- model.matrix(y ~ ., data = tb))
l <- ncol(X) # l = # linear terms

stan_code <- "
data {
  int<lower=0> n;
  int<lower=0> l;
  matrix[n,l] X;
  vector[n] y;
}

parameters {
  vector[l] beta;
  real<lower=0> sigma;
} 

transformed parameters {
  vector[n] mu;
  mu = X*beta;
}

model {
  y ~ normal(mu, sigma);
  beta ~ normal(0, 100);
  sigma ~ cauchy(0, 2);
}
"

fit <- stan(
  model_code = stan_code, 
  data = list(n = n, X = X, y = y, l = l), 
  chains = n_chains, iter = n_iter, warmup = n_warmup
)

show(fit)
#          mean se_mean   sd  2.5%   25%   50%   75% 97.5% n_eff Rhat
# beta[1]  2.87    0.00 0.29  2.31  2.70  2.87  3.04  3.44  9499    1
# beta[2] -1.48    0.00 0.49 -2.44 -1.77 -1.48 -1.19 -0.54  9766    1
# sigma    0.48    0.00 0.14  0.29  0.38  0.45  0.54  0.82  9130    1
# mu[1]    2.87    0.00 0.29  2.31  2.70  2.87  3.04  3.44  9499    1
# mu[2]    2.72    0.00 0.25  2.24  2.57  2.72  2.87  3.21  9951    1
# mu[3]    2.57    0.00 0.21  2.16  2.45  2.57  2.70  2.99 10896    1
# mu[4]    2.43    0.00 0.18  2.07  2.32  2.42  2.54  2.78 13026    1
# mu[5]    2.28    0.00 0.16  1.97  2.18  2.28  2.37  2.59 18131    1
# mu[6]    2.13    0.00 0.15  1.83  2.04  2.13  2.22  2.43 38000    1
# mu[7]    1.98    0.00 0.16  1.67  1.89  1.98  2.08  2.29 38000    1
# mu[8]    1.83    0.00 0.18  1.48  1.73  1.83  1.94  2.19 38000    1
# mu[9]    1.68    0.00 0.21  1.27  1.56  1.69  1.81  2.09 25075    1
# mu[10]   1.54    0.00 0.24  1.05  1.39  1.54  1.68  2.02 18814    1
# mu[11]   1.39    0.00 0.28  0.82  1.22  1.39  1.56  1.94 16173    1
# lp__     2.65    0.02 1.49 -1.16  2.00  3.03  3.71  4.30  8175    1



