data {
  int<lower=0> num_uncensored;         
  int<lower=0> num_censored;         
  real<lower=0> t[num_uncensored];         
  real<lower=0> t_censor[num_censored];         
  int x[num_uncensored];         
  int x_c[num_censored];         
}

parameters {
  real beta;
  real<lower=0> t_u[num_censored];
}

model {
  for (i in 1:num_uncensored) {
    t[i] ~ exponential(beta * x[i]);
  }
  for (i in 1:num_censored) {
    t_u[i] ~ exponential(beta * x_c[i]) T[t_censor[i],];
  }
  beta ~ normal(0,1000);
}
