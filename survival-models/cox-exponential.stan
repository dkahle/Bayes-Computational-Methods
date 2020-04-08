data {
  int<lower=0> N;         
  int<lower=0> num_uncensored;         
  int<lower=0> num_censored;         
  real<lower=0> t[num_uncensored];         
  real<lower=0> t_censor[num_censored];         
  int x[N];         
}

parameters {
  real beta;
  // real t_c[num_censored];
}

model {
  for (i in 1:num_uncensored) {
    t[i] ~ exponential(exp(beta * x[i]));
  }
  for (i in 1:num_censored) {
    // t_c[i] ~ exponential(exp(beta * x[num_uncensored + i])) T[t_censor[i],];
    target += exponential_lccdf(t_censor[i] | exp(beta * x[num_uncensored + i]));
    // t_c[i] ~ exponential_cdf(exp(beta * x[num_uncensored + i])) T[t_censor[i],];
  }
  beta ~ normal(0,1000);
}
