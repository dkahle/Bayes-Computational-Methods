data {
  int<lower=0> N;
  real y[N];
  real <lower=0> sigma;
}

parameters {
  real mu;                
}

model {
  y ~ normal(mu,sigma);
  mu ~ normal(0,1);
}

