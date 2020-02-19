data {
  int<lower=0> N;
  real y[N];
  real <lower=0> mu;
}

parameters {
  real tau;                
}

model {
  y ~ normal(mu,(1 / tau^2));
  tau ~ gamma(1,3);
}

