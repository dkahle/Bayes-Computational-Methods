data {
  int<lower=0> N;
  real<lower=0> y[N];
}

parameters {
  real<lower=0> lambda;                
}

model {
  y ~ exponential(lambda);
  lambda ~ gamma(1,1);
}
