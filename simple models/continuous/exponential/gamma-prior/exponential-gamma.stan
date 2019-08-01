data {
  int<lower=0> N;
  real y[N];
}

parameters {
  real lambda;                
}

model {
  y ~ exponential(lambda);
  lambda ~ gamma(1,1);
}

