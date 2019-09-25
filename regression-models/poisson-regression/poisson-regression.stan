data {
  int<lower=0> N;
  vector[N] x;
  int<lower=0> y[N];
}

parameters {
  real alpha;
  real beta;              
}

model {
  y ~ poisson_log(alpha + beta * x);
  alpha ~ normal(0,100);
  beta ~ normal(0,100);
}
