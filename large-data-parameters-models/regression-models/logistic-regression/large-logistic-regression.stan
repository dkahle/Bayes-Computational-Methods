data {
  int<lower=0> n;
  int<lower=0> m;
  matrix[n,m] x;
  int<lower=0,upper=1> y[n];
}

parameters {
  real alpha;
  vector[m] beta;             
}

model {
  y ~ bernoulli_logit(alpha + x * beta);
  alpha ~ normal(0,1000);
  beta ~ normal(0,1000);
}
