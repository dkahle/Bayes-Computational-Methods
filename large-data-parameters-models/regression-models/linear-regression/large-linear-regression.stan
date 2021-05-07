data {
  int<lower=0> N;
  int<lower=0> M;
  matrix[N,M] x;
  vector[N] y;
}

parameters {
  real alpha;
  vector[M] beta;
  real<lower=0> sigma;
}

model {
  alpha ~ normal(0, 100);
  beta ~ normal(0, 100);
  y ~ normal(alpha + x * beta, sigma);
}
