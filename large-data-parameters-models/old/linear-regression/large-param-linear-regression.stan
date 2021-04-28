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
  for (i in 1:N) {
    y[i] ~ normal(alpha + x[i,] * beta, sigma);
  }
  // y ~ normal(alpha + beta %*% x, sigma);
}
