data {
  int<lower=0> N;
  int<lower=0> M;
  // vector[N] y;
  row_vector[N] y;
}

parameters {
  real alpha;
  vector[M] beta;
  // real beta[M];
  real<lower=0> sigma;
}

model {
  for (i in (M+1):N)
    y[i] ~ normal(alpha + y[(i-M):(i-1)] * beta, sigma);
}
