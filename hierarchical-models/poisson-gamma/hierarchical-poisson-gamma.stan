data {
  int<lower=0> N;
  int<lower=0> y[N];
  vector<lower=0>[N] t;
}

parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  vector<lower=0>[N] theta;
}

transformed parameters{
  vector<lower=0>[N] lambda;
  for (i in 1:N) {
    lambda[i] = t[i] * theta[i];
  }
}

model {
  alpha ~ exponential(1);
  beta ~ gamma(0.1, 1);
  theta ~ gamma(alpha, beta);
  y ~ poisson(lambda);
}
