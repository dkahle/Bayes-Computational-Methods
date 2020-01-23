data {
  int<lower=0> N;
  int<lower=0> n[N];
  int<lower=0> y[N];
}

parameters {
  real<lower=0, upper=1> theta[N];                
}

model {
  theta ~ beta(2,1);
  y ~ binomial(n, theta);
}
