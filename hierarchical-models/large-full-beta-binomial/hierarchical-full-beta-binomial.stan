data {
  int<lower=0> N;
  int<lower=0> n[N];
  int<lower=0> y[N];
}

parameters {
  real<lower=0, upper=1> theta[N];                
  real<lower=0, upper=1> alpha;                
  real<lower=0, upper=1> beta;                
}

model {
  alpha ~ gamma(1,1);
  beta ~ gamma(1,1);
  theta ~ beta(alpha,beta);
  y ~ binomial(n, theta);
}
