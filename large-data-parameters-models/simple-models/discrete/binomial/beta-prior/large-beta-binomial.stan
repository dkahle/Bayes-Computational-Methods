data {
  int<lower=0> n;
  int<lower=0> N;
  int<lower=0> y[N];
}

parameters {
  real<lower=0,upper=1> p;                
}

model {
  y ~ binomial(n,p);
  p ~ beta(1,1);
}

