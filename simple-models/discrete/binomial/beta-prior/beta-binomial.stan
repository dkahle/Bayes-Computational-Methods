data {
  int<lower=0> y;
  int<lower=0> n;
}

parameters {
  real p;                
}

model {
  y ~ binomial(n,p);
  p ~ beta(1,1);
}

