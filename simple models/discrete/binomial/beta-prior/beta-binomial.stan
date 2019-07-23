data {
  int<lower=0> y;
  int<lower=0> n;
}

parameters {
  real theta;                
}

model {
  y ~ binomial(n,theta);
  theta ~ beta(1,1);
}

