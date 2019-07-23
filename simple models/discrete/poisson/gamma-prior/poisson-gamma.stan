data {
  int<lower=0> y;         
}

parameters {
  real theta;                
}

model {
  y ~ poisson(theta);
  theta ~ gamma(3,1);
}

