data {
  int<lower=0> y;         
}

parameters {
  real <lower=0> theta;                
}

model {
  y ~ poisson(theta);
  theta ~ gamma(3,1);
}
