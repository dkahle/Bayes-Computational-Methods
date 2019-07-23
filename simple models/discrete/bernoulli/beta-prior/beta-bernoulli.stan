data {
  int<lower=0> y;         
}

parameters {
  real theta;                
}

model {
  y ~ bernoulli(theta);
}

