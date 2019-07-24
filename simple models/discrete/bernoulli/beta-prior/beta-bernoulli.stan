data {
  int<lower=0> y;         
}

parameters {
  real p;                
}

model {
  y ~ bernoulli(p);
}

