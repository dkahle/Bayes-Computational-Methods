data {
  int<lower=0> y;         
}

parameters {
  real<lower=0, upper=1> p;                
}

model {
  y ~ bernoulli(p);
}

