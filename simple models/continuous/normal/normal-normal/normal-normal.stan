data {
  int<lower=0> J;
  real y[J];
  real <lower=0> tau;
}
parameters {
  real mu;                
}
model {
  y ~ normal(mu,(1 / tau^2));
  mu ~ normal(0,1);
}

