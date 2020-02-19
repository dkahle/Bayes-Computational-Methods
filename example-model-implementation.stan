data {
  int<lower=0> n;
  int <lower=0,upper=1> y[n];
}

parameters{
  real<lower=0, upper=1> theta;
}

model {
  y ~ bernoulli(theta);
  theta ~ beta(1,1);
}
