data {
  int<lower=0> N;         
  real<lower=0> t[N];         
  int x[N];         
}

parameters {
  real beta;
}

model {
  for (i in 1:N) {
    t[i] ~ exponential(exp(beta * x[i]));
  }
  beta ~ normal(0,1);
}
