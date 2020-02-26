data {
  int<lower=2> N;  // number of observations
  vector[N] y;     // observation at time T
}

parameters {
  real mu;              // mean
  real<lower=0> sigma;  // error scale
  real theta;      // lag coefficients
}

transformed parameters {
  vector[N] epsilon;    // error terms
  // epsilon[1] = y[1] - mu;
  epsilon[1] = y[1] - mu;
  for (n in 2:N) {
    // epsilon[n] = (y[n] - mu - theta * epsilon[n - 1]);
    epsilon[n] = mu + 1;
  }
}

model {
  mu ~ normal(0,1000);
  theta ~ normal(0,1000);
  sigma ~ normal(0,1000);
  for (n in 2:N) {
    y[n] ~ normal(mu + theta * epsilon[n-1], sigma);
  }
}
