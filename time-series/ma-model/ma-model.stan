data {
  int<lower=2> T;  // number of observations
  vector[T] y;     // observation at time T
}

parameters {
  real mu;              // mean
  real<lower=0> sigma;  // error scale
  vector[1] theta;      // lag coefficients
}

transformed parameters {
  vector[T] epsilon;    // error terms
  epsilon[1] = y[1] - mu;
  for (t in 2:T)
    epsilon[t] = ( y[t] - mu
                    - theta[1] * epsilon[t - 1] );
}

model {
  mu ~ cauchy(0, 2.5);
  theta ~ cauchy(0, 2.5);
  sigma ~ cauchy(0, 2.5);
  for (t in 2:T)
    y[t] ~ normal(mu
                  + theta[1] * epsilon[t - 1],
                  sigma);
}
