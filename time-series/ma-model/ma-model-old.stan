data {
  int<lower=3> N;  // number of observations
  vector[N] y;     // observation at time T
}

parameters {
  real mu;              // mean
  real<lower=0> sigma;  // error scale
  vector[2] theta;      // lag coefficients
}

transformed parameters {
  vector[N] epsilon;    // error terms
  epsilon[1] = y[1] - mu;
  epsilon[2] = y[2] - mu - theta[1] * epsilon[1];
  for (n in 3:N)
    epsilon[n] = ( y[n] - mu
                    - theta[1] * epsilon[n - 1]
                    - theta[2] * epsilon[n - 2] );
}

model {
  mu ~ cauchy(0, 2.5);
  theta ~ cauchy(0, 2.5);
  sigma ~ cauchy(0, 2.5);
  for (n in 3:N)
    y[n] ~ normal(mu
                  + theta[1] * epsilon[n - 1]
                  + theta[2] * epsilon[n - 2],
                  sigma);
}
