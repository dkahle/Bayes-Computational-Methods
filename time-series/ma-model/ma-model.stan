data {
  int<lower=2> T;  // number of observations
  vector[T] y;     // observation at time T
}

parameters {
  real mu;              // mean
  real<lower=0> sigma;  // error scale
  real theta;      // lag coefficients
}

transformed parameters {
  vector[T] epsilon;    // error terms
  epsilon[1] = y[1] - mu;
  for (t in 2:T)
    epsilon[t] = ( y[t] - mu
                    - theta * epsilon[t - 1] );
}

model {
  mu ~ normal(0,1000);
  theta ~ normal(0,1000);
  sigma ~ normal(0,1000) T[0,];
  for (t in 2:T)
    y[t] ~ normal(mu
                  + theta * epsilon[t - 1],
                  sigma);
}
