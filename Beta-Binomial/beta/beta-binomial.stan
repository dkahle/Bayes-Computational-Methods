data {
  int<lower=0> y;         
}
parameters {
  real theta;                
}
model {
  y ~ binomial(10,theta);
  theta ~ beta(1,1);
}

