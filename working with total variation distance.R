library(tidyverse); options(tibble.width = Inf)
library(magrittr)


library(distr)
library(distrEx)


true_dist <- Beta(3,9)
x <- rbeta(10000, 3,9)
true_dist <- Norm(0,1)
x <- rnorm(100000, 0, 1)

x_dist <- TotalVarDist(true_dist, x, asis.smooth.discretize = "asis")
x_dist
x_dist <- TotalVarDist(true_dist, x, asis.smooth.discretize = "discretize")
x_dist
x_dist <- TotalVarDist(x, true_dist, asis.smooth.discretize = "smooth")
x_dist



jags_dist <- TotalVarDist(true_dist,jags_fit_condense)
bugs_dist <- TotalVarDist(true_dist,bugs_fit_condense)
stan_dist <- TotalVarDist(true_dist,stan_fit_condense)
greta_dist <- TotalVarDist(true_dist,greta_fit_condense)

jags_dist <- TotalVarDist(true_dist,jags_fit_condense, 
                          asis.smooth.discretize = "asis")
jags_dist
bugs_dist <- TotalVarDist(true_dist,bugs_fit_condense, 
                          asis.smooth.discretize = "asis")
bugs_dist
stan_dist <- TotalVarDist(true_dist,stan_fit_condense)
greta_dist <- TotalVarDist(true_dist,greta_fit_condense)

tibble(
  jags_dist = jags_dist,
  bugs_dist = bugs_dist,
  stan_dist = stan_dist,
  greta_dist = greta_dist
)
