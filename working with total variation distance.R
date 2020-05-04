library(distr)
library(distrEx)

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
