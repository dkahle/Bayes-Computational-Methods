library(tidyverse); options(tibble.width = Inf)
library(magrittr)


library(distr)
library(distrEx)

x <- rbeta(10000, 2,1)

true_dist <- Beta(1,2)

jags_fit_object <- jags_fit$mcmc %>% as.array()
# dim(jags_fit_object) <- c(dim(jags_fit_object), 1)
jags_fit_condense <- c(jags_fit_object[,1], 
                       jags_fit_object[,2],
                       jags_fit_object[,3],
                       jags_fit_object[,4])

TotalVarDist(true_dist,jags_fit_condense, 
             asis.smooth.discretize = "smooth")
TotalVarDist(true_dist,x, 
             asis.smooth.discretize = "smooth")


jags_fit_condense <- readRDS("jags_fit_condense.RDS")
stan_fit_condense <- readRDS("stan_fit_condense.RDS")
bugs_fit_condense <- readRDS("bugs_fit_condense.RDS")
greta_fit_condense <- readRDS("greta_fit_condense.RDS")
nimble_fit_condense <- readRDS("nimble_fit_condense.RDS")

x <- rgamma(10000, 7, 2)


jags_dist <- TotalVarDist(true_dist,jags_fit_condense, 
                          asis.smooth.discretize = "smooth")
bugs_dist <- TotalVarDist(true_dist,bugs_fit_condense, 
                          asis.smooth.discretize = "smooth")
stan_dist <- TotalVarDist(true_dist,stan_fit_condense, 
                          asis.smooth.discretize = "smooth")
greta_dist <- TotalVarDist(true_dist,greta_fit_condense, 
                           asis.smooth.discretize = "smooth")
nimble_dist <- TotalVarDist(true_dist,nimble_fit_condense, 
                           asis.smooth.discretize = "smooth")
x_dist <- TotalVarDist(true_dist,x, 
                           asis.smooth.discretize = "smooth")

jags_dist
bugs_dist
stan_dist
greta_dist
nimble_dist
x_dist

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
