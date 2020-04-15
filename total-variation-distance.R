library(distr)
library(distrEx)


TotalVarDist(Norm(), UnivarMixingDistribution(Norm(1,2),Norm(0.5,3),
                                              mixCoeff=c(0.2,0.8)))
TotalVarDist(Norm(), Td(10))
TotalVarDist(Norm(mean = 50, sd = sqrt(25)), Binom(size = 100)) # mutually singular
TotalVarDist(Pois(10), Binom(size = 20))
x <- rnorm(100)
TotalVarDist(Norm(), x)
TotalVarDist(x, Norm(), asis.smooth.discretize = "smooth")
y <- (rbinom(50, size = 20, prob = 0.5)-10)/sqrt(5)
TotalVarDist(y, Norm())
TotalVarDist(y, Norm(), asis.smooth.discretize = "smooth")
TotalVarDist(rbinom(50, size = 20, prob = 0.5), Binom(size = 20, prob = 0.5))

x <- rnorm(100)
y <- rnorm(100)

TotalVarDist(x,y)



# Check for beta-binomial -------------------------------------------------

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")
library("bayesplot")
library("bench")
library("here")



## generate/specify data
################################################################################

p <- .25 # binomial p
n <-  10 # binomial n

set.seed(1)

(y <- rbinom(1, n, p))

jags_data <- list(
  "y" = y,
  "n" = n
)



## specify jags model
################################################################################

jags_model <- "
  model{
    y ~ dbin(p,n)
    p ~ dbeta(1,1)
  }
"

jags_monitor <- c("p")


## configure model settings
################################################################################

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

source(here("simple-models", "discrete", "binomial", "beta-prior", "beta-binomial-jags.R"))
source(here("simple-models", "discrete", "binomial", "beta-prior", "beta-binomial-nimble.R"))
source(here("simple-models", "discrete", "binomial", "beta-prior", "beta-binomial-stan.R"))


jags_fit_object <- jags_fit$mcmc %>% as.array()
dim(jags_fit_object) <- c(dim(jags_fit_object), 1)
jags_fit_condense <- c(jags_fit_object[,1,1], 
                       jags_fit_object[,2,1],
                       jags_fit_object[,3,1],
                       jags_fit_object[,4,1])

nimble_fit_object <- nimble_fit$samples %>% as.array()
nimble_fit_condense <- c(
  nimble_fit_object$chain1[,1],
  nimble_fit_object$chain2[,1],
  nimble_fit_object$chain3[,1],
  nimble_fit_object$chain4[,1]
)

stan_fit_object <- stan_fit %>% as.array()
stan_fit_condense <- c(
  stan_fit_object[,1,1],
  stan_fit_object[,2,1],
  stan_fit_object[,3,1],
  stan_fit_object[,4,1]
)

TotalVarDist(Beta(3,9),jags_fit_condense) #0.0466
TotalVarDist(Beta(3,9),nimble_fit_condense) #0.0399
TotalVarDist(Beta(3,9),stan_fit_condense) #0.0420


## stan and nimble affected by set.seed, jags not










