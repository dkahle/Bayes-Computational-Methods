## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("bench")
library("here")
library("rjags"); library("runjags")
library("R2OpenBUGS")
library("nimble")
library("rstan"); rstan_options(auto_write = FALSE)
options("bayes_benchmark" = TRUE)

## Source code
################################################################################

source(here("simple models", "discrete", "binomial", "beta-prior", "beta-binomial-jags.R"))
source(here("simple models", "discrete", "binomial", "beta-prior", "beta-binomial-bugs.R"))
source(here("simple models", "discrete", "binomial", "beta-prior", "beta-binomial-nimble.R"))
source(here("simple models", "discrete", "binomial", "beta-prior", "beta-binomial-stan.R"))

options("bayes_benchmark" = FALSE)

## Set Parameters
################################################################################
n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

## benchmarking
###################################################################################

num_iterations <- 1 # Number of times you want to benchmark each model\

rds_file_location <- here("simple models", "discrete", "binomial", "beta-prior", "beta-binomial.rds")

source(here("benchmark-function.R"))

# choose to include stan compilation time with argument "stan_compile = TRUE"
run_benchmark(rds_file_location)
run_benchmark(rds_file_location, stan_compile = TRUE)




