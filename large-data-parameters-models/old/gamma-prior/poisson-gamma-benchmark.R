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
library("greta")
options("bayes_benchmark" = TRUE)

## Source code
################################################################################

source(here("simple-models", "discrete", "poisson", "gamma-prior", "poisson-gamma-jags.R"))
source(here("simple-models", "discrete", "poisson", "gamma-prior", "poisson-gamma-bugs.R"))
source(here("simple-models", "discrete", "poisson", "gamma-prior", "poisson-gamma-nimble.R"))
source(here("simple-models", "discrete", "poisson", "gamma-prior", "poisson-gamma-stan.R"))
source(here("simple-models", "discrete", "poisson", "gamma-prior", "poisson-gamma-greta.R"))

options("bayes_benchmark" = FALSE)

## Set Parameters
################################################################################
n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

## benchmarking
###################################################################################

num_iterations <- 2 # Number of times you want to benchmark each model\

rds_file_location <- here("simple-models", "discrete", "poisson", "gamma-prior", "poisson-gamma.rds")

source(here("benchmark-function.R"))

# choose to include stan compilation time with argument "stan_compile = TRUE"
run_benchmark(rds_file_location)
run_benchmark(rds_file_location, stan_compile = TRUE)
run_benchmark(rds_file_location, stan_compile = TRUE, stan_only = TRUE)

## benchmarking accuracy
###################################################################################

num_iterations <- 20 # Number of times you want to benchmark each model\

source(here("benchmark-accuracy-function.R"))

true_dist <- distr::Gammad(7,0.5)
true_mean <- 3.5

run_accuracy_benchmark(true_dist,num_iterations, n_iter, n_warmup)



