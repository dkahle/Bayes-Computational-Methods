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

source(here("simple-models", "continuous", "normal", "normal-prior", "normal-normal-jags.R"))
source(here("simple-models", "continuous", "normal", "normal-prior", "normal-normal-bugs.R"))
source(here("simple-models", "continuous", "normal", "normal-prior", "normal-normal-nimble.R"))
source(here("simple-models", "continuous", "normal", "normal-prior", "normal-normal-stan.R"))
source(here("simple-models", "continuous", "normal", "normal-prior", "normal-normal-greta.R"))

options("bayes_benchmark" = FALSE)

## Set Parameters
################################################################################
n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L

## benchmarking
###################################################################################

num_iterations <- 50 # Number of times you want to benchmark each model\

rds_file_location <- here("simple-models", "continuous", "normal", "normal-prior", "normal-normal.rds")

source(here("benchmark-function.R"))

# choose to include stan compilation time with argument "stan_compile = TRUE"
run_benchmark(rds_file_location)
run_benchmark(rds_file_location, stan_compile = TRUE)

## benchmarking accuracy
###################################################################################

num_iterations <- 20 # Number of times you want to benchmark each model\

source(here("benchmark-accuracy-function.R"))

true_dist <- distr::Norm(0.9031468, sqrt(0.2))

run_accuracy_benchmark(true_dist,num_iterations, n_iter / 2, n_warmup / 2)







