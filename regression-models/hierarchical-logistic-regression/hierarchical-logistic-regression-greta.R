## load required packages and set basic options
################################################################################

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("greta")
library("bayesplot")
library("bench")
library("here")


## generate/specify data
################################################################################
n <- 30L # sample size
d <- 2L
l <- 3L
ll <- sapply(1:l, function(x) rep(x,n / l)) %>% as.numeric()

set.seed(1)
(x <- matrix(rnorm(n * d), nrow = n, byrow = TRUE))
(y <- rbinom(n, 1, 0.5))

bugs_data <- list(
  "N" = n,
  "D" = d,
  "L" = l,
  "ll" = ll,
  "y" = y, 
  "x" = x
)

x <- as_data(x)
y <- as_data(y)



## specify greta model
################################################################################


#### NOT COMPLETE ############
mu <- normal(0, 1000, dim = c(1,d))
sigma <- normal(0,1000, dim = c(d,1), truncation = c(0,Inf))
mu_matrix <- zeros(3,2)
mu_matrix[1,] <- mu
mu_matrix[2,] <- mu
mu_matrix[3,] <- mu
sigma_matrix <- zeros(3,2)
sigma_matrix[1,] <- sigma
sigma_matrix[2,] <- sigma
sigma_matrix[3,] <- sigma
beta <- normal(mu_matrix, sigma_matrix)

theta <- zeros(n,1)
i <- 1
for (i in 1:n) {
  theta[i,] <- x[i,] %*% t(beta[ll[i],])
}

distribution(y) <- bernoulli(ilogit(theta))

greta_model <- model(mu, beta, sigma, theta)

plot(greta_model)

## configure model settings
################################################################################

n_chains <- 4
n_iter <- 1e4L
n_warmup <- 1e3L



## fit model
################################################################################

source(here("currently-benchmarking.R"))

if (!currently_benchmarking()) {
  
  
  greta_fit <- mcmc(
    "model" = greta_model, "n_samples" = n_iter,
    "warmup" = n_warmup, "chains" = n_chains
  )
  
  
  
  ## assess fit
  ################################################################################
  
  summary(greta_fit)
  
}

