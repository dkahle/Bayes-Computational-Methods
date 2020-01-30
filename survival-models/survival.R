library(tidyverse)
library(survival)
library(survminer)
data("lung")
head(lung)
fit <- survfit(Surv(time, status) ~ sex, data = lung)
print(fit)

library(ranger)
library(ggfortify)

#------------
data(veteran)
head(veteran)

km <- with(veteran, Surv(time, status))
km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_fit)

vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))

km_AG_fit <- survfit(Surv(time, status) ~ AG, data=vet)
autoplot(km_AG_fit)

cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = vet)
summary(cox)

?Surv
Surv(vet$time, vet$status)

vet <- filter(vet, status == 1)

cox <- coxph(Surv(time, status) ~ trt, data = vet)
summary(cox)


####

library("tidyverse"); theme_set(theme_minimal())
library("parallel"); options(mc.cores = detectCores())
library("rjags"); library("runjags")
library("bayesplot")
library("bench")
library("here")

y <- vet$time
trt <- vet$trt
trt <- trt %>% as.numeric() - 1

jags_data <- list(
  "t" = y,
  "x" = trt,
  "N" = length(y)#,
#  "alpha" = 1
)

jags_model <- "
  model{
    for (i in 1:N) {
      lambda[i] <- exp(beta_0 + beta * x[i])
      t[i] ~ dexp(lambda[i])
    }
    beta ~ dunif(-1000,1000)
    beta_0 ~ dunif(-1000,1000)
  }
"

jags_model <- "
  model{
    for (i in 1:N) {
      lambda[i] <- exp(beta * x[i])
      t[i] ~ dexp(lambda[i])
    }
    beta ~ dnorm(0,0.0001)
  }
"

jags_model <- "
  model{
    for (i in 1:N) {
      h[i] <- exp(beta * x[i])
      lambda[i] <- h[i]
      t[i] ~ dweib(lambda[i], alpha)
    }
    beta ~ dnorm(0,0.0001)
  }
"

# 
# jags_model <- "
#   model{
#     for (i in 1:N) {
#       y_hat[i] <- exp(beta * x[i])
#       y[i] ~ dnorm(log(y_hat[i]), tau)
#     }
#     beta ~ dnorm(0,0.0001)
#     tau ~ dnorm(0,0.0001)  I(0, )
#   }
# "

jags_monitor <- c("beta")

n_chains <- 4L
n_iter <- 1e4L
n_warmup <- 1e3L


jags_fit <- run.jags(
  "model" = jags_model, "data" = jags_data, "monitor" = jags_monitor, 
  "n.chains" = n_chains, "sample" = n_iter, "burnin" = n_warmup
) 

jags_fit


# http://www.mas.ncl.ac.uk/~nmf16/teaching/mas8391/survival.pdf
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html#what_is_censoring

# http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Survival/BS704_Survival6.html

# http://www.people.vcu.edu/~dbandyop/pubh7440/SurvivalBUGS2015.pdf





