# installing greta / python / anaconda / tensorflow
# library(reticulate)
# repl_python()
# use_python('/Users/evan_miyakawa1/opt/anaconda3/bin/python', require=T)
# system("python --version")

remove.packages("greta")
remove.packages("tensorflow")
devtools::install_github("greta-dev/greta")

install.packages("greta")

library("greta")
install_tensorflow(
  version = "1.14.0",
  extra_packages = "tensorflow-probability==0.7.0"
)

install.packages("igraph")
install.packages("DiagrammeR")
# 
# install_tensorflow(method = "conda")
# 
# install_tensorflow(
#   method = "conda",
#   version = "1.14.0",
#   extra_packages = "tensorflow-probability==0.7.0"
# )
# 
# install_tensorflow(
#   method = "conda",
#   version = "1.14.0"
# )
# 
# 
# reticulate::conda_install("r-tensorflow", "tensorflow-probability", pip = TRUE)
# 
# 
# remove.packages("greta")
# remove.packages("tensorflow")
# devtools::install_github("greta-dev/greta")

# data
x <- as_data(iris$Petal.Length)
y <- as_data(iris$Sepal.Length)

# variables and priors
int <- normal(0, 1)
coef <- normal(0, 3)
sd <- student(3, 0, 1, truncation = c(0, Inf))

# operations
mean <- int + coef * x

# likelihood
distribution(y) <- normal(mean, sd)

# defining the model - things we want to track
m <- model(int, coef, sd)

# plotting
plot(m)

# sampling
draws <- mcmc(m, n_samples = 1000)
