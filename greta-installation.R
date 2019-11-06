# installing greta / python / anaconda / tensorflow
# library(reticulate)
# repl_python()
# use_python('/Users/evan_miyakawa1/opt/anaconda3/bin/python', require=T)
# system("python --version")

remove.packages("greta")
remove.packages("tensorflow")
devtools::install_github("greta-dev/greta")

library("greta")
install_tensorflow(
  version = "1.14.0",
  extra_packages = "tensorflow-probability==0.7.0"
)
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
