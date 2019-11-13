currently_benchmarking <- function() {
  if ((is.null(options()[["bayes_benchmark"]]) || !(options()[["bayes_benchmark"]]))) {
    FALSE
  } else {
    TRUE
  }
}