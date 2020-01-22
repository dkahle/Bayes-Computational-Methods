currently_benchmarking <- function() {
  if (is.null(getOption("bayes_benchmark")) || !(getOption("bayes_benchmark"))) {
    FALSE
  } else {
    TRUE
  }
}
