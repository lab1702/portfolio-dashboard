# Run from the project root:  Rscript tests/run_tests.R
suppressMessages({
  library(testthat)
  library(xts)
  library(PerformanceAnalytics)
})
source("portfolio_core.R")
test_dir("tests/testthat", stop_on_failure = TRUE)
