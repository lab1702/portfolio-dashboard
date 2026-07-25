# Run from the project root:  Rscript tests/run_tests.R
suppressMessages({
  library(testthat)
  library(xts)
  library(PerformanceAnalytics)
})

# png is a test-only dependency, used to read a rendered chart back and check
# what actually landed on the canvas. It is named here rather than left to
# skip_if_not_installed alone: the test it guards is the only one covering a
# chart that renders completely blank, a failure that has happened once for
# real, and a silent skip on a machine without png would take that cover away
# without anything going red. There is no renv.lock or DESCRIPTION to pin it,
# so this is the pin.
if (!requireNamespace("png", quietly = TRUE)) {
  stop("The test suite needs the 'png' package: install.packages(\"png\").\n",
       "  It reads rendered charts back to verify they are not blank.",
       call. = FALSE)
}

source("portfolio_core.R")
test_dir("tests/testthat", stop_on_failure = TRUE)
