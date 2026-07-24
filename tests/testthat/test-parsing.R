test_that("parse_symbols cleans, uppercases, and drops empties", {
  expect_equal(parse_symbols("voo, tqqq , SCHD"), c("VOO", "TQQQ", "SCHD"))
  expect_equal(parse_symbols("VOO,,SCHD,"), c("VOO", "SCHD"))
  expect_equal(parse_symbols(""), character(0))
  expect_equal(parse_symbols(" , , "), character(0))
})

test_that("parse_weights handles blanks, trailing commas, and bad tokens", {
  expect_equal(parse_weights("40, 30, 30"), c(40, 30, 30))
  expect_equal(parse_weights("1,2,"), c(1, 2))
  expect_true(is.na(parse_weights("abc")))
  expect_equal(parse_weights(".5, 40."), c(0.5, 40))
})

test_that("valid inputs produce no errors", {
  expect_length(portfolio_input_errors(c("VOO", "QQQ"), c(1, 2), "SPY", 5), 0)
  expect_length(portfolio_input_errors("VOO", 100, "^GSPC", 1), 0)
})

test_that("each validation rule fires with its exact message", {
  e <- portfolio_input_errors
  expect_true("Enter at least one symbol." %in%
              e(character(0), numeric(0), "SPY", 5))
  expect_true("Remove duplicate symbols." %in%
              e(c("VOO", "VOO"), c(1, 1), "SPY", 5))
  expect_true("Number of symbols and weights must match." %in%
              e("VOO", c(1, 2), "SPY", 5))
  expect_true("Weights must be numeric." %in%
              e("VOO", NA_real_, "SPY", 5))
  expect_true("Weights cannot be negative." %in%
              e(c("A", "B"), c(-1, 2), "SPY", 5))
  expect_true("At least one weight must be positive." %in%
              e(c("A", "B"), c(0, 0), "SPY", 5))
  expect_true("Enter a benchmark symbol." %in%
              e("VOO", 1, "", 5))
  expect_true("Enter a single benchmark symbol." %in%
              e("VOO", 1, "SPY, QQQ", 5))
  expect_true("Years of history must be at least 1." %in%
              e("VOO", 1, "SPY", NA))
  expect_true("Years of history must be at least 1." %in%
              e("VOO", 1, "SPY", 0))
})

test_that("years of history is bounded at both ends", {
  e <- portfolio_input_errors
  expect_length(e("VOO", 1, "SPY", MAX_YEARS), 0)
  expect_true(sprintf("Years of history cannot exceed %d.", MAX_YEARS) %in%
              e("VOO", 1, "SPY", MAX_YEARS + 1))
  # a missing value reports the lower bound only, not both bounds at once
  expect_equal(sum(grepl("^Years of history", e("VOO", 1, "SPY", NA))), 1)
})

test_that("non-finite weights are rejected (Inf regression)", {
  e <- portfolio_input_errors
  expect_true("Weights must be numeric." %in% e(c("A", "B"), c(Inf, 1), "SPY", 5))
  expect_true("Weights must be numeric." %in% e(c("A", "B"), c(NaN, 1), "SPY", 5))
  # "1e999" parses to Inf
  expect_true("Weights must be numeric." %in%
              e(c("A", "B"), parse_weights("1e999, 1"), "SPY", 5))
})

test_that("the symbol list is bounded at both ends", {
  e <- portfolio_input_errors
  ok <- sprintf("S%03d", seq_len(MAX_SYMBOLS))
  expect_length(e(ok, rep(1, MAX_SYMBOLS), "SPY", 5), 0)

  # the symbol list is the only input that reaches the network: one sequential
  # download each, so an unbounded list froze the dashboard rather than failing
  too_many <- sprintf("S%03d", seq_len(MAX_SYMBOLS + 1))
  expect_true(sprintf("Enter at most %d symbols.", MAX_SYMBOLS) %in%
              e(too_many, rep(1, MAX_SYMBOLS + 1), "SPY", 5))
  expect_length(e(sprintf("S%03d", 1:500), rep(1, 500), "SPY", 5) , 1)
})

test_that("weights normalize from any scale", {
  expect_equal(normalize_weights(c(1, 2)), c(1 / 3, 2 / 3))
  expect_equal(normalize_weights(c(40, 30, 30)), c(0.4, 0.3, 0.3))
  expect_equal(sum(normalize_weights(c(7, 11, 13))), 1)
  expect_equal(normalize_weights(c(1, 0)), c(1, 0))
})
