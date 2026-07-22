# Synthetic-fixture tests for compute_backtest and the table builders.
# No network access required.

weekday_dates <- function(start, n) {
  d <- seq(as.Date(start), by = "day", length.out = ceiling(n * 1.5) + 7)
  d <- d[format(d, "%u") %in% c("1", "2", "3", "4", "5")]
  d[seq_len(n)]
}

make_prices <- function(dates, ...) {
  xts::xts(do.call(cbind, list(...)), order.by = dates)
}

test_that("CAGR is exactly consistent with growth of $10,000", {
  dates <- weekday_dates("2020-01-06", 520)
  pA <- 100 * cumprod(c(1, rep(1.0006, 519)))
  pB <- 80  * cumprod(c(1, rep(1.0002, 519)))
  prices <- make_prices(dates, A = pA, B = pB, BEN = pB)
  bt <- compute_backtest(prices, c("A", "B"), c(0.5, 0.5), "BEN", "months")

  span <- as.numeric(difftime(end(prices), start(prices), units = "days"))
  implied <- (grew_to(bt$port) / START_CAPITAL)^(365.25 / span) - 1
  cagr <- as.numeric(Return.annualized(bt$port, scale = bt$scale)[1])
  expect_equal(cagr, implied, tolerance = 1e-10)
})

test_that("CAGR is invariant to the sampling grid (round-3 regression)", {
  # same continuously-growing price observed Mon-Fri vs Mon-Thu must give
  # the same CAGR; 2020-01-06 is a Monday, 2021-12-30 a Thursday
  all_days <- seq(as.Date("2020-01-06"), as.Date("2021-12-30"), by = "day")
  price <- function(d) 100 * exp(0.0004 * as.numeric(d - all_days[1]))
  monfri <- all_days[format(all_days, "%u") %in% c("1", "2", "3", "4", "5")]
  monthu <- all_days[format(all_days, "%u") %in% c("1", "2", "3", "4")]

  bt1 <- compute_backtest(make_prices(monfri, A = price(monfri), BEN = price(monfri)),
                          "A", 1, "BEN", "none")
  bt2 <- compute_backtest(make_prices(monthu, A = price(monthu), BEN = price(monthu)),
                          "A", 1, "BEN", "none")
  c1 <- as.numeric(Return.annualized(bt1$port, scale = bt1$scale)[1])
  c2 <- as.numeric(Return.annualized(bt2$port, scale = bt2$scale)[1])
  expect_equal(c1, c2, tolerance = 1e-10)
})

test_that("effective scale reflects observation frequency", {
  wd <- weekday_dates("2020-01-06", 260)
  p <- 100 * cumprod(c(1, rep(1.0005, 259)))
  # a synthetic no-holiday weekday calendar has ~261 obs/year (5/7 of 365.25);
  # real markets land near 251 because of holidays
  bt_wd <- compute_backtest(make_prices(wd, A = p, BEN = p), "A", 1, "BEN", "none")
  expect_gt(bt_wd$scale, 255)
  expect_lt(bt_wd$scale, 266)

  daily <- seq(as.Date("2020-01-01"), by = "day", length.out = 400)
  p2 <- 100 * cumprod(c(1, rep(1.0003, 399)))
  bt_daily <- compute_backtest(make_prices(daily, A = p2, BEN = p2), "A", 1, "BEN", "none")
  expect_equal(bt_daily$scale, 365.25, tolerance = 0.01)
})

test_that("zero-weight asset does not affect the portfolio", {
  set.seed(42)
  dates <- weekday_dates("2020-01-06", 200)
  pA <- 100 * cumprod(c(1, 1 + rnorm(199, 0.0005, 0.01)))
  pB <- 100 * cumprod(c(1, 1 + rnorm(199, 0.0002, 0.02)))
  prices <- make_prices(dates, A = pA, B = pB, BEN = pB)
  bt <- compute_backtest(prices, c("A", "B"), c(1, 0), "BEN", "months")
  expect_equal(as.numeric(bt$port), as.numeric(bt$returns[, "A"]),
               tolerance = 1e-12)
})

test_that("non-syntactic tickers keep their names end-to-end", {
  dates <- weekday_dates("2020-01-06", 100)
  p <- 100 * cumprod(c(1, rep(1.001, 99)))
  prices <- xts::xts(cbind(p, p), order.by = dates)
  colnames(prices) <- c("BRK-B", "^GSPC")

  bt <- compute_backtest(prices, "BRK-B", 1, "^GSPC", "none")
  expect_equal(colnames(bt$combined), c("Portfolio", "^GSPC"))

  st <- build_stats_table(bt$port, bt$returns, "^GSPC", "BRK-B", bt$scale)
  expect_equal(rownames(st), c("Portfolio", "^GSPC", "BRK-B"))
})

test_that("benchmark that is also a holding is not duplicated in stats", {
  set.seed(7)
  dates <- weekday_dates("2020-01-06", 150)
  pA <- 100 * cumprod(c(1, 1 + rnorm(149, 5e-4, 0.01)))
  pB <- 100 * cumprod(c(1, 1 + rnorm(149, 2e-4, 0.01)))
  prices <- make_prices(dates, A = pA, B = pB)
  bt <- compute_backtest(prices, c("A", "B"), c(0.5, 0.5), "A", "months")
  st <- build_stats_table(bt$port, bt$returns, "A", c("A", "B"), bt$scale)
  expect_equal(rownames(st), c("Portfolio", "A", "B"))
})

test_that("a 100% single-holding portfolio matches that holding's stats", {
  set.seed(11)
  dates <- weekday_dates("2020-01-06", 300)
  pA <- 100 * cumprod(c(1, 1 + rnorm(299, 5e-4, 0.012)))
  pB <- 100 * cumprod(c(1, 1 + rnorm(299, 3e-4, 0.009)))
  prices <- make_prices(dates, A = pA, B = pB)
  bt <- compute_backtest(prices, "A", 1, "B", "none")
  st <- build_stats_table(bt$port, bt$returns, "B", "A", bt$scale)
  expect_equal(st["Portfolio", "Return (ann.)"], st["A", "Return (ann.)"])
  expect_equal(st["Portfolio", "Sharpe"], st["A", "Sharpe"])
  expect_equal(st["Portfolio", "Max Drawdown"], st["A", "Max Drawdown"])
})

test_that("drawdown table formats depth, dates, and recovery", {
  dates <- weekday_dates("2020-01-06", 40)
  pv <- c(100, 110, 99, 90, 111, seq(112, length.out = 35, by = 1))
  r <- xts::xts(pv[-1] / pv[-length(pv)] - 1, order.by = dates[-1])
  colnames(r) <- "Portfolio"

  dd <- suppressWarnings(build_drawdown_table(r, scale = 251))
  expect_equal(names(dd), c("From", "Trough", "To", "Depth", "Trading Days"))
  expect_equal(dd$Depth[1], "-18.2%")  # 90 / 110 - 1
  expect_false(any(dd$To == "ongoing"))
  expect_true(grepl("^[A-Z][a-z]{2} \\d{2}, \\d{4}$", dd$From[1]))
})

test_that("unrecovered drawdowns show 'ongoing' and crypto scale uses Days", {
  dates <- seq(as.Date("2021-01-01"), by = "day", length.out = 38)
  pv <- c(seq(100, length.out = 35, by = 1), 130, 120, 118)
  r <- xts::xts(pv[-1] / pv[-length(pv)] - 1, order.by = dates[-1])
  colnames(r) <- "Portfolio"

  dd <- suppressWarnings(build_drawdown_table(r, scale = 365.4))
  expect_equal(names(dd)[5], "Days")
  expect_true("ongoing" %in% dd$To)
})
