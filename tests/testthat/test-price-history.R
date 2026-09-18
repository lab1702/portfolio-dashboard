test_that("price preparation trims unavailable history only at the ends", {
  dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 45)
  prices <- xts::xts(cbind(A = c(rep(NA, 5), 101:140),
                            BEN = c(201:241, rep(NA, 4))), dates)
  prepared <- prepare_price_history(prices)
  expect_null(prepared$errors)
  expect_equal(prepared$prices, prices[6:41, ])
})

test_that("a weekday benchmark cannot erase a crypto weekend drawdown", {
  dates <- seq(as.Date("2025-01-03"), by = "day", length.out = 45)
  crypto <- rep(100, length(dates))
  crypto[2] <- 80 # Saturday loss, recovered before the benchmark reopens.
  bench <- rep(100, length(dates))
  bench[format(dates, "%u") %in% c("6", "7")] <- NA
  prices <- xts::xts(cbind(BTC = crypto, SPY = bench), dates)
  prepared <- prepare_price_history(prices)
  expect_null(prepared$prices)
  expect_match(prepared$errors, "matching trading dates", fixed = TRUE)

  # A benchmark with the same daily calendar preserves that observed loss.
  prices[, "SPY"] <- 100
  prepared <- prepare_price_history(prices)
  expect_null(prepared$errors)
  bt <- compute_backtest(prepared$prices, "BTC", 1, "SPY", "none")
  expect_equal(ann_maxdd(bt$port), 0.2, tolerance = 1e-12)
})

test_that("interior missing quotes are reported rather than removed", {
  dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 45)
  prices <- xts::xts(cbind(A = 101:145, BEN = 201:245), dates)
  prices[20, "A"] <- NA
  prepared <- prepare_price_history(prices)
  expect_null(prepared$prices)
  expect_match(prepared$errors, "Some prices are missing", fixed = TRUE)
})

test_that("empty, disjoint, or short histories report insufficient overlap", {
  dates <- seq(as.Date("2025-01-01"), by = "day", length.out = 45)
  prices <- xts::xts(cbind(A = 101:145, BEN = 201:245), dates)
  expect_match(prepare_price_history(prices[1:30, ])$errors, "Not enough")
  expect_match(prepare_price_history(prices[FALSE, ])$errors, "Not enough")
  prices[, "A"] <- NA
  expect_match(prepare_price_history(prices)$errors, "Not enough")
  prices[1:10, "A"] <- 100
  prices[1:20, "BEN"] <- NA
  expect_match(prepare_price_history(prices)$errors, "Not enough")
})
