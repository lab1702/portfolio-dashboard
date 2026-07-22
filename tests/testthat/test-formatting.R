test_that("fmt_pct formats fractions with one decimal", {
  expect_equal(fmt_pct(0.165), "16.5%")
  expect_equal(fmt_pct(-0.441), "-44.1%")
  expect_equal(fmt_pct(0), "0.0%")
})

test_that("fmt_dollar groups thousands and never goes scientific", {
  expect_equal(fmt_dollar(21328.4), "$21,328")
  expect_equal(fmt_dollar(9999), "$9,999")
  # regression: format() prefers "1e+05"-style output for round powers of ten
  expect_equal(fmt_dollar(1e5), "$100,000")
  expect_equal(fmt_dollar(2e5), "$200,000")
  expect_equal(fmt_dollar(1e6), "$1,000,000")
})

test_that("grew_to compounds from starting capital", {
  expect_equal(grew_to(c(0.1, -0.05)), 10000 * 1.1 * 0.95)
  expect_equal(grew_to(numeric(0)), 10000)
  expect_equal(grew_to(0.5, capital = 100), 150)
})

test_that("sharpe_verdict bands match the '1+ good, 2+ great' caption", {
  expect_equal(sharpe_verdict(-0.1), "poor")
  expect_equal(sharpe_verdict(0), "fair")     # exactly 0 is not negative
  expect_equal(sharpe_verdict(0.99), "fair")
  expect_equal(sharpe_verdict(1), "good")     # boundary: 1+ is good
  expect_equal(sharpe_verdict(1.99), "good")
  expect_equal(sharpe_verdict(2), "great")    # boundary: 2+ is great
  expect_equal(sharpe_verdict(NaN), "n/a")
  expect_equal(sharpe_verdict(Inf), "n/a")
})
