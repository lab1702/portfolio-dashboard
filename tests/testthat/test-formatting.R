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

test_that("sharpe_caption discloses the risk-free rate it was graded against", {
  cap <- sharpe_caption(1.4)
  expect_true(grepl("good", cap, fixed = TRUE))
  expect_match(cap, "risk-free")
  expect_true(grepl(fmt_pct(RISK_FREE_RATE), cap, fixed = TRUE))
  # the grade still tracks the verdict bands
  expect_true(grepl("poor", sharpe_caption(-0.2), fixed = TRUE))
})

test_that("month labels are the English abbreviations for all twelve months", {
  d <- as.Date(sprintf("2021-%02d-15", 1:12))
  expect_equal(fmt_month_year(d), paste(MONTH_ABB, "2021"))
  expect_equal(fmt_month_day_year(d), paste0(MONTH_ABB, " 15, 2021"))
})

test_that("month formatters pass NA through rather than printing 'NA NA, NA'", {
  expect_true(is.na(fmt_month_day_year(as.Date(NA))))
  expect_true(is.na(fmt_month_year(as.Date(NA))))
})

test_that("entity_chip escapes symbol text instead of emitting raw HTML", {
  chip <- entity_chip('A<img src=x onerror="boom()">', "#2a78d6")
  expect_false(grepl("<img", chip, fixed = TRUE))
  expect_true(grepl("&lt;img", chip, fixed = TRUE))
  # Whatever the label contained, the only markup left is the chip's own span:
  # an onerror= with no tag to hang off is inert text, so the check is that no
  # angle bracket survives rather than that the substring disappears.
  body <- sub('^<span class="entity-chip" style="background:[^"]*"></span>', "", chip)
  expect_false(grepl("[<>]", body))
})

test_that("entity_chip escapes the colour it writes into the style attribute", {
  chip <- entity_chip("VOO", '#fff" onmouseover="boom()')
  expect_false(grepl('" onmouseover', chip, fixed = TRUE))
  expect_true(grepl("&quot;", chip, fixed = TRUE))
})

test_that("entity_chip renders an ordinary symbol unchanged", {
  expect_equal(entity_chip("VOO", "#2a78d6"),
    '<span class="entity-chip" style="background:#2a78d6"></span>VOO')
})

test_that("entity_chip builds a whole column at once", {
  # the stats table hands it every row label in one call
  chips <- entity_chip(c("Portfolio", "SPY"), c(PORTFOLIO_INK, SERIES_SLOTS[1]))
  expect_length(chips, 2)
  expect_true(grepl(PORTFOLIO_INK, chips[1], fixed = TRUE))
  expect_true(grepl("SPY", chips[2], fixed = TRUE))
})
