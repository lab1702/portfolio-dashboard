test_that("entity_colors is deterministic, and anchors the fixed entities", {
  a <- entity_colors("SPY", c("VOO", "TQQQ", "SCHD"))
  expect_equal(a, entity_colors("SPY", c("VOO", "TQQQ", "SCHD")))

  # The subject and the benchmark keep their colour whatever the holdings are:
  # those two are the entities that appear in every chart and every table.
  b <- entity_colors("SPY", c("VTI", "BND"))
  expect_equal(a[["Portfolio"]], b[["Portfolio"]])
  expect_equal(a[["SPY"]], b[["SPY"]])

  # A holding's colour follows its position in the symbol list. With only eight
  # slots that is the most stability available; see entity_colors() for why.
  expect_equal(a[["VOO"]], b[["VTI"]])
})

test_that("entity_colors gives every drawn series a distinct colour", {
  cols <- entity_colors("SPY", c("VOO", "TQQQ", "SCHD"))
  expect_equal(names(cols), c("Portfolio", "SPY", "VOO", "TQQQ", "SCHD"))
  expect_false(anyDuplicated(cols) > 0)
  expect_equal(cols[["Portfolio"]], PORTFOLIO_INK)
  expect_equal(cols[["SPY"]], SERIES_SLOTS[1])
})

test_that("entity_colors does not recycle hues past the last slot", {
  syms <- sprintf("S%02d", 1:12)
  cols <- entity_colors("SPY", syms)
  expect_equal(unname(cols[2:9]), SERIES_SLOTS)   # bench + first 7 holdings
  expect_true(all(cols[10:13] == INK_MUTED))      # overflow, never a repeat hue
})

test_that("entity_colors folds a benchmark that is also a holding", {
  cols <- entity_colors("VOO", c("VOO", "SCHD"))
  expect_equal(names(cols), c("Portfolio", "VOO", "SCHD"))
  expect_false(anyDuplicated(cols) > 0)
})

test_that("top_holdings keeps everything until the palette runs out", {
  syms <- c("A", "B", "C")
  expect_equal(top_holdings(syms, c(1, 1, 1)), syms)
  # seven holdings fit: the benchmark has already taken the first slot
  expect_equal(top_holdings(sprintf("S%d", 1:7), rep(1, 7)), sprintf("S%d", 1:7))
})

test_that("top_holdings never asks for more colours than entity_colors has", {
  syms <- sprintf("S%02d", 1:20)
  kept <- top_holdings(syms, rev(seq_along(syms)))
  cols <- entity_colors("SPY", syms)
  expect_false(any(cols[kept] == INK_MUTED))  # every drawn line has a real hue
  expect_false(anyDuplicated(cols[kept]) > 0)
})

test_that("top_holdings keeps the largest positions in input order", {
  syms <- sprintf("S%d", 1:10)
  wts  <- c(1, 9, 1, 9, 9, 9, 9, 9, 9, 1)  # the three 1s are the smallest
  kept <- top_holdings(syms, wts)
  expect_equal(kept, c("S2", "S4", "S5", "S6", "S7", "S8", "S9"))
  expect_equal(length(kept), length(SERIES_SLOTS) - 1)
})

test_that("return_tint is neutral at zero and diverges by sign", {
  expect_equal(return_tint(0), SURFACE_COLOR)
  expect_equal(return_tint(NA_real_), SURFACE_COLOR)
  expect_equal(return_tint(NaN), SURFACE_COLOR)

  to_rgb <- function(hex) as.numeric(grDevices::col2rgb(hex))
  gain <- to_rgb(return_tint(5))
  loss <- to_rgb(return_tint(-5))
  expect_gt(gain[3], gain[1])   # gains lean blue
  expect_gt(loss[1], loss[3])   # losses lean red
})

test_that("return_tint deepens with magnitude and saturates at the cap", {
  blueness <- function(x) as.numeric(grDevices::col2rgb(return_tint(x, cap = 10))[1])
  expect_gt(blueness(1), blueness(5))    # less tint = closer to the pale surface
  expect_gt(blueness(5), blueness(10))
  expect_equal(blueness(10), blueness(40))  # beyond the cap, no further deepening
})

test_that("tint_cap ignores outliers but never collapses to zero", {
  expect_equal(tint_cap(c(rep(1, 100), 500)), 2)  # floor wins over a lone spike
  expect_equal(tint_cap(numeric(0)), 2)
  expect_equal(tint_cap(c(NA, NaN)), 2)
  expect_gt(tint_cap(c(rep(20, 10), -20)), 15)
})

test_that("delta_vs_benchmark pairs each sign with its own arrow", {
  ahead <- delta_vs_benchmark(0.164, 0.128)
  expect_equal(ahead$class, "delta-good")
  expect_match(ahead$text, "^▲ 3\\.6 pts$")

  behind <- delta_vs_benchmark(0.100, 0.128)
  expect_equal(behind$class, "delta-bad")
  expect_match(behind$text, "^▼ 2\\.8 pts$")     # magnitude, sign in the arrow

  expect_equal(delta_vs_benchmark(0.1, 0.1)$class, "delta-good")  # a tie is not a loss
})

test_that("rebal_label covers every choice the sidebar offers", {
  for (choice in c("months", "quarters", "years", "none")) {
    expect_true(nzchar(rebal_label(choice)))
    expect_false(is.na(rebal_label(choice)))
  }
})
