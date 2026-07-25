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

# The pairing of "which holdings are drawn" with "what colour each one gets" is
# the invariant that matters, and only display_plan() decides both. Asking
# top_holdings() and entity_colors() separately is what let them disagree:
# selection is by weight, assignment was by list position, and a fixture whose
# weights happened to descend in list order hid the collision for a whole round.
test_that("every drawn holding gets a distinct real hue, wherever the weight sits", {
  syms <- sprintf("S%02d", 1:20)
  fixtures <- list(
    "largest last"   = seq_along(syms),
    "largest first"  = rev(seq_along(syms)),
    "largest in back half" = c(rep(1, 10), rep(9, 10)),
    "largest split"  = rep(c(1, 9), 10)
  )
  for (nm in names(fixtures)) {
    plan  <- display_plan("SPY", syms, fixtures[[nm]])
    drawn <- plan$colors[plan$shown]
    expect_false(any(drawn == INK_MUTED), info = nm)
    expect_false(anyDuplicated(drawn) > 0, info = nm)
  }
})

test_that("display_plan anchors the portfolio and the benchmark", {
  plan <- display_plan("SPY", sprintf("S%02d", 1:20), seq_len(20))
  expect_equal(plan$colors[["Portfolio"]], PORTFOLIO_INK)
  expect_equal(plan$colors[["SPY"]], SERIES_SLOTS[1])
})

test_that("display_plan still colours every instrument for the stats table", {
  syms <- sprintf("S%02d", 1:20)
  plan <- display_plan("SPY", syms, seq_along(syms))
  expect_setequal(names(plan$colors), c("Portfolio", "SPY", syms))
})

test_that("display_plan changes nothing when every holding fits", {
  syms <- c("VOO", "TQQQ", "SCHD")
  plan <- display_plan("SPY", syms, c(40, 30, 30))
  expect_equal(plan$shown, syms)
  expect_equal(plan$colors, entity_colors("SPY", syms))
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
  # Distance from the surface, not a single channel: the old version watched the
  # red channel fall, which only holds when the surface is the lighter of the
  # two. On a dark surface the tint runs the other way and that test inverted.
  depth <- function(x) delta_e2000(hex_to_lab(SURFACE_COLOR),
                                   hex_to_lab(return_tint(x, cap = 10)))
  expect_lt(depth(1), depth(5))
  expect_lt(depth(5), depth(10))
  expect_equal(depth(10), depth(40))   # beyond the cap, no further deepening
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

test_that("delta_vs_benchmark reports n/a instead of inventing a shortfall", {
  # isTRUE(NA >= 0) is FALSE, which used to render a confident "▼ NA pts"
  for (bad in list(NA_real_, NaN, Inf, -Inf)) {
    d <- delta_vs_benchmark(bad, 0.1)
    expect_equal(d$text, "n/a")
    expect_equal(d$class, "delta-muted")
  }
  expect_equal(delta_vs_benchmark(0.1, NA_real_)$text, "n/a")
})

test_that("perf chart alt text carries the run's own numbers", {
  dates <- seq(as.Date("2021-01-04"), by = "day", length.out = 400)
  port  <- xts::xts(rep(0.0010, 400), order.by = dates)
  bench <- xts::xts(rep(0.0005, 400), order.by = dates)
  colnames(port) <- "Portfolio"

  txt <- perf_chart_alt(port, bench, "SPY")
  expect_match(txt, "SPY", fixed = TRUE)
  expect_match(txt, "Jan 2021", fixed = TRUE)          # window, not "Plot object"
  expect_match(txt, fmt_dollar(grew_to(port)), fixed = TRUE)
  expect_match(txt, fmt_dollar(grew_to(bench)), fixed = TRUE)
  expect_match(txt, "drawdown", fixed = TRUE)          # names all three panels
  expect_false(grepl("Plot object", txt, fixed = TRUE))
})

test_that("fund chart alt text describes the lines drawn, and only those", {
  dates <- seq(as.Date("2021-01-04"), by = "day", length.out = 200)
  returns <- xts::xts(cbind(VOO = rep(0.0010, 200), TQQQ = rep(0.0020, 200),
                            SCHD = rep(0.0005, 200)), order.by = dates)

  txt <- fund_chart_alt(returns, c("VOO", "TQQQ"))
  expect_match(txt, "VOO ends at \\$\\d+\\.\\d{2}")
  expect_match(txt, "TQQQ ends at \\$\\d+\\.\\d{2}")
  # SCHD is a column of `returns` but is not drawn, so describing it would be
  # narrating a line that is not on the chart
  expect_false(grepl("SCHD", txt, fixed = TRUE))
  expect_match(txt, "Jan 2021", fixed = TRUE)
})

test_that("the chip column is named for assistive tech but not on screen", {
  expect_match(CHIP_COLUMN_HEADER, "visually-hidden", fixed = TRUE)
  # renderTable passes column names through sanitize.text.function (identity
  # here), so this has to be markup rather than a bare label
  expect_match(CHIP_COLUMN_HEADER, "^<span[^>]*>[^<]+</span>$")
})

test_that("rebal_label covers every choice the sidebar offers", {
  # REBAL_CHOICES is what the selectInput is built from, so this reads the
  # sidebar's actual vocabulary rather than a third hardcoded copy of it.
  expect_setequal(unname(REBAL_CHOICES), names(REBAL_LABELS))
  for (choice in REBAL_CHOICES) {
    expect_true(nzchar(rebal_label(choice)), info = choice)
    expect_false(is.na(rebal_label(choice)), info = choice)
  }
  expect_true(all(nzchar(names(REBAL_CHOICES))))  # every option is labelled
})

test_that("the performance caption reads as a sentence for every choice", {
  # The test above passed while the caption said "rebalanced buy & hold, never
  # rebalanced": it checked that each label existed, never that the assembled
  # sentence scanned. This asserts the whole string, for every option the
  # sidebar can produce.
  for (choice in REBAL_CHOICES) {
    txt <- perf_caption_text(c(0.4, 0.6), c("VOO", "SCHD"), choice, "SPY")
    expect_false(grepl("rebalanced.*rebalanced", txt), info = choice)
    expect_match(txt, "^40% VOO / 60% SCHD · ", info = choice)
    expect_match(txt, " · benchmark SPY$", info = choice)
  }

  expect_equal(perf_caption_text(1, "VOO", "months", "SPY"),
               "100% VOO · rebalanced monthly · benchmark SPY")
  expect_equal(perf_caption_text(1, "VOO", "none", "SPY"),
               "100% VOO · buy & hold, never rebalanced · benchmark SPY")
})

test_that("the composed performance chart draws without disturbing par()", {
  # The chart is composed from three panel functions rather than taken whole
  # from charts.PerformanceSummary, so that the drawdown panel can carry the
  # loss colour while the top panel keeps the series colours. Reproducing the
  # library's par() glue means we now own restoring it: a leaked par() would
  # corrupt every plot drawn after this one in the same session.
  dates <- seq(as.Date("2021-01-04"), by = "day", length.out = 300)
  combined <- xts::xts(cbind(Portfolio = rep(0.0010, 300),
                             SPY       = rep(0.0005, 300)), order.by = dates)

  before <- par(no.readonly = TRUE)
  pdf(NULL)                      # draw to a null device, not a file
  on.exit({ dev.off(); par(before) }, add = TRUE)

  # Not expect_silent: the panel functions are entitled to warn about a short
  # series or a degenerate axis, and a test that forbids that is a test that
  # fails for reasons having nothing to do with the chart.
  expect_no_error(chart_performance_summary(combined, PORTFOLIO_INK,
                                            SERIES_SLOTS[1]))
  expect_equal(par("mar"), before$mar)
  expect_equal(par("oma"), before$oma)
})

test_that("run_problem_banner reports every message, once, or nothing at all", {
  expect_null(run_problem_banner(character(0)))
  expect_null(run_problem_banner(NULL))

  msgs <- c("Number of symbols and weights must match.",
            "Weights cannot be negative.",
            "Years of history cannot exceed 30.")
  html <- as.character(run_problem_banner(msgs))

  # One list item per message, each appearing exactly once — the whole point of
  # the banner is that it replaced eleven copies of the same text.
  expect_equal(lengths(regmatches(html, gregexpr("<li>", html, fixed = TRUE))),
               length(msgs))
  for (m in msgs) {
    expect_equal(lengths(regmatches(html, gregexpr(m, html, fixed = TRUE))), 1L,
                 info = m)
  }

  # role="alert" is what makes a message that appears after the click reach a
  # screen reader at all.
  expect_match(html, 'role="alert"', fixed = TRUE)
})
