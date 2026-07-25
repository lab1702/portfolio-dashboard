# The palette's invariants, executable. Everything here was previously a
# sentence in a comment above SERIES_SLOTS.
#
# Thresholds are the ones the light theme claimed, unchanged: only the surface
# moved. Do not relax one to make a failure go away — change the colour.

SERIES_MIN_CONTRAST <- 3     # WCAG 1.4.11, graphical objects
ADJACENT_MIN_NORMAL <- 15    # dE2000, normal vision
ADJACENT_MIN_CVD    <- 8     # dE2000, each simulated dichromacy
TEXT_MIN_CONTRAST   <- 4.5   # WCAG 1.4.3, normal-size text

test_that("every series slot is visible against the card surface", {
  for (col in SERIES_SLOTS) {
    expect_gte(contrast_ratio(col, SURFACE_COLOR), SERIES_MIN_CONTRAST)
  }
  expect_gte(contrast_ratio(PORTFOLIO_INK, SURFACE_COLOR), SERIES_MIN_CONTRAST)
})

test_that("adjacent series slots stay apart at normal vision", {
  # Adjacent pairs, not all pairs: the *ordering* is the safety mechanism, so
  # the constraint is on the neighbours a reader actually has to tell apart.
  for (i in seq_len(length(SERIES_SLOTS) - 1)) {
    d <- delta_e2000(hex_to_lab(SERIES_SLOTS[i]),
                     hex_to_lab(SERIES_SLOTS[i + 1]))
    expect_gte(d, ADJACENT_MIN_NORMAL,
               label = sprintf("slots %d/%d (%s, %s) dE %.2f",
                               i, i + 1, SERIES_SLOTS[i], SERIES_SLOTS[i + 1], d))
  }
})

test_that("adjacent series slots stay apart under each dichromacy", {
  for (ty in CVD_TYPES) {
    for (i in seq_len(length(SERIES_SLOTS) - 1)) {
      d <- delta_e2000(lab_under_cvd(SERIES_SLOTS[i], ty),
                       lab_under_cvd(SERIES_SLOTS[i + 1], ty))
      expect_gte(d, ADJACENT_MIN_CVD,
                 label = sprintf("%s slots %d/%d (%s, %s) dE %.2f",
                                 ty, i, i + 1,
                                 SERIES_SLOTS[i], SERIES_SLOTS[i + 1], d))
    }
  }
})

test_that("the portfolio outranks every hue it is drawn against", {
  # PORTFOLIO_INK is the extreme of the ink scale rather than a ninth hue, so
  # it has to clear every slot it can sit beside in a chart.
  for (col in SERIES_SLOTS) {
    expect_gte(delta_e2000(hex_to_lab(PORTFOLIO_INK), hex_to_lab(col)),
               ADJACENT_MIN_NORMAL)
  }
})

test_that("the calendar tint never swallows the ink on top of it", {
  # return_tint mixes the surface toward a pole. On a dark surface that runs
  # *upward* in luminance, so the deepest cells are where legibility would go
  # first. tint_cap's floor is 2, so cap = 2 with a large value is the
  # steepest ramp the calendar can actually produce.
  for (x in c(100, -100)) {
    deepest <- return_tint(x, cap = 2)
    expect_gte(contrast_ratio(INK_COLOR, deepest), TEXT_MIN_CONTRAST)
  }
})

test_that("chart ink and gridlines are legible on the surface", {
  expect_gte(contrast_ratio(INK_COLOR, SURFACE_COLOR), TEXT_MIN_CONTRAST)
  expect_gte(contrast_ratio(INK_MUTED, SURFACE_COLOR), TEXT_MIN_CONTRAST)
  # Body ink outranks muted ink; muted is support, and support that reads as
  # loudly as the thing it supports is just noise.
  expect_gt(contrast_ratio(INK_COLOR, SURFACE_COLOR),
            contrast_ratio(INK_MUTED, SURFACE_COLOR))
  # The gridline is a hairline behind data, not text: it only has to be seen.
  expect_gt(contrast_ratio(GRID_COLOR, SURFACE_COLOR), 1.1)
  expect_lt(contrast_ratio(GRID_COLOR, SURFACE_COLOR),
            contrast_ratio(INK_MUTED, SURFACE_COLOR))
})

test_that("the diverging poles are distinguishable from each other", {
  expect_gte(delta_e2000(hex_to_lab(GAIN_COLOR), hex_to_lab(LOSS_COLOR)),
             ADJACENT_MIN_NORMAL)
  for (ty in CVD_TYPES) {
    expect_gte(delta_e2000(lab_under_cvd(GAIN_COLOR, ty),
                           lab_under_cvd(LOSS_COLOR, ty)),
               ADJACENT_MIN_CVD)
  }
})

# theme.scss says the palette is "defined once, in portfolio_core.R". It is not:
# six values are duplicated across the two files by hand, and the failure mode
# is silent — a card surface that no longer matches the transparent-background
# plot sitting on it reads as a rendering bug, not as a stale constant.

scss_token <- function(name, path = "../../theme.scss") {
  lines <- readLines(path, warn = FALSE)
  hit <- grep(sprintf("^\\%s:\\s*(#[0-9a-fA-F]{6})\\s*;", name), lines, value = TRUE)
  expect_length(hit, 1)
  tolower(sub(sprintf("^\\%s:\\s*(#[0-9a-fA-F]{6})\\s*;.*$", name), "\\1", hit))
}

test_that("theme.scss and portfolio_core.R agree on every shared colour", {
  expect_equal(scss_token("$viz-series-1"), tolower(SERIES_SLOTS[1]))
  expect_equal(scss_token("$viz-series-1"), tolower(GAIN_COLOR))
  expect_equal(scss_token("$viz-surface"),  tolower(SURFACE_COLOR))
  expect_equal(scss_token("$viz-grid"),     tolower(GRID_COLOR))
  expect_equal(scss_token("$viz-muted"),    tolower(INK_MUTED))
  expect_equal(scss_token("$viz-critical"), tolower(LOSS_COLOR))
  expect_equal(scss_token("$viz-ink"),      tolower(INK_COLOR))
})

test_that("the filled value box holds white text", {
  expect_gte(contrast_ratio("#ffffff", scss_token("$viz-accent-fill")),
             TEXT_MIN_CONTRAST)
  # It must also stay below the accent line colour in brightness, or the one
  # filled card becomes the brightest object on a deliberately dark page.
  expect_lt(relative_luminance(scss_token("$viz-accent-fill")),
            relative_luminance(scss_token("$viz-series-1")))
})

test_that("secondary ink clears AA on the card surface", {
  expect_gte(contrast_ratio(scss_token("$viz-ink-2"), SURFACE_COLOR),
             TEXT_MIN_CONTRAST)
  expect_gte(contrast_ratio(scss_token("$viz-good"), SURFACE_COLOR),
             TEXT_MIN_CONTRAST)
})
