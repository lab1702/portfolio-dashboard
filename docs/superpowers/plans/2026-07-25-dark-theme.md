# Dark Theme Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the dashboard's light theme with a true-black dark theme whose
palette is validated by tests rather than asserted in comments.

**Architecture:** Colour lives in two files — `theme.scss` for chrome,
`portfolio_core.R` for everything painted in R — and five values are duplicated
across them by hand. All five change here, so a test asserts the seam. A new
test-only colour-maths helper (CIE Lab, ΔE2000, dichromat simulation) turns the
palette's stated invariants into executable ones. The performance chart is
recomposed from the three PerformanceAnalytics panel functions so the drawdown
panel can take its own colour.

**Tech Stack:** R, Quarto dashboard, Shiny, testthat, PerformanceAnalytics, SCSS.

**Design spec:** `docs/superpowers/specs/2026-07-25-dark-mode-design.md`

## Global Constraints

- **No new package dependencies.** Colour maths uses base R and `grDevices` only.
- **The colour maths is test-only.** `helper-color.R` lives in `tests/testthat/`
  and is never sourced by the application. `test_dir()` picks up `helper-*.R`
  automatically.
- **Thresholds, exact:** series slot vs card surface ≥ **3:1**; adjacent slot
  pair ≥ **15** ΔE2000 at normal vision; adjacent slot pair ≥ **8** ΔE2000 under
  each of protanopia, deuteranopia, tritanopia; text vs its background ≥ **4.5:1**.
- **Never relax a threshold to make a test pass.** If a value fails, change the
  value. If no value works, stop and report.
- ΔE2000 is CIEDE2000 per Sharma, Wu & Dalal (2005).
- Dichromat simulation is Viénot, Brettel & Mollon (1999), applied in **linear**
  RGB via LMS.
- Test runner: `Rscript tests/run_tests.R` from the project root, which does
  `test_dir("tests/testthat", stop_on_failure = TRUE)`.
- On this machine R is at `C:\Program Files\R\R-4.6.1\bin\x64\Rscript.exe`.
- Comments in this repo explain *why*, not *what*. Match that.

## File Structure

| file | responsibility |
|---|---|
| `tests/testthat/helper-color.R` | **new** — colour space conversion, contrast, ΔE2000, CVD simulation. Pure functions, no project knowledge. |
| `tests/testthat/test-color-math.R` | **new** — proves the helper against published reference values. |
| `tests/testthat/test-palette.R` | **new** — proves the *project's* palette against the thresholds, plus the scss/R seam. |
| `portfolio_core.R` | colour constants re-derived; new `chart_performance_summary()`. |
| `theme.scss` | chrome tokens and rules for the black surface. |
| `portfolio_dashboard.qmd` | `perf_chart` calls the new helper. |
| `tests/testthat/test-visuals.R` | one existing test rewritten (see Task 3). |

The two new test files are split because they fail for different reasons and a
reviewer should be able to tell them apart: `test-color-math.R` failing means
the maths is wrong, `test-palette.R` failing means a colour is wrong.

---

### Task 1: Colour maths helper

**Files:**
- Create: `tests/testthat/helper-color.R`
- Test: `tests/testthat/test-color-math.R`

**Interfaces:**
- Consumes: nothing.
- Produces, all used by Tasks 2–3:
  - `hex_to_rgb01(hex) -> numeric(3)` — sRGB channels in 0..1
  - `relative_luminance(hex) -> numeric(1)` — WCAG relative luminance
  - `contrast_ratio(hex_a, hex_b) -> numeric(1)` — WCAG ratio, order-independent
  - `hex_to_lab(hex) -> numeric(3)` — CIE L\*a\*b\*, D65
  - `delta_e2000(lab_a, lab_b) -> numeric(1)` — CIEDE2000
  - `simulate_cvd(hex, type) -> character(1)` — hex as seen under `type`
  - `lab_under_cvd(hex, type) -> numeric(3)` — `hex_to_lab(simulate_cvd(...))`
  - `CVD_TYPES -> c("protan", "deutan", "tritan")`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-color-math.R`:

```r
# The colour maths is only worth having if it agrees with the published
# reference data. These are the CIEDE2000 pairs from Sharma, Wu & Dalal (2005),
# Table 1 — the standard conformance set for the formula.

test_that("contrast_ratio matches the WCAG definition at its endpoints", {
  expect_equal(contrast_ratio("#ffffff", "#000000"), 21, tolerance = 1e-9)
  expect_equal(contrast_ratio("#0f0f0f", "#0f0f0f"), 1, tolerance = 1e-9)
  # order-independent: it is a ratio of the lighter to the darker
  expect_equal(contrast_ratio("#ffffff", "#4f9bf0"),
               contrast_ratio("#4f9bf0", "#ffffff"))
})

test_that("hex_to_lab anchors the achromatic axis", {
  expect_equal(hex_to_lab("#ffffff"), c(100, 0, 0), tolerance = 1e-3)
  expect_equal(hex_to_lab("#000000"), c(0, 0, 0), tolerance = 1e-9)
})

test_that("delta_e2000 reproduces the Sharma reference pairs", {
  cases <- list(
    list(c(50,  2.6772, -79.7751), c(50,  0.0000, -82.7485), 2.0425),
    list(c(50,  3.1571, -77.2803), c(50,  0.0000, -82.7485), 2.8615),
    list(c(50,  2.8361, -74.0200), c(50,  0.0000, -82.7485), 3.4412),
    list(c(50, -1.3802, -84.2814), c(50,  0.0000, -82.7485), 1.0000),
    list(c(50, -1.1848, -84.8006), c(50,  0.0000, -82.7485), 1.0000),
    list(c(50,  0.0000,   0.0000), c(50, -1.0000,   2.0000), 2.3669),
    list(c(50,  2.4900,  -0.0010), c(50, -2.4900,   0.0009), 7.1792),
    list(c(60.2574, -34.0099, 36.2677),
         c(60.4626, -34.1751, 39.4387), 1.2644)
  )
  for (k in cases) {
    expect_equal(delta_e2000(k[[1]], k[[2]]), k[[3]], tolerance = 1e-4,
                 info = sprintf("expected %.4f", k[[3]]))
  }
})

test_that("delta_e2000 is a metric on the cases we rely on", {
  a <- hex_to_lab("#4f9bf0"); b <- hex_to_lab("#ff6b6b")
  expect_equal(delta_e2000(a, a), 0, tolerance = 1e-12)
  expect_equal(delta_e2000(a, b), delta_e2000(b, a), tolerance = 1e-9)
})

test_that("simulate_cvd collapses each dichromacy's own confusion pair", {
  # One pair per type, each chosen because the simulation flattens it: a normal
  # observer separates them easily, the simulated observer barely at all. This
  # is the property the palette test leans on, so a simulation that quietly did
  # nothing has to fail here.
  #
  # The obvious pairs do not work and are a trap. Pure red against pure green
  # under protanopia stays 45.8 apart, because protanopia darkens red and the
  # lightness difference survives; pure blue against pure yellow under
  # tritanopia does not move at all, because that pair differs almost entirely
  # in lightness. Both would be tests that assert nothing. These pairs are
  # matched closely enough in lightness that only the confused axis separates
  # them.
  confusion <- list(
    protan = c("#80c0c0", "#ffc0c0"),   # cyan/pink, the red-green axis
    deutan = c("#80c080", "#ff8080"),   # green/salmon
    tritan = c("#ff0000", "#ff00ff")    # red/magenta, the blue-yellow axis
  )
  for (ty in names(confusion)) {
    pair <- confusion[[ty]]
    normal <- delta_e2000(hex_to_lab(pair[1]), hex_to_lab(pair[2]))
    simulated <- delta_e2000(lab_under_cvd(pair[1], ty),
                             lab_under_cvd(pair[2], ty))
    expect_gt(normal, 40)      # clearly two colours to a normal observer
    expect_lt(simulated, 10)   # barely one to this one
  }
})

test_that("simulate_cvd returns a usable hex and is idempotent in gamut", {
  for (ty in CVD_TYPES) {
    out <- simulate_cvd("#4f9bf0", ty)
    expect_match(out, "^#[0-9a-fA-F]{6}$", info = ty)
    expect_equal(simulate_cvd(out, ty), out, info = ty)
  }
})
```

- [ ] **Step 2: Run the test to verify it fails**

```bash
Rscript tests/run_tests.R
```

Expected: FAIL, `could not find function "contrast_ratio"`.

- [ ] **Step 3: Write the helper**

Create `tests/testthat/helper-color.R`:

```r
# Colour maths for the palette tests. Test-only: the dashboard never needs any
# of this at runtime, so it stays out of portfolio_core.R.
#
# The palette's invariants — "adjacent slots stay apart, including for a
# colourblind reader" — used to be a sentence in a comment with nothing behind
# it. This is what puts something behind it.

hex_to_rgb01 <- function(hex) as.numeric(grDevices::col2rgb(hex)[, 1]) / 255

srgb_to_linear <- function(c) {
  ifelse(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055)^2.4)
}

linear_to_srgb <- function(c) {
  ifelse(c <= 0.0031308, c * 12.92, 1.055 * c^(1 / 2.4) - 0.055)
}

relative_luminance <- function(hex) {
  sum(c(0.2126, 0.7152, 0.0722) * srgb_to_linear(hex_to_rgb01(hex)))
}

contrast_ratio <- function(hex_a, hex_b) {
  la <- relative_luminance(hex_a)
  lb <- relative_luminance(hex_b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

# sRGB D65 primaries.
.RGB_TO_XYZ <- matrix(c(0.4124564, 0.3575761, 0.1804375,
                        0.2126729, 0.7151522, 0.0721750,
                        0.0193339, 0.1191920, 0.9503041),
                      nrow = 3, byrow = TRUE)

.D65 <- c(0.95047, 1.00000, 1.08883)

hex_to_lab <- function(hex) {
  xyz <- as.numeric(.RGB_TO_XYZ %*% srgb_to_linear(hex_to_rgb01(hex))) / .D65
  f <- ifelse(xyz > (6 / 29)^3, xyz^(1 / 3), xyz / (3 * (6 / 29)^2) + 4 / 29)
  c(116 * f[2] - 16, 500 * (f[1] - f[2]), 200 * (f[2] - f[3]))
}

# CIEDE2000, per Sharma, Wu & Dalal (2005). Verified against that paper's
# conformance pairs in test-color-math.R. Written out longhand rather than
# simplified: the hue-difference branches and the RT rotation term are exactly
# where a compressed version goes quietly wrong.
delta_e2000 <- function(lab_a, lab_b) {
  L1 <- lab_a[1]; a1 <- lab_a[2]; b1 <- lab_a[3]
  L2 <- lab_b[1]; a2 <- lab_b[2]; b2 <- lab_b[3]

  C1 <- sqrt(a1^2 + b1^2); C2 <- sqrt(a2^2 + b2^2)
  Cbar <- (C1 + C2) / 2
  G <- 0.5 * (1 - sqrt(Cbar^7 / (Cbar^7 + 25^7)))

  a1p <- (1 + G) * a1; a2p <- (1 + G) * a2
  C1p <- sqrt(a1p^2 + b1^2); C2p <- sqrt(a2p^2 + b2^2)

  h1p <- if (a1p == 0 && b1 == 0) 0 else atan2(b1, a1p) %% (2 * pi)
  h2p <- if (a2p == 0 && b2 == 0) 0 else atan2(b2, a2p) %% (2 * pi)

  dLp <- L2 - L1
  dCp <- C2p - C1p
  dhp <- if (C1p * C2p == 0) 0
         else if (abs(h2p - h1p) <= pi) h2p - h1p
         else if (h2p - h1p > pi) h2p - h1p - 2 * pi
         else h2p - h1p + 2 * pi
  dHp <- 2 * sqrt(C1p * C2p) * sin(dhp / 2)

  Lbar <- (L1 + L2) / 2
  Cbarp <- (C1p + C2p) / 2
  hbarp <- if (C1p * C2p == 0) h1p + h2p
           else if (abs(h1p - h2p) <= pi) (h1p + h2p) / 2
           else if (h1p + h2p < 2 * pi) (h1p + h2p + 2 * pi) / 2
           else (h1p + h2p - 2 * pi) / 2

  T_ <- 1 - 0.17 * cos(hbarp - pi / 6) +
            0.24 * cos(2 * hbarp) +
            0.32 * cos(3 * hbarp + pi / 30) -
            0.20 * cos(4 * hbarp - 63 * pi / 180)

  dTheta <- (30 * pi / 180) * exp(-(((hbarp * 180 / pi) - 275) / 25)^2)
  RC <- 2 * sqrt(Cbarp^7 / (Cbarp^7 + 25^7))
  SL <- 1 + (0.015 * (Lbar - 50)^2) / sqrt(20 + (Lbar - 50)^2)
  SC <- 1 + 0.045 * Cbarp
  SH <- 1 + 0.015 * Cbarp * T_
  RT <- -sin(2 * dTheta) * RC

  sqrt((dLp / SL)^2 + (dCp / SC)^2 + (dHp / SH)^2 +
       RT * (dCp / SC) * (dHp / SH))
}

# Vienot, Brettel & Mollon (1999). The dichromat matrices act in LMS on
# *linear* RGB; applying them to gamma-encoded values is the usual way this
# gets implemented wrong, and it understates how close two hues become.
.RGB_TO_LMS <- matrix(c(17.8824,    43.5161,   4.11935,
                         3.45565,   27.1554,   3.86714,
                         0.0299566,  0.184309, 1.46709),
                      nrow = 3, byrow = TRUE)

.LMS_TO_RGB <- solve(.RGB_TO_LMS)

.CVD_MATRIX <- list(
  protan = matrix(c(0, 2.02344, -2.52581,
                    0, 1,        0,
                    0, 0,        1), nrow = 3, byrow = TRUE),
  deutan = matrix(c(1,        0, 0,
                    0.494207, 0, 1.24827,
                    0,        0, 1), nrow = 3, byrow = TRUE),
  tritan = matrix(c( 1,        0,        0,
                     0,        1,        0,
                    -0.395913, 0.801109, 0), nrow = 3, byrow = TRUE)
)

CVD_TYPES <- names(.CVD_MATRIX)

simulate_cvd <- function(hex, type) {
  stopifnot(type %in% CVD_TYPES)
  lms <- .RGB_TO_LMS %*% srgb_to_linear(hex_to_rgb01(hex))
  out <- as.numeric(.LMS_TO_RGB %*% (.CVD_MATRIX[[type]] %*% lms))
  # Clamp in linear space before encoding: the projection can land outside the
  # sRGB cube, and an out-of-gamut channel is not a colour anyone will see.
  ch <- round(255 * linear_to_srgb(pmin(pmax(out, 0), 1)))
  grDevices::rgb(ch[1], ch[2], ch[3], maxColorValue = 255)
}

lab_under_cvd <- function(hex, type) hex_to_lab(simulate_cvd(hex, type))
```

- [ ] **Step 4: Run the test to verify it passes**

```bash
Rscript tests/run_tests.R
```

Expected: PASS. All existing tests still pass — nothing the app uses has changed yet.

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/helper-color.R tests/testthat/test-color-math.R
git commit -m "Add colour maths so the palette's claims can be checked"
```

---

### Task 2: Re-derive the R colour constants

**Files:**
- Modify: `portfolio_core.R:19-32` (the colour constants and their comments)
- Modify: `portfolio_core.R:98-105` (`return_tint` ramp bounds)
- Modify: `tests/testthat/test-visuals.R:104-109` (one test that assumes a light surface)
- Test: `tests/testthat/test-palette.R` (create)

**Interfaces:**
- Consumes: everything from Task 1.
- Produces: the constants `PORTFOLIO_INK`, `SERIES_SLOTS`, `INK_MUTED`,
  `GRID_COLOR`, `SURFACE_COLOR`, `GAIN_COLOR`, `LOSS_COLOR` at their new values.
  Task 3 asserts five of them against `theme.scss`.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-palette.R`:

```r
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
  expect_gt(contrast_ratio(GRID_COLOR, SURFACE_COLOR), 1.2)
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
```

Note this test refers to `INK_COLOR`, which does not exist yet. Step 3 adds it.

- [ ] **Step 2: Run the test to verify it fails**

```bash
Rscript tests/run_tests.R
```

Expected: FAIL. `object 'INK_COLOR' not found`, and the contrast assertions fail
against the current light constants.

- [ ] **Step 3: Re-derive the constants**

In `portfolio_core.R`, replace lines 15–32 with:

```r
# The portfolio is the subject of the dashboard rather than one instrument among
# many, so it wears primary ink at the heaviest weight instead of a hue. On a
# black surface that extreme is white — the same rule as the light theme, the
# other way up. White is a rank here, not a default: body ink is INK_COLOR, one
# step down, so that the portfolio's line and figure are the only pure white on
# the page.

PORTFOLIO_INK <- "#ffffff"

# Validated categorical order for the #0f0f0f card surface: worst adjacent pair
# is dE2000 45.6 at normal vision and 9.95 under simulated dichromacy. The
# *ordering* is the colourblind-safety mechanism, not decoration — these eight
# hues in the order they were first authored fail, amber against pink at dE 4.5
# under tritanopia. test-palette.R holds the thresholds; re-run it rather than
# trusting this comment.
SERIES_SLOTS <- c("#4f9bf0", "#ffc53d", "#a98cff", "#ff8a4d",
                  "#35d39a", "#ff92be", "#7ee36b", "#ff6b6b")

INK_COLOR     <- "#ededed"  # body ink; pure white is reserved, see above
INK_MUTED     <- "#858585"  # axis labels, captions
GRID_COLOR    <- "#232323"  # hairline gridlines
SURFACE_COLOR <- "#0f0f0f"  # card surface the plots sit on
GAIN_COLOR    <- "#4f9bf0"  # diverging pole: gains
LOSS_COLOR    <- "#e05c5c"  # diverging pole: losses
```

Then in `return_tint()`, change the ramp bounds:

```r
return_tint <- function(x, cap = 10) {
  if (!is.finite(x) || x == 0) return(SURFACE_COLOR)
  # 0.10-0.45, not the light theme's 0.12-0.60. On black the tint runs upward
  # in luminance, so a full-weight calendar would be the brightest region on a
  # page whose whole point is not being bright. Legibility is not the binding
  # constraint here — even 0.60 clears 5.3:1 — restraint is.
  weight <- 0.10 + 0.35 * min(abs(x) / cap, 1)
  pole <- grDevices::col2rgb(if (x > 0) GAIN_COLOR else LOSS_COLOR)[, 1]
  base <- grDevices::col2rgb(SURFACE_COLOR)[, 1]
  mix <- round((1 - weight) * base + weight * pole)
  grDevices::rgb(mix[1], mix[2], mix[3], maxColorValue = 255)
}
```

- [ ] **Step 4: Fix the one existing test that assumed a light surface**

`test-visuals.R:104-109` measures "tint deepens with magnitude" by the **red
channel falling** — true only when mixing a pale surface toward blue. On black
the red channel *rises*, so the test now fails while the behaviour it describes
is correct. Replace that whole `test_that` block with:

```r
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
```

- [ ] **Step 5: Run the tests to verify they pass**

```bash
Rscript tests/run_tests.R
```

Expected: PASS, all files. If a series slot fails a threshold, change the slot —
never the threshold. `test-visuals.R:27` (`cols[2:9] == SERIES_SLOTS`) and
`test-visuals.R:89` (`length(SERIES_SLOTS) - 1`) must still pass untouched: the
slot count is deliberately unchanged at eight.

- [ ] **Step 6: Commit**

```bash
git add portfolio_core.R tests/testthat/test-palette.R tests/testthat/test-visuals.R
git commit -m "Re-derive the palette for a black surface, and test it"
```

---

### Task 3: Dark chrome in theme.scss, and the seam test

**Files:**
- Modify: `theme.scss:20-63` (defaults block)
- Modify: `theme.scss:220-231` (value-box fill rules)
- Test: `tests/testthat/test-palette.R` (append)

**Interfaces:**
- Consumes: the constants from Task 2.
- Produces: `$viz-accent-fill` as a new SCSS token. No R-side interface.

- [ ] **Step 1: Write the failing seam test**

Append to `tests/testthat/test-palette.R`:

```r
# theme.scss says the palette is "defined once, in portfolio_core.R". It is not:
# five values are duplicated across the two files by hand, and the failure mode
# is silent — a card surface that no longer matches the transparent-background
# plot sitting on it reads as a rendering bug, not as a stale constant.

scss_token <- function(name, path = "theme.scss") {
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
```

The test runs with the working directory at the project root, so `theme.scss`
resolves. `run_tests.R` is documented as run from the root.

- [ ] **Step 2: Run the test to verify it fails**

```bash
Rscript tests/run_tests.R
```

Expected: FAIL — `$viz-surface` in `theme.scss` is still `#fcfcfb` while
`SURFACE_COLOR` is now `#0f0f0f`; `$viz-accent-fill` does not exist.

- [ ] **Step 3: Rewrite the defaults block**

In `theme.scss`, replace the comment header and defaults (lines 3–63) with:

```scss
// ─────────────────────────────────────────────────────────────────────────────
// Portfolio dashboard theme — true black.
//
// The colour system has exactly two jobs:
//   · chrome — quiet, near-neutral surfaces and ink, so nothing competes with
//     the data;
//   · data — one validated categorical palette, shared by the charts, the
//     tables and the value boxes. It is defined in portfolio_core.R, because
//     the chips and chart lines are painted in R.
//
// Five values below are duplicated from portfolio_core.R by hand. That is a
// seam, and tests/testthat/test-palette.R asserts it: change one side and the
// suite fails rather than the dashboard quietly rendering a card surface that
// no longer matches the plot sitting on it.
//
// Series hues live in portfolio_core.R and are validated there against this
// surface. Only slot 1 crosses into CSS, as $viz-series-1, because it doubles
// as the UI accent.
// ─────────────────────────────────────────────────────────────────────────────

$font-family-sans-serif: system-ui, -apple-system, "Segoe UI", Roboto,
  "Helvetica Neue", Arial, sans-serif !default;

// Chrome
$viz-surface:   #0f0f0f; // card surface            [seam: SURFACE_COLOR]
$viz-plane:     #000000; // page plane behind the cards
$viz-ink:       #ededed; // primary ink             [seam: INK_COLOR]
$viz-ink-2:     #a6a6a6; // secondary ink
$viz-muted:     #858585; // axis labels, captions   [seam: INK_MUTED]
$viz-grid:      #232323; // hairline gridline       [seam: GRID_COLOR]
$viz-rule:      #3a3a3a; // baseline / axis
$viz-hairline:  rgba(255, 255, 255, 0.10);

// Data — categorical slot 1 doubles as the UI accent
$viz-series-1:  #4f9bf0; //                         [seam: SERIES_SLOTS[1]]

// On white one blue could be both a chart line and a filled card. On black
// those want opposite things: the line wants to be bright, the fill wants to be
// deep enough to hold white text without becoming the brightest object on the
// page. Same hue, two stops.
$viz-accent-fill: #1f5fae;

// Status (reserved: never used as a series colour)
$viz-good:      #45b874;
$viz-critical:  #e05c5c; //                         [seam: LOSS_COLOR]

$primary:       $viz-series-1;
$secondary:     $viz-ink-2;
$body-bg:       $viz-plane;
$body-color:    $viz-ink;
$link-color:    $viz-series-1;
$border-color:  $viz-hairline;

$card-bg:                 $viz-surface;
$card-border-color:       $viz-hairline;
$card-border-radius:      0.5rem;
$card-cap-bg:             $viz-surface;
$card-cap-padding-y:      0.6rem;
$card-box-shadow:         none;

// The navbar was #1a1a19, which vanishes against a black plane. At the card
// surface it reads as chrome sitting on the plane rather than as a void.
$navbar-bg:               $viz-surface;
$navbar-fg:               $viz-ink;

$table-border-color:      $viz-grid;
$table-cell-padding-y:    0.35rem;
$table-cell-padding-x:    0.6rem;

$input-bg:                #000000;
$input-color:             $viz-ink;
$input-border-color:      $viz-rule;
$input-border-radius:     0.375rem;
$input-focus-border-color: $viz-series-1;
```

`$input-bg` and `$input-color` are new: Bootstrap's defaults are a white field
with dark text, which would put six bright rectangles down the sidebar.

- [ ] **Step 4: Point the filled value box at the new token**

In `theme.scss`, the `.bslib-value-box.bg-primary` rule currently relies on
Bootstrap painting it with `$primary`. `$primary` is now the bright line colour,
so the rule must set the fill explicitly. Replace the rule at lines 226–231:

```scss
// The one filled card: growth of $10,000 is the dashboard's headline. It takes
// the deep accent stop, not $primary — $primary is the bright line colour now,
// and a card painted in it would be the brightest thing on the page.
.bslib-value-box.bg-primary {
  background-color: $viz-accent-fill !important;
  color: #ffffff !important;
  border-color: transparent;

  .value-box-showcase .bi { opacity: 0.55; }
}
```

- [ ] **Step 5: Run the tests to verify they pass**

```bash
Rscript tests/run_tests.R
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add theme.scss tests/testthat/test-palette.R
git commit -m "Invert the chrome to black, and assert the scss/R seam"
```

---

### Task 4: Compose the performance chart

**Files:**
- Modify: `portfolio_core.R` (append `chart_performance_summary()` near the other
  display helpers)
- Modify: `portfolio_dashboard.qmd:176-188` (`output$perf_chart`)

**Interfaces:**
- Consumes: `SERIES_SLOTS`, `PORTFOLIO_INK`, `LOSS_COLOR`, `INK_MUTED`,
  `GRID_COLOR` from Task 2.
- Produces:
  `chart_performance_summary(combined, port_color, bench_color) -> invisible(NULL)`
  where `combined` is the two-column xts from `compute_backtest()`, column 1 the
  portfolio and column 2 the benchmark. Draws the three stacked panels.

- [ ] **Step 1: Read the library's own layout before copying it**

```bash
Rscript -e "suppressMessages(library(PerformanceAnalytics)); print(body(charts.PerformanceSummary))"
```

Read the `default` branch of the `switch(plot.engine, ...)`. The `par()` calls
and the three chart calls in Step 3 must match what you see there. If this
release differs from what the plan shows, **follow the library, not the plan**,
and note the difference in the commit message.

- [ ] **Step 2: Write the failing test**

Append to `tests/testthat/test-visuals.R`:

```r
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
```

- [ ] **Step 3: Run the test to verify it fails**

```bash
Rscript tests/run_tests.R
```

Expected: FAIL, `could not find function "chart_performance_summary"`.

- [ ] **Step 4: Write the helper**

Append to `portfolio_core.R`:

```r
# charts.PerformanceSummary forwards a single `colorset` through ... to all
# three of its panels, so the drawdown panel cannot be coloured separately
# through it. Its body is a little par() glue around three panel functions, so
# this reproduces the glue and makes the three calls itself: all the drawing,
# axis scaling, date handling and drawdown arithmetic stay in the library, and
# only the layout becomes ours.
#
# The cost of that is a pin to the package's current stacking behaviour — if a
# future release changes its layout, this copy will not follow. test-visuals.R
# checks the one consequence we can check cheaply, that par() is left as found.
chart_performance_summary <- function(combined, port_color, bench_color) {
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)

  series <- c(port_color, bench_color)

  # The subject is drawn heavier than everything it is compared against. In the
  # drawdown panel there is no legend, so this weight is the only thing naming
  # which line is the portfolio — which is why it is the same in all three.
  weights <- c(2.6, 1.6)

  par(oma = c(2, 0, 4, 0), mar = c(1, 4, 4, 2))
  par(mar = c(1, 4, 0, 2))

  chart.CumReturns(combined, main = "", xaxis = FALSE, legend.loc = "topleft",
                   wealth.index = TRUE, ylab = "Cumulative Return",
                   colorset = series, lwd = weights, element.color = GRID_COLOR,
                   cex.axis = 0.85, cex.legend = 0.9, cex.lab = 0.85)

  chart.BarVaR(combined, main = "", xaxis = FALSE, ylab = "Periodic Return",
               methods = "none", event.labels = NULL, ylog = FALSE, add = TRUE,
               colorset = series, lwd = weights, element.color = GRID_COLOR,
               cex.axis = 0.85, cex.lab = 0.85)

  par(mar = c(5, 4, 0, 2))

  # A drawdown panel is a loss panel, so the subject's line carries the loss
  # colour. The benchmark takes muted ink rather than a second red: two reds
  # far enough apart to read as two lines could not both clear 3:1 on this
  # surface, and red-against-grey separates on lightness as well as hue.
  chart.Drawdown(combined, main = "", ylab = "Drawdown",
                 event.labels = NULL, ylog = FALSE, add = TRUE,
                 colorset = c(LOSS_COLOR, INK_MUTED), lwd = weights,
                 element.color = GRID_COLOR,
                 cex.axis = 0.85, cex.lab = 0.85)

  invisible(NULL)
}
```

- [ ] **Step 5: Run the test to verify it passes**

```bash
Rscript tests/run_tests.R
```

Expected: PASS.

- [ ] **Step 6: Call it from the dashboard**

In `portfolio_dashboard.qmd`, replace the body of `output$perf_chart` (keeping
the `alt` function exactly as it is):

```r
output$perf_chart <- renderPlot({
  bt <- backtest()
  cols <- bt$plan$colors
  chart_performance_summary(bt$combined,
                            port_color  = cols[["Portfolio"]],
                            bench_color = cols[[bt$p$bench]])
}, bg = "transparent", alt = function() {
  bt <- backtest()
  perf_chart_alt(bt$port, bt$bench, bt$p$bench)
})
```

Delete the comment above it that says the plots draw no title of their own only
because the card header names them — it is still true, and now lives in
`chart_performance_summary()` instead.

- [ ] **Step 7: Render and compare against the previous chart**

```bash
quarto render portfolio_dashboard.qmd
```

Open the rendered dashboard and confirm: three stacked panels in the same
proportions as before, the portfolio line clearly heavier than the benchmark,
the drawdown panel red, axis labels legible. **If the panels do not stack
correctly, stop.** The spec's fallback is to revert to `charts.PerformanceSummary`
with a single `colorset` and no red drawdown panel; take it, and say so.

- [ ] **Step 8: Commit**

```bash
git add portfolio_core.R portfolio_dashboard.qmd tests/testthat/test-visuals.R
git commit -m "Compose the performance chart so the drawdown panel reads as loss"
```

---

### Task 5: Verify the whole dashboard

**Files:**
- Modify: `images/dashboard.png`
- Modify: `README.md` (only if Step 3 finds a claim that no longer holds)

**Interfaces:**
- Consumes: everything above. Produces nothing for later tasks.

- [ ] **Step 1: Full test run**

```bash
Rscript tests/run_tests.R
```

Expected: PASS, every file, no skips.

- [ ] **Step 2: Render and inspect every card**

```bash
quarto render portfolio_dashboard.qmd
```

Check each against the design spec, and record what you actually saw:

- sidebar inputs are dark fields with light text, not white boxes
- the one filled value box is the deep accent; the other five are card surface
- the monthly-returns calendar reads as a quiet grid, not a bright block —
  this is the item most likely to still be too loud, since the ramp change is
  a judgement call rather than a threshold
- both charts sit on the card with no white rectangle behind them
  (`bg = "transparent"` relies on the card surface matching `SURFACE_COLOR`,
  which is exactly what the seam test protects)
- the run-problems banner is legible: trigger it by entering three symbols and
  two weights, then clicking Run Backtest
- the entity chips in Key Statistics match the chart line colours

- [ ] **Step 3: Check the README against what now exists**

`README.md` describes the colour system only structurally at lines 55–56, which
still holds. Line 29 says "the seven largest positions when there are more" —
that follows from `length(SERIES_SLOTS) - 1`, and the slot count is unchanged at
eight, so it also still holds. Confirm both by reading them; change nothing if
they are accurate.

- [ ] **Step 4: Regenerate the screenshot**

The script drives Chrome over the DevTools Protocol against a *running* server,
so this is two terminals. First:

```bash
quarto serve portfolio_dashboard.qmd --port 4455
```

Then, in a second terminal:

```bash
node tools/capture-screenshot.mjs http://127.0.0.1:4455/ images/dashboard.png
```

It needs Node 18+ and a Chrome it can find; set `CHROME_PATH` if it cannot.
The viewport is fixed at 1600x1150 at scale factor 2 inside the script — do not
change it, because the committed image would jump size between commits.

Confirm `images/dashboard.png` is the dark dashboard, at the same dimensions as
the file it replaced, before committing it.

- [ ] **Step 5: Commit**

```bash
git add images/dashboard.png README.md
git commit -m "Regenerate the dashboard screenshot for the dark theme"
```

---

## Notes for the implementer

**The two charts get their colours from different places.** The Per-Holding
chart needs no code change at all: its `colorset` comes from `bt$plan$colors`
(so, from `SERIES_SLOTS`) and its `element.color` from `GRID_COLOR`. Both move
in Task 2. If it still looks light after Task 2, something is wrong with the
constants, not with the chart.

**`bg = "transparent"` is load-bearing.** Both `renderPlot` calls pass it, which
is why the plots inherit the card surface. This is also why the seam between
`$viz-surface` and `SURFACE_COLOR` matters enough to test: the R side uses
`SURFACE_COLOR` to compute calendar tints, and the CSS side paints the card the
plot sits on. If they drift, the calendar's "neutral" cell stops matching the
card behind it.

**Do not add a light theme back.** The spec rejects a toggle deliberately: the
charts render server-side and cannot see a CSS media query, so a toggle needs
the active theme pushed back into R. That is out of scope here.
