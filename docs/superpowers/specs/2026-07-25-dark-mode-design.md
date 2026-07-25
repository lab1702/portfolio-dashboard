# Dark mode for the portfolio dashboard

**Date:** 2026-07-25
**Status:** approved, not yet implemented

## Problem

The dashboard is clean but bland, and most of its pixels are bright. Dark mode
addresses the brightness directly. The blandness is a separate fault with two
causes worth fixing at the same time: every surface sits at the same visual
elevation, and the accent colour appears exactly once in the whole interface.

## Decisions

Dark **replaces** light. There is no toggle. The two charts are base-R graphics
rendered server-side, so a CSS-level theme switch cannot reach them without
pushing the active theme back into R and re-rendering — real plumbing, and a
class of bug (charts stuck in the wrong palette) that a single theme does not
have. A toggle also forces the palette to be validated twice, against two
surfaces.

Scope is the colour system plus elevation and accent. Layout, type scale and
density stay as they are: those decisions each carry a recorded reason, several
of them fixes for specific defects, and reopening them risks re-breaking what
they fixed.

## What changes

| file | change |
|---|---|
| `theme.scss` | chrome tokens and rules inverted to the true-black surface |
| `portfolio_core.R` | colour constants re-derived; new `chart_performance_summary()` |
| `portfolio_dashboard.qmd` | `perf_chart` calls the new helper; `fund_chart` gains the new palette |
| `tests/testthat/helper-color.R` | new — colour maths, test-only |
| `tests/testthat/test-palette.R` | new — palette, tint-ramp, contrast and seam assertions |
| `images/dashboard.png` | regenerated via `tools/capture-screenshot.mjs` |
| `README.md` | whatever it states about the theme |

## The surface

True black: page plane at `#000000`, cards at `#0f0f0f`. This is the fewest
bright pixels of the options considered and the most contrast on the data. The
cost accepted knowingly: there is nothing below `#000` for a card to recede
into, so elevation only runs upward, and pure black beside near-white is the
harshest pairing available. The ink choice below mitigates the second.

## Chrome tokens

| token | light (current) | dark |
|---|---|---|
| `$viz-plane` | `#f9f9f7` | `#000000` |
| `$viz-surface` | `#fcfcfb` | `#0f0f0f` |
| `$viz-ink` | `#0b0b0b` | `#ededed` |
| `$viz-ink-2` | `#52514e` | `#a6a6a6` |
| `$viz-muted` | `#898781` | `#858585` |
| `$viz-grid` | `#e1e0d9` | `#232323` |
| `$viz-rule` | `#c3c2b7` | `#3a3a3a` |
| `$viz-hairline` | `rgba(11,11,11,.10)` | `rgba(255,255,255,.10)` |
| `$viz-good` | `#006300` | `#45b874` |
| `$viz-critical` | `#d03b3b` | `#e05c5c` |

`$navbar-bg` was `#1a1a19`, which disappears against a black plane. It becomes
`#0f0f0f` — the card surface — so the navbar reads as chrome sitting on the
plane rather than as a void.

### Ink is `#ededed`, not white

Pure white on pure black is the harshest pairing available, and this dashboard
is meant to be sat in front of. `#ededed` on `#0f0f0f` is 16.4:1, well past any
threshold that matters.

Actual `#ffffff` is then reserved for one job: the portfolio's own line and its
headline figure. White becomes a rank rather than a default. This is the
existing `PORTFOLIO_INK` idea — the subject wears the extreme of the ink scale,
every other series steps down — carried across the inversion.

### Muted gets brighter than a faithful inversion

`#898781` on `#fcfcfb` is 3.3:1, under AA for the 0.72rem captions and axis
labels it is used on. Inverting it faithfully would carry that shortfall
across. `#858585` on `#0f0f0f` is 5.2:1.

### The accent splits into two stops

On white, one mid-blue served as both chart line and filled-card background. On
black those want opposite things: the line wants to be bright, the fill wants
to be deep enough to hold white text without becoming the brightest object on
screen.

- `$viz-series-1: #4f9bf0` — chart lines, chips, links, focus rings (6.6:1 on surface)
- `$viz-accent-fill: #1f5fae` — the one filled value box (6.4:1 for white text)

Same hue, two stops. `$primary` maps to `$viz-series-1`; the `.bslib-value-box.bg-primary`
rule takes `$viz-accent-fill`.

## Elevation and accent

The stated problem has two halves and the tokens above only answer one. The
second half, concretely:

**Elevation.** Today the card surface and the page plane differ by roughly one
percent of luminance, with no shadow and a 10%-opacity hairline — cards are
implied rather than seen. `#0f0f0f` on `#000000` is a real step, and the
hairline at `rgba(255,255,255,.10)` reads as a lit edge on black rather than as
a faint smudge on white. `$card-box-shadow` stays `none`: on black a shadow has
nothing to darken, and the surface step is doing the work.

**Accent.** Today `$viz-series-1` appears in exactly one place — the single
filled value box — because on white every additional use looked like noise. On
black the accent has room to work without shouting:

- the benchmark line in both charts, at `$viz-series-1`
- the entity chips in the statistics table, unchanged in role but now legible
  against a surface they contrast with
- links and `$input-focus-border-color`, already mapped through `$primary`
- the filled value box, at `$viz-accent-fill`

No new component gets colour it did not have. The accent simply stops being a
single decorative instance and becomes the thing that marks the benchmark
wherever the benchmark appears.

## Series palette

The eight categorical slots are **re-derived, not inverted**. `#008300` and
`#4a3aa7` are near-invisible on black; they were chosen against white.

Derivation constraints, with the validator as the gate:

1. Each slot ≥ 3:1 against `#0f0f0f` (WCAG 1.4.11, graphical objects).
2. Adjacent-pair ΔE2000 ≥ 15 at normal vision.
3. Adjacent-pair ΔE2000 ≥ 8 under simulated deuteranopia, protanopia and tritanopia.
4. Slot 1 is fixed at `#4f9bf0`, since it doubles as the UI accent.

Thresholds 2 and 3 are the ones the current comments already claim. The bar is
unchanged; only the surface moves. Ordering remains the colourblind-safety
mechanism, so the constraint is on *adjacent* pairs.

**Derived, and confirmed feasible.** Eight hues were selected and then ordered
by exhaustive search over all 5040 permutations of slots 2–8 (slot 1 pinned),
maximising the CVD floor:

```r
SERIES_SLOTS <- c("#4f9bf0", "#ffc53d", "#a98cff", "#ff8a4d",
                  "#35d39a", "#ff92be", "#7ee36b", "#ff6b6b")
```

Worst adjacent pair: ΔE2000 45.6 at normal vision, 9.92 under simulated
dichromacy — both above the thresholds, and both better than the light
theme's 19.6 / 9.1. Lowest contrast against the surface is slot 1 at 6.6:1,
comfortably past the 3:1 floor. The ordering is load-bearing: the same eight
hues in their originally authored order fail, with amber against pink at
ΔE 4.5 under tritanopia.

`PORTFOLIO_INK` becomes `#ffffff`. The overflow colour past slot 8 becomes
`INK_MUTED` (`#858585`), preserving today's behaviour: an unlabelled series is
admitted as unlabelled rather than given a recycled hue.

### The tint ramp is re-tuned for restraint, not for legibility

An earlier draft of this spec claimed the ramp would break legibility when
inverted. Measured, it does not: at the existing maximum weight of 0.60 the
deepest cells are `#356396` and `#8c3d3d`, which hold 5.3:1 and 6.3:1 against
`$viz-ink`. The existing 0.12–0.60 bounds would pass every contrast assertion
unchanged.

The ramp still moves, for a different reason. On black the tint runs *upward*
in luminance, so a 12-column grid of monthly returns at full weight becomes the
brightest region on the page — which is the complaint this whole change exists
to answer. Bounds become **0.10–0.45**, keeping the calendar quiet. Assertion
group 2 below stays in the suite regardless, since it is the constraint that
stops a future widening of the ramp from going unnoticed.

## The charts

`combined` is exactly two columns, `Portfolio` and the benchmark.

`charts.PerformanceSummary` forwards a single `colorset` through `...` to all
three of its panels, so per-panel colour is unreachable through it. Its body is
roughly fifteen lines of `par()` glue around three calls to `chart.CumReturns`,
`chart.BarVaR` and `chart.Drawdown` with `add=TRUE`. A new
`chart_performance_summary()` in `portfolio_core.R` reproduces that glue and
makes the three calls itself. All drawing, axis scaling, date handling and
drawdown arithmetic stay in the library; only the layout glue becomes ours.

| panel | function | colours | `lwd` |
|---|---|---|---|
| Cumulative Return | `chart.CumReturns` | Portfolio `#ffffff`, benchmark `$viz-series-1` | `c(2.6, 1.6)` |
| Periodic Return | `chart.BarVaR` | same pair | same |
| Drawdown | `chart.Drawdown` | same pair | same |

`lwd` is per-series: `chart.TimeSeries.builtin` does
`if (length(lwd) < columns) lwd <- rep(lwd, columns)`.

**All three panels use the same entity colours.** An earlier version of this
design painted the drawdown panel red-on-grey, on the reasoning that a drawdown
panel is a loss panel and the subject should wear the loss colour. That was
wrong, and a reader hit it immediately: the same two series changed colour
partway down a single chart.

It fails for three reasons. Colour means entity identity everywhere else in this
dashboard — the chips in Key Statistics exist to key the tables to the charts —
so one panel using colour to mean something else forces two schemes on the
reader at once. That panel gets no legend of its own, so repainting it left line
weight as the only surviving identifier, which is far too quiet for the job.
And the hue was redundant anyway: every value in a drawdown panel is ≤ 0 by
definition and the panel is titled "Drawdown", so a categorical channel was
being spent on what the axis and title already said.

Line weight still separates subject from reference in all three panels. It is
now reinforcement rather than the sole mechanism.

Tinting the panel — a background wash or a fill under the curve — would have
kept the loss reading without touching entity identity. `chart.Drawdown` has no
fill option, so that needs the custom plotting this design rejected.

Gridlines take `$viz-grid`; on black the current weight competes with the data.

The Per-Holding chart stays a plain `chart.CumReturns` call, with the new
palette and `element.color`. Nothing structural.

Sign-split return bars were considered and rejected: `chart.BarVaR` colours by
column, not by sign, so they would require writing real plotting code.

## The seam between the two colour sources

`theme.scss` claims the palette is "defined once, in portfolio_core.R". That is
not true today — six values are duplicated by hand across the two files:

| value | `theme.scss` | `portfolio_core.R` |
|---|---|---|
| accent | `$viz-series-1` | `SERIES_SLOTS[1]`, `GAIN_COLOR` |
| card surface | `$viz-surface` | `SURFACE_COLOR` |
| body ink | `$viz-ink` | `INK_COLOR` |
| gridline | `$viz-grid` | `GRID_COLOR` |
| muted ink | `$viz-muted` | `INK_MUTED` |
| loss | `$viz-critical` | `LOSS_COLOR` |

The duplication is stable only because nothing has touched the colours since
they were set. This change touches all six, and the failure mode is silent: a
card surface that no longer matches the `bg="transparent"` plot sitting on it,
or a gridline a shade off the CSS one, both read as rendering bugs rather than
stale constants.

No build step is introduced to generate one file from the other. Instead the
seam is asserted in the test suite, turning a hand-sync convention into
something that fails loudly.

## The validator

`tests/testthat/helper-color.R` holds the colour maths: sRGB→linear→XYZ→Lab,
ΔE2000, WCAG contrast ratio, and Viénot 1999 LMS matrices for the three
dichromacies. Base R and `grDevices`; no new dependency. Test-only — the
application has no runtime need for any of it.

`tests/testthat/test-palette.R` asserts four groups:

1. **Series separation** — each slot ≥ 3:1 against the card surface; adjacent
   pairs ≥ 15 ΔE2000 at normal vision and ≥ 8 under each of the three
   simulated dichromacies.
2. **Tint ramp** — `return_tint()` at maximum weight, both poles, ≥ 4.5:1
   against `$viz-ink`.
3. **Text contrast** — ink, ink-2 and muted each ≥ 4.5:1 on the card surface;
   white on `$viz-accent-fill` ≥ 4.5:1; `$viz-series-1` ≥ 3:1 on surface.
4. **The seam** — the five shared values parsed out of `theme.scss` equal their
   `portfolio_core.R` counterparts.

The ΔE2000 formula is named in the test, so the numbers are reproducible from
the repository. They will not match the `9.1` currently asserted in a comment
in `portfolio_core.R`: different formula, different surface. That comment is
replaced by the test.

## Verification

1. `tests/run_tests.R` green, including the new file.
2. Render the dashboard and compare the composed performance chart against the
   current one, confirming the panel layout reproduces.
3. Regenerate `images/dashboard.png` with `tools/capture-screenshot.mjs`.

`README.md` needs no wording change for the theme itself — it describes the
colour system only structurally, at lines 55–56. The one sentence at risk is
line 29, "the seven largest positions when there are more", which is a
consequence of the slot count and only changes if the palette does.

## Risks

**The composed layout may not reproduce.** `add=TRUE` stacking outside
`charts.PerformanceSummary` is the least certain part. Verified by rendering
before the change is committed. Fallback: keep `charts.PerformanceSummary`,
accept a single `colorset` across all three panels, and lose the red drawdown
panel. Everything else in this design is unaffected.

**~~Eight hues may not fit the constraints on black.~~ Retired during
planning.** Eight hues were derived and validated before the plan was written;
see the palette section. The slot count is unchanged at eight, so
`top_holdings()`, the Per-Holding caption and `README.md` line 29 all stand as
they are.

**A pinned dependency on package internals.** Reproducing the `par()` glue ties
the performance chart to `charts.PerformanceSummary`'s current stacking
behaviour. A future PerformanceAnalytics release could change its layout without
this copy following. Accepted: the alternative is either no per-panel colour or
writing the plotting outright.
