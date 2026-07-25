# portfolio_core.R — pure logic for the portfolio dashboard, kept out of
# the Shiny layer so it can be unit-tested (run: Rscript tests/run_tests.R).
# Requires xts and PerformanceAnalytics to be loaded by the caller.

# The "$10,000 grew to" value box states this figure in its own title, which is
# a Quarto chunk option and so cannot interpolate R. Changing this constant
# means editing that title in portfolio_dashboard.qmd by hand.
START_CAPITAL <- 10000

# ── Visual system ────────────────────────────────────────────────────────────
# One entity, one colour, everywhere it appears: charts, legends and the stats
# table all read from entity_colors(), so a colour follows the entity and never
# its rank — changing the symbol list never repaints the survivors.
#
# The portfolio is the subject of the dashboard rather than one instrument among
# many, so it wears primary ink at the heaviest weight instead of a hue. On a
# black surface that extreme is white — the same rule as the light theme, the
# other way up. White is a rank here, not a default: body ink is INK_COLOR, one
# step down, so that the portfolio's line and figure are the only pure white on
# the page.

PORTFOLIO_INK <- "#ffffff"

# Validated categorical order for the #0f0f0f card surface: worst adjacent pair
# is dE2000 45.6 at normal vision and 9.92 under simulated dichromacy. The
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

# There is deliberately no separate colour for the zero line, and these charts
# no longer pass `element.color` at all.
#
# The reasoning that wanted one was sound; the mechanism was not.
# chart.TimeSeries.builtin does `addSeries(xts(rep(0, rows), ...), col =
# element.color, on = 1)`, which reads like a zero rule you can style. It draws
# nothing here. Set element.color to magenta and render: the canvas has zero
# magenta pixels, while the identical probe on grid.color paints 13,731 of them.
# Panel 1 is a wealth index around 1.0-2.5, so a line at y = 0 is off-scale and
# clipped, and the panels added onto it never get one.
#
# The zero level in the drawdown panel is therefore drawn by whatever gridline
# happens to land there, at GRID_COLOR, and distinguishing it needs plotting
# code this design has twice declined to write. `element.color` came out rather
# than staying in looking like the knob that controls it — the same reason
# `ylab` came out of these calls.

# An instrument past the last slot takes this rather than INK_MUTED. The two
# were the same value until labels.col started painting axis text INK_MUTED —
# at which point a ninth holding's line became exactly the colour of the tick
# labels behind it. An unlabelled series should read as unlabelled, not as
# chrome.
OVERFLOW_COLOR <- "#9a9a9a"

# Instruments past the eighth slot fall back to muted grey rather than a cycled
# hue: a recycled categorical colour is indistinguishable from the entity it
# repeats, which is worse than admitting the series is unlabelled.
#
# Colours are keyed to an instrument's position in the symbol list, so the same
# inputs always produce the same chart. That is as stable as eight slots allow:
# editing the symbol list re-runs the whole backtest, and with more instruments
# than slots no assignment can both stay fixed per symbol and stay distinct.
# There is deliberately no filter control that would drop a series from a drawn
# chart, which is the case where repainting the survivors would mislead.
#
# `drawn` names the instruments that will actually be plotted, and they take
# slots first. Without that, selection (by weight, in top_holdings) and
# assignment (by list position) were free to disagree: twenty holdings whose
# largest positions sat late in the list drew seven lines that were all the
# same overflow grey. Hue priority is the only thing rank decides here — an
# instrument's colour still comes from the inputs, never from its own rank.
entity_colors <- function(bench_sym, syms, drawn = syms) {
  instruments <- unique(c(bench_sym, drawn, syms))
  slots <- c(SERIES_SLOTS,
             rep(OVERFLOW_COLOR,
                 max(0, length(instruments) - length(SERIES_SLOTS))))
  setNames(c(PORTFOLIO_INK, slots[seq_along(instruments)]),
           c("Portfolio", instruments))
}

# The per-holding chart can only draw as many holdings as there are distinct
# slots left once the benchmark has taken slot 1. Past that it shows the largest
# positions — in input order, so colours stay put — and the caller says so
# rather than silently dropping the tail.
top_holdings <- function(syms, wts, n = length(SERIES_SLOTS) - 1L) {
  if (length(syms) <= n) return(syms)
  syms[sort(order(wts, decreasing = TRUE)[seq_len(n)])]
}

# Which series get drawn and what colour each entity wears are one decision, so
# they are resolved together and once. Every output — both charts, the legend
# and the stats table — reads this same plan; asking the two functions
# separately is what let the chart and the palette disagree.
display_plan <- function(bench_sym, syms, wts) {
  shown <- top_holdings(syms, wts)
  list(shown = shown, colors = entity_colors(bench_sym, syms, drawn = shown))
}

# The row label names an entity chosen by the user, so it is escaped before it
# reaches a table rendered with sanitize.text.function = identity. Nothing
# hostile survives the Yahoo download today, which is exactly the kind of
# guarantee that quietly stops being true.
entity_chip <- function(label, color)
  sprintf('<span class="entity-chip" style="background:%s"></span>%s',
          htmltools::htmlEscape(color, attribute = TRUE),
          htmltools::htmlEscape(label))

# Robust full-tint magnitude for the monthly-returns grid. A single outlier
# month (a leveraged fund's +40%) would otherwise flatten every other cell to
# near-neutral, so the scale tops out at the 90th percentile of |return|.
tint_cap <- function(values, floor_pct = 2) {
  v <- abs(values[is.finite(values)])
  if (!length(v)) return(floor_pct)
  max(floor_pct, as.numeric(stats::quantile(v, 0.9)))
}

# Diverging cell tint, neutral at zero. Returns a background pale enough to keep
# primary ink readable on top — the printed sign, not the tint, is what carries
# the polarity; the colour only reinforces it.
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

# Signed comparison against the benchmark. The arrow duplicates the sign so the
# outcome is never encoded by colour alone.
#
# A non-finite delta is not a loss. isTRUE(NA >= 0) is FALSE, so the unguarded
# version answered "▼ NA pts" — a confident-looking claim that the portfolio
# trailed, built out of a missing number.
delta_vs_benchmark <- function(port, bench) {
  d <- port - bench
  if (!isTRUE(is.finite(d)))
    return(list(class = "delta-muted", text = "n/a"))
  ahead <- d >= 0
  list(class = if (ahead) "delta-good" else "delta-bad",
       text  = sprintf("%s%.1f pts", if (ahead) "▲ " else "▼ ",
                       abs(100 * d)))
}

parse_symbols <- function(text) {
  syms <- toupper(trimws(strsplit(text, ",")[[1]]))
  syms[nzchar(syms)]
}

parse_weights <- function(text) {
  wtxt <- trimws(strsplit(text, ",")[[1]])
  suppressWarnings(as.numeric(wtxt[nzchar(wtxt)]))
}

MAX_YEARS <- 30

# The symbol list is the only input that reaches the network, and it was the
# only one with no ceiling: each symbol costs one sequential Yahoo download on
# Shiny's single thread, so a pasted list of several hundred froze the whole
# dashboard for minutes with nothing on screen to explain it. Twenty-five is
# well past any portfolio this tool is meant for and still bounds the wait.
MAX_SYMBOLS <- 25

# Returns a character vector of validation messages, empty when inputs are
# valid. Mirrors shiny::need semantics: any non-TRUE condition fails.
#
# The upper bound on `years` is enforced here as well as on the input control:
# numericInput's max is a browser hint, and a typed or scripted value sails
# past it. A missing year reports the lower bound only — two complaints about
# one empty box reads as two separate problems.
portfolio_input_errors <- function(syms, wts, bench, years) {
  fail <- function(cond) !isTRUE(cond)
  c(
    if (fail(length(syms) >= 1)) "Enter at least one symbol.",
    if (fail(length(syms) <= MAX_SYMBOLS))
      sprintf("Enter at most %d symbols.", MAX_SYMBOLS),
    if (fail(!anyDuplicated(syms))) "Remove duplicate symbols.",
    if (fail(length(syms) == length(wts))) "Number of symbols and weights must match.",
    if (fail(all(is.finite(wts)))) "Weights must be numeric.",
    if (fail(all(wts >= 0))) "Weights cannot be negative.",
    if (fail(sum(wts) > 0)) "At least one weight must be positive.",
    if (fail(nzchar(bench))) "Enter a benchmark symbol.",
    if (fail(!grepl("[,[:space:]]", bench))) "Enter a single benchmark symbol.",
    if (fail(!is.na(years) && years >= 1)) "Years of history must be at least 1.",
    if (fail(is.na(years) || years <= MAX_YEARS))
      sprintf("Years of history cannot exceed %d.", MAX_YEARS)
  )
}

normalize_weights <- function(wts) wts / sum(wts)

# Every expected failure reports once, in the sidebar, beneath the button that
# triggered it. The dashboard used to raise these through validate(), which
# propagates to every output that touched the reactive: one bad form painted
# the same text into all eleven cards, and the value boxes — fixed height, two
# lines of room — clipped a three-message error mid-word and dropped the third
# message entirely. Returns NULL for a clean run so renderUI emits nothing.
run_problem_banner <- function(msgs) {
  if (!length(msgs)) return(NULL)
  htmltools::tags$div(
    class = "run-problems", role = "alert",
    htmltools::tags$p(class = "run-problems-title", "Cannot run this backtest"),
    htmltools::tags$ul(lapply(unname(msgs), htmltools::tags$li)))
}

# Month names are pinned rather than taken from %b, which renders in the
# session locale: an R process started under a German or French locale served
# "Mrz 05, 2021" and "Mrz" column headers inside otherwise English chrome. The
# numeric parts of format() are locale-free, so only the label needs replacing.
MONTH_ABB <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

month_abb_of <- function(d) MONTH_ABB[as.integer(format(as.Date(d), "%m"))]

fmt_month_year <- function(d) {
  out <- sprintf("%s %s", month_abb_of(d), format(as.Date(d), "%Y"))
  out[is.na(as.Date(d))] <- NA_character_
  out
}

fmt_month_day_year <- function(d) {
  out <- sprintf("%s %s, %s", month_abb_of(d),
                 format(as.Date(d), "%d"), format(as.Date(d), "%Y"))
  out[is.na(as.Date(d))] <- NA_character_
  out
}

# The control's options and the caption's wording are one vocabulary, so they
# live together: the sidebar builds its selectInput from REBAL_CHOICES and the
# caption reads REBAL_LABELS, and a test holds the two sets equal. Keeping the
# choice list in the .qmd let a new option arrive with no label, which read as
# "rebalanced NA" under the chart while every test still passed.
REBAL_CHOICES <- c(Monthly = "months", Quarterly = "quarters",
                   Yearly = "years", "Buy & Hold" = "none")

# Each label is a complete phrase rather than a word the caption prefixes with
# "rebalanced". The prefix version read "rebalanced buy & hold, never
# rebalanced" for the one option that isn't a frequency, because "none" is the
# only label that has to describe the absence of the thing being prefixed.
# Whole phrases have no such special case.
REBAL_LABELS <- c(months = "rebalanced monthly", quarters = "rebalanced quarterly",
                  years = "rebalanced yearly", none = "buy & hold, never rebalanced")

rebal_label <- function(rebal) unname(REBAL_LABELS[rebal])

# The assembled sentence lives here, not in the .qmd, so a test can read the
# whole caption. The prefix bug above survived a green suite precisely because
# the sentence was built inside renderUI where no test could reach it — only
# the individual labels were covered.
perf_caption_text <- function(wts, syms, rebal, bench_sym)
  sprintf("%s · %s · benchmark %s",
          paste0(round(wts * 100, 1), "% ", syms, collapse = " / "),
          rebal_label(rebal), bench_sym)

fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)

fmt_dollar <- function(x)
  sprintf("$%s", format(round(x), big.mark = ",", scientific = FALSE))

grew_to <- function(r, capital = START_CAPITAL) capital * prod(1 + as.numeric(r))

# A Sharpe ratio divides return by volatility, so a series with no volatility —
# a halted ticker, a stable-NAV fund, a window in which the price never moved —
# divides by almost nothing and lands on a *finite* number in the trillions.
# That is an absent denominator, not a spectacular portfolio: printed verbatim
# it graded a frozen series "great" and put an eighteen-character cell in the
# stats table, pushing the last column out of the card. Past this bound the
# ratio has stopped measuring anything, so it is reported as unavailable rather
# than rounded to two decimals.
SHARPE_MAX <- 100

sharpe_measurable <- function(s) is.finite(s) & abs(s) <= SHARPE_MAX

# The printed number and the printed grade read the same predicate, so a card
# can never show a figure its own caption calls unavailable.
fmt_sharpe <- function(s) ifelse(sharpe_measurable(s), sprintf("%.2f", s), "n/a")

sharpe_verdict <- function(s) {
  if (!isTRUE(sharpe_measurable(s))) return("n/a")
  as.character(cut(s, c(-Inf, 0, 1, 2, Inf),
                   labels = c("poor", "fair", "good", "great"),
                   right = FALSE))
}

# Sharpe is computed with Rf = 0 — PerformanceAnalytics' default, used by both
# the value box and the stats table, so the two always agree. The card grades
# the number in plain English for people who have not met a Sharpe ratio
# before, and a grade needs its baseline stated: with cash paying anything at
# all, a zero-Rf figure is return per unit of risk, not *excess* return per
# unit of risk, and it flatters every portfolio it touches.
RISK_FREE_RATE <- 0

# The value boxes and the stats table are two routes to the same three numbers.
# The boxes call these; the table goes through table.AnnualizedReturns and a
# matrix-coerced apply(). Nothing forced the routes to agree — the guarantee
# lived in the comment above and nowhere else. Computing the boxes' figures
# here puts both routes inside the test suite's reach, and a test now holds
# each box equal to its own cell in the table.
ann_return <- function(r, scale)
  as.numeric(Return.annualized(r, scale = scale)[1])

ann_sharpe <- function(r, scale)
  as.numeric(SharpeRatio.annualized(r, scale = scale, Rf = RISK_FREE_RATE,
                                    geometric = TRUE)[1])

ann_maxdd <- function(r) as.numeric(maxDrawdown(r))

sharpe_caption <- function(s) {
  if (!isTRUE(sharpe_measurable(s)))
    return("Return per unit of risk — not measurable here: volatility over this window is effectively zero.")
  sprintf("Return per unit of risk — %s (1+ good, 2+ great), against a %s risk-free rate.",
          sharpe_verdict(s), fmt_pct(RISK_FREE_RATE))
}

# renderPlot's alt text defaults to the static string "Plot object", which left
# the two charts — the dashboard's main content — as the one place where meaning
# arrived visually and nowhere else, in a layout that otherwise pairs every
# colour with a printed sign and every table header with a scope. These build
# the description out of the same backtest the chart is drawn from, so the words
# offered to a screen reader cannot end up describing a different run than the
# picture beside them.
perf_chart_alt <- function(port, bench, bench_sym)
  sprintf(paste("Three stacked panels — cumulative growth, daily returns and",
                "drawdown — for the portfolio against %s, %s to %s.",
                "%s in the portfolio grew to %s; in %s, to %s."),
          bench_sym, fmt_month_year(start(port)), fmt_month_year(end(port)),
          fmt_dollar(START_CAPITAL), fmt_dollar(grew_to(port)),
          bench_sym, fmt_dollar(grew_to(bench)))

# Describes the series actually drawn, not every column available: the chart
# shows `shown`, so the alt text has to say the same or it is describing lines
# that are not there.
fund_chart_alt <- function(returns, shown) {
  ends <- vapply(shown, function(s) grew_to(returns[, s], capital = 1),
                 numeric(1))
  sprintf("Growth of $1 in each holding on its own, %s to %s: %s.",
          fmt_month_year(start(returns)), fmt_month_year(end(returns)),
          paste(sprintf("%s ends at $%.2f", shown, ends), collapse = "; "))
}

# The chip column carries no visible label — the chip is the entire content —
# but a blank <th> announces to a screen reader as an unnamed column. The name
# lives in text only assistive tech reaches. renderTable puts column names
# through sanitize.text.function, which is identity here, so the span survives.
CHIP_COLUMN_HEADER <- '<span class="visually-hidden">Series</span>'

# Everything downstream of the price download. `prices` is a merged, na.omit'd
# xts of adjusted prices whose columns are named by symbol; `wts` is already
# normalized to fractions.
compute_backtest <- function(prices, syms, wts, bench_sym, rebal) {
  returns <- na.omit(Return.calculate(prices))

  # effective annualization scale (observations per calendar year) keeps
  # CAGR calendar-true for any trading frequency (weekday, Sun-Thu, 7-day);
  # span from prices, not returns: growth compounds from the day-0 price
  span_days <- as.numeric(difftime(end(prices), start(prices), units = "days"))
  scale <- 365.25 * nrow(returns) / span_days

  rebalance_on <- if (rebal == "none") NA else rebal
  port <- Return.portfolio(returns[, syms, drop = FALSE],
                           weights = wts, rebalance_on = rebalance_on)
  colnames(port) <- "Portfolio"

  bench <- returns[, bench_sym, drop = FALSE]
  combined <- merge(port, bench)
  colnames(combined) <- c("Portfolio", bench_sym)

  list(returns = returns, scale = scale,
       port = port, bench = bench, combined = combined)
}

build_stats_table <- function(port, returns, bench_sym, syms, scale) {
  cols <- merge(port, returns[, unique(c(bench_sym, syms)), drop = FALSE])
  colnames(cols) <- c("Portfolio", unique(c(bench_sym, syms)))
  ann <- table.AnnualizedReturns(cols, scale = scale, digits = 6)
  data.frame(
    "Return (ann.)"  = fmt_pct(as.numeric(ann[1, ])),
    "Std Dev (ann.)" = fmt_pct(as.numeric(ann[2, ])),
    "Sharpe"         = fmt_sharpe(as.numeric(ann[3, ])),
    "Max Drawdown"   = fmt_pct(-apply(cols, 2, maxDrawdown)),
    row.names = colnames(cols),
    check.names = FALSE
  )
}

# charts.PerformanceSummary forwards a single `colorset` through ... to all
# three of its panels, so the drawdown panel cannot be coloured separately
# through it. Its body is a little par() glue around three panel functions, so
# this reproduces the glue and makes the three calls itself: all the drawing,
# axis scaling, date handling and drawdown arithmetic stay in the library, and
# only the layout becomes ours.
#
# The library (PerformanceAnalytics 2.1.0, default plot.engine) sets all three
# par(mar = ...) values *before* drawing any of the three panels, not one per
# panel as an earlier draft of this plan assumed — body(charts.PerformanceSummary)
# is the source of truth, and each mar assignment simply overwrites the last, so
# the net effect is oma = c(2, 0, 4, 0), mar = c(5, 4, 0, 2) for all three calls.
# Reproduced verbatim here rather than collapsed to that one call, so this stays
# a visible match against the library body if a future release is diffed against
# it.
#
# The cost of that is a pin to the package's current stacking behaviour — if a
# future release changes its layout, this copy will not follow. test-visuals.R
# checks the one consequence we can check cheaply, that par() is left as found.
#
# Each chart.* call below returns a "replot_xts" chob rather than drawing
# eagerly — like lattice, it only draws when printed, and a bare top-level
# statement auto-prints while a statement inside a function body does not. The
# three calls are add = TRUE-chained onto one chob, so printing only the last
# one (from chart.Drawdown) replays all three panels; this was found by pixel
# census, not by the test above, because a null device does not care whether
# anything was actually drawn on it. The library's own body does the same
# print() for the same reason, right before it restores par().
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
  par(mar = c(5, 4, 0, 2))

  # bg, labels.col and grid.color are not decoration. plot.xts paints its own
  # plot region, defaulting to white, and renderPlot's bg = "transparent" does
  # not reach it — so without these the chart is a white rectangle on a black
  # card. The plot is opaque either way; it is merely painted to match.
  #
  # Each panel keeps its own main: chart.CumReturns/BarVaR/Drawdown draw it as
  # an in-plot title, not the outer one charts.PerformanceSummary sets with
  # title(main, outer = TRUE) — this function never calls that, so there is no
  # second, card-header-duplicating title above the three panels, only the
  # names a reader needs to tell "Cumulative Return" from "Drawdown" apart
  # without a legend (the middle panel has none). ylab is not part of that:
  # plot.xts renders no y-axis title regardless of what is passed, so the
  # three panels below pass none rather than an argument that reads as a label
  # but does nothing.
  #
  # main.timespan = FALSE is still suppressed, for a narrower reason: left at
  # its default, plot.xts draws its own "start / end" header in the top-right
  # corner of this panel — a second, ISO-formatted copy of the window the
  # Period value box already states in fmt_month_year's words.
  chob <- chart.CumReturns(combined, main = "Cumulative Return", xaxis = FALSE,
                           legend.loc = "topleft", main.timespan = FALSE,
                           wealth.index = TRUE,
                           colorset = series, lwd = weights,
                           bg = SURFACE_COLOR,
                           labels.col = INK_MUTED, grid.color = GRID_COLOR,
                           cex.axis = 0.85, cex.legend = 0.9, cex.lab = 0.85)

  # chart.BarVaR's own default main is paste(date.label, "Return"), where
  # date.label comes from periodicity(x)$scale. Hardcoded here rather than
  # derived: `combined` is always daily. compute_backtest() never resamples
  # (no to.weekly/to.monthly), and get_prices() calls quantmod::getSymbols()
  # with no periodicity argument, which returns Yahoo's daily series. If either
  # of those changes, this label has to start reading periodicity(combined)
  # the way the library does.
  chob <- chart.BarVaR(combined, main = "Daily Return", xaxis = FALSE,
                       methods = "none",
                       event.labels = NULL, ylog = FALSE, add = TRUE,
                       colorset = series, lwd = weights,
                       bg = SURFACE_COLOR,
                       labels.col = INK_MUTED, grid.color = GRID_COLOR,
                       cex.axis = 0.85, cex.lab = 0.85)

  # Entity colours here too, not the loss palette. This panel used to be painted
  # red-on-grey to say "loss", which cost more than it bought: every value in a
  # drawdown panel is <= 0 by definition and the title already reads "Drawdown",
  # so the hue carried nothing the axis did not — while breaking the rule the
  # rest of the dashboard keeps, that a colour names an entity and only that.
  # The chips in Key Statistics key off that rule. Repainting the same two
  # series mid-chart left line weight as the only thing still naming them, on
  # the one panel that gets no legend, and readers landed on exactly that.
  chob <- chart.Drawdown(combined, main = "Drawdown",
                         event.labels = NULL, ylog = FALSE, add = TRUE,
                         colorset = series, lwd = weights,
                         bg = SURFACE_COLOR,
                         labels.col = INK_MUTED, grid.color = GRID_COLOR,
                         cex.axis = 0.85, cex.lab = 0.85)

  # The three add = TRUE calls chain onto one chob, so printing the last one
  # replays all three panels — see the note above the function.
  print(chob)

  invisible(NULL)
}

# `top = 5` is a ceiling, not a quota: table.Drawdowns warns when the series
# has fewer, and hands back a placeholder row (Depth 0, To = NA) when it has
# none at all. Rendered verbatim that placeholder claimed an ongoing 0.0%
# drawdown, so only genuine drawdowns survive here — an empty table is the
# honest answer for a series that never fell.
build_drawdown_table <- function(port, scale) {
  dd <- suppressWarnings(table.Drawdowns(port, top = 5))
  dd <- dd[is.finite(dd$Depth) & dd$Depth < 0, , drop = FALSE]
  # ifelse() returns the type of its *test*, so on an empty table this handed
  # back logical(0) and the date columns came out logi. Zero rows hid it; the
  # columns are built as character either way now.
  fmt_date <- function(d) {
    out <- fmt_month_day_year(d)
    out[is.na(d)] <- "ongoing"
    as.character(out)
  }
  df <- data.frame(
    From   = fmt_date(dd$From),
    Trough = fmt_date(dd$Trough),
    To     = fmt_date(dd$To),
    Depth  = fmt_pct(dd$Depth),
    Days   = as.integer(dd$Length),
    check.names = FALSE
  )
  names(df)[5] <- if (scale > 300) "Days" else "Trading Days"
  df
}
