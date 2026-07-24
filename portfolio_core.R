# portfolio_core.R — pure logic for the portfolio dashboard, kept out of
# the Shiny layer so it can be unit-tested (run: Rscript tests/run_tests.R).
# Requires xts and PerformanceAnalytics to be loaded by the caller.

START_CAPITAL <- 10000

# ── Visual system ────────────────────────────────────────────────────────────
# One entity, one colour, everywhere it appears: charts, legends and the stats
# table all read from entity_colors(), so a colour follows the entity and never
# its rank — changing the symbol list never repaints the survivors.
#
# The portfolio is the subject of the dashboard rather than one instrument among
# many, so it wears primary ink at the heaviest weight instead of a hue. The
# instruments take the categorical slots in order.

PORTFOLIO_INK <- "#0b0b0b"

# Validated categorical order: worst adjacent pair is CVD dE 9.1 and
# normal-vision dE 19.6 on the #fcfcfb card surface. The *ordering* is the
# colourblind-safety mechanism, not decoration — re-validate before reordering
# or extending it.
SERIES_SLOTS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                  "#e87ba4", "#008300", "#4a3aa7", "#e34948")

INK_MUTED     <- "#898781"  # axis labels, captions
GRID_COLOR    <- "#e1e0d9"  # hairline gridlines
SURFACE_COLOR <- "#fcfcfb"  # card surface the plots sit on
GAIN_COLOR    <- "#2a78d6"  # diverging pole: gains
LOSS_COLOR    <- "#d03b3b"  # diverging pole: losses

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
             rep(INK_MUTED, max(0, length(instruments) - length(SERIES_SLOTS))))
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
  weight <- 0.12 + 0.48 * min(abs(x) / cap, 1)
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

REBAL_LABELS <- c(months = "monthly", quarters = "quarterly",
                  years = "yearly", none = "buy & hold, never rebalanced")

rebal_label <- function(rebal) unname(REBAL_LABELS[rebal])

fmt_pct <- function(x) sprintf("%.1f%%", 100 * x)

fmt_dollar <- function(x)
  sprintf("$%s", format(round(x), big.mark = ",", scientific = FALSE))

grew_to <- function(r, capital = START_CAPITAL) capital * prod(1 + as.numeric(r))

sharpe_verdict <- function(s) {
  if (!is.finite(s)) return("n/a")
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

sharpe_caption <- function(s)
  sprintf("Return per unit of risk — %s (1+ good, 2+ great), against a %s risk-free rate.",
          sharpe_verdict(s), fmt_pct(RISK_FREE_RATE))

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
    "Sharpe"         = sprintf("%.2f", as.numeric(ann[3, ])),
    "Max Drawdown"   = fmt_pct(-apply(cols, 2, maxDrawdown)),
    row.names = colnames(cols),
    check.names = FALSE
  )
}

# `top = 5` is a ceiling, not a quota: table.Drawdowns warns when the series
# has fewer, and hands back a placeholder row (Depth 0, To = NA) when it has
# none at all. Rendered verbatim that placeholder claimed an ongoing 0.0%
# drawdown, so only genuine drawdowns survive here — an empty table is the
# honest answer for a series that never fell.
build_drawdown_table <- function(port, scale) {
  dd <- suppressWarnings(table.Drawdowns(port, top = 5))
  dd <- dd[is.finite(dd$Depth) & dd$Depth < 0, , drop = FALSE]
  fmt_date <- function(d) ifelse(is.na(d), "ongoing", fmt_month_day_year(d))
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
