# portfolio_core.R — pure logic for the portfolio dashboard, kept out of
# the Shiny layer so it can be unit-tested (run: Rscript tests/run_tests.R).
# Requires xts and PerformanceAnalytics to be loaded by the caller.

START_CAPITAL <- 10000

parse_symbols <- function(text) {
  syms <- toupper(trimws(strsplit(text, ",")[[1]]))
  syms[nzchar(syms)]
}

parse_weights <- function(text) {
  wtxt <- trimws(strsplit(text, ",")[[1]])
  suppressWarnings(as.numeric(wtxt[nzchar(wtxt)]))
}

# Returns a character vector of validation messages, empty when inputs are
# valid. Mirrors shiny::need semantics: any non-TRUE condition fails.
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
    if (fail(!is.na(years) && years >= 1)) "Years of history must be at least 1."
  )
}

normalize_weights <- function(wts) wts / sum(wts)

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

build_drawdown_table <- function(port, scale) {
  dd <- table.Drawdowns(port, top = 5)
  fmt_date <- function(d) ifelse(is.na(d), "ongoing",
                                 format(as.Date(d), "%b %d, %Y"))
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
