###############################################################################
# tests/testthat/test-rebalancing.R
#
# Tests for optimize.portfolio.rebalancing(), trailingFUN(), and the
# associated generic/extract methods for rebalancing result objects.
#
# Source files covered (primary):
#   R/optimize.portfolio.R  — optimize.portfolio.rebalancing()
#   R/trailingFUN.R         — trailingFUN()
#   R/extractstats.R        — extractWeights.optimize.portfolio.rebalancing,
#                             extractObjectiveMeasures.optimize.portfolio.rebalancing,
#                             extractStats.optimize.portfolio.rebalancing,
#                             extractWeights.opt.rebal.list,
#                             extractObjectiveMeasures.opt.rebal.list,
#                             extractStats.opt.rebal.list
#   R/generics.R            — print.optimize.portfolio.rebalancing,
#                             summary.optimize.portfolio.rebalancing,
#                             print.summary.optimize.portfolio.rebalancing,
#                             print.opt.rebal.list
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

library(ROI)
library(ROI.plugin.quadprog)

# ---------------------------------------------------------------------------
# Shared setup — built once at file scope for speed.
# Use edhec[, 1:5] with quarterly rebalancing and a 36-month training window
# so the deterministic ROI solver keeps wall time acceptable.
# ---------------------------------------------------------------------------

utils::data(edhec)
R   <- edhec[, 1:5]
nms <- colnames(R)

# Long-only, full-investment, minimize-StdDev portfolio
portf <- portfolio.spec(assets = nms)
portf <- add.constraint(portf, type = "full_investment")
portf <- add.constraint(portf, type = "long_only")
portf <- add.objective(portf, type = "risk", name = "StdDev")

# Run the rebalancing optimization once and reuse across all tests
opt.rebal <- optimize.portfolio.rebalancing(
  R               = R,
  portfolio       = portf,
  optimize_method = "ROI",
  rebalance_on    = "quarters",
  training_period = 36,
  rolling_window  = 36
)

# Manually build an opt.rebal.list from two identical rebalancing results.
# Using structure() directly (same as what optimize.portfolio.rebalancing
# produces internally when passed a portfolio.list) avoids running an extra
# expensive optimization just to construct the list class.
opt.rebal.list.obj <- structure(
  list(opt.rebal, opt.rebal),
  class = "opt.rebal.list"
)

# ---------------------------------------------------------------------------
# trailingFUN — basic functionality
# ---------------------------------------------------------------------------

test_that("trailingFUN returns numeric result for vector input with FUNargs=list()", {
  x      <- 1:100
  result <- suppressWarnings(
    trailingFUN(x, weights = NULL, n = 12, FUN = "mean", FUNargs = list())
  )
  expected <- mean(x[(length(x) - 12L):length(x)])
  expect_true(is.numeric(result))
  expect_equal(result, expected)
})

test_that("trailingFUN errors when FUN is NULL", {
  expect_error(
    trailingFUN(1:10, weights = NULL, n = 5, FUN = NULL),
    "you must supply a function"
  )
})

test_that("trailingFUN errors when FUN is not a function name", {
  expect_error(
    trailingFUN(1:10, weights = NULL, n = 5, FUN = "not_a_real_function_xyz"),
    regexp = NULL   # any error is sufficient
  )
})

test_that("trailingFUN slices an xts matrix to n+1 trailing rows (scalar FUN)", {
  # Make a small xts matrix
  dates  <- seq(as.Date("2010-01-31"), by = "month", length.out = 20)
  mat    <- xts::xts(matrix(rnorm(20 * 3), nrow = 20), order.by = dates)
  n_trail <- 5L

  # Use nrow (returns a scalar integer) so the is.na() guard inside
  # trailingFUN does not receive a length->1 vector and error out.
  result <- suppressWarnings(
    trailingFUN(mat, weights = NULL, n = n_trail, FUN = "nrow",
                FUNargs = list())
  )
  # nrow of the sliced matrix == n_trail + 1
  expect_true(is.numeric(result) || is.integer(result))
  expect_equal(as.integer(result), n_trail + 1L)
})

# ---------------------------------------------------------------------------
# optimize.portfolio.rebalancing — class and basic structure
# ---------------------------------------------------------------------------

test_that("optimize.portfolio.rebalancing returns correct S3 class", {
  expect_s3_class(opt.rebal, "optimize.portfolio.rebalancing")
})

test_that("opt.rebal result contains required list components", {
  expect_true(!is.null(opt.rebal$opt_rebal),    label = "opt_rebal component present")
  expect_true(!is.null(opt.rebal$R),            label = "R component present")
  expect_true(!is.null(opt.rebal$portfolio),    label = "portfolio component present")
  expect_true(!is.null(opt.rebal$call),         label = "call component present")
  expect_true(!is.null(opt.rebal$elapsed_time), label = "elapsed_time component present")
  expect_true(is.list(opt.rebal$opt_rebal),     label = "opt_rebal is a list")
  expect_true(length(opt.rebal$opt_rebal) > 0,  label = "opt_rebal is non-empty")
})

test_that("each element of opt_rebal inherits from optimize.portfolio", {
  first_el <- opt.rebal$opt_rebal[[1]]
  expect_true(inherits(first_el, "optimize.portfolio"),
              label = "first opt_rebal element is optimize.portfolio")
})

# ---------------------------------------------------------------------------
# extractWeights — rebalancing result
# ---------------------------------------------------------------------------

test_that("extractWeights returns an xts matrix with correct dimensions", {
  w <- extractWeights(opt.rebal)
  expect_true(xts::is.xts(w) || is.matrix(w),
              label = "extractWeights result is xts or matrix")
  expect_true(nrow(w) > 0,    label = "weight matrix has rows")
  expect_equal(ncol(w), length(nms),
               label = "number of weight columns equals number of assets")
  expect_equal(colnames(w), nms,
               label = "weight column names match asset names")
})

test_that("rebalancing produces a positive number of periods", {
  w <- extractWeights(opt.rebal)
  expect_true(nrow(w) > 0)
})

# ---------------------------------------------------------------------------
# Constraint validation: long-only, full-investment
# ---------------------------------------------------------------------------

test_that("all weights are non-negative across every rebalancing period (long-only)", {
  w <- extractWeights(opt.rebal)
  expect_true(all(w >= -1e-6),
              label = "no weight is meaningfully negative")
})

test_that("weights sum to 1 in every rebalancing period (full-investment)", {
  w        <- extractWeights(opt.rebal)
  row_sums <- rowSums(w)
  expect_true(all(abs(row_sums - 1) < 1e-4),
              label = "all row sums are within 1e-4 of 1")
})

# Spot-check first, middle and last rebalancing dates
test_that("first, middle and last rebalancing period weights are valid", {
  w <- extractWeights(opt.rebal)
  nr <- nrow(w)
  mid <- ceiling(nr / 2)
  for (i in c(1L, mid, nr)) {
    wi <- as.numeric(w[i, ])
    expect_true(all(wi >= -1e-6),    label = paste("row", i, "non-negative"))
    expect_true(abs(sum(wi) - 1) < 1e-4, label = paste("row", i, "sums to 1"))
  }
})

# ---------------------------------------------------------------------------
# extractObjectiveMeasures — rebalancing result
# ---------------------------------------------------------------------------

test_that("extractObjectiveMeasures returns an xts/matrix with correct row count", {
  obj <- extractObjectiveMeasures(opt.rebal)
  expect_true(xts::is.xts(obj) || is.matrix(obj),
              label = "extractObjectiveMeasures result is xts or matrix")
  expect_true(nrow(obj) > 0, label = "objective measures matrix has rows")

  # Number of rows must match the number of rebalancing dates
  w <- extractWeights(opt.rebal)
  expect_equal(nrow(obj), nrow(w),
               label = "one objective-measures row per rebalancing date")
})

test_that("objective measure values are finite numbers", {
  obj <- extractObjectiveMeasures(opt.rebal)
  expect_true(all(is.finite(as.numeric(obj))),
              label = "all objective measure values are finite")
})

# ---------------------------------------------------------------------------
# extractStats — rebalancing result returns a list
# ---------------------------------------------------------------------------

test_that("extractStats.optimize.portfolio.rebalancing returns a list", {
  stats <- extractStats(opt.rebal)
  expect_true(is.list(stats),          label = "extractStats result is a list")
  expect_true(length(stats) > 0,       label = "stats list is non-empty")
})

test_that("each extractStats element is a named numeric vector (one per rebalancing date)", {
  stats <- extractStats(opt.rebal)
  # extractStats.optimize.portfolio.ROI returns a named numeric vector
  # (weights + objective measures + out), not a matrix
  expect_true(is.numeric(stats[[1]]),         label = "first stats element is numeric")
  expect_false(is.null(names(stats[[1]])),    label = "stats element has names")
  # length of stats list matches number of rebalancing dates
  w <- extractWeights(opt.rebal)
  expect_equal(length(stats), nrow(w),        label = "stats list length matches rebalancing dates")
})

# ---------------------------------------------------------------------------
# print.optimize.portfolio.rebalancing
# ---------------------------------------------------------------------------

test_that("print.optimize.portfolio.rebalancing produces output without error", {
  expect_output(print(opt.rebal), regexp = "PortfolioAnalytics")
})

test_that("print output contains rebalancing date information", {
  expect_output(print(opt.rebal), regexp = "rebalancing dates")
})

# ---------------------------------------------------------------------------
# summary.optimize.portfolio.rebalancing
# ---------------------------------------------------------------------------

test_that("summary returns a summary.optimize.portfolio.rebalancing object", {
  s <- summary(opt.rebal)
  expect_s3_class(s, "summary.optimize.portfolio.rebalancing")
  expect_true(is.list(s))
})

test_that("summary result contains all expected components", {
  s <- summary(opt.rebal)
  expect_true(!is.null(s$weights),            label = "weights present in summary")
  expect_true(!is.null(s$objective_measures), label = "objective_measures present")
  expect_true(!is.null(s$rebalance_dates),    label = "rebalance_dates present")
  expect_true(!is.null(s$annualized_returns), label = "annualized_returns present")
  expect_true(!is.null(s$annualized_StdDev),  label = "annualized_StdDev present")
  expect_true(!is.null(s$downside_risk),      label = "downside_risk present")
  expect_true(!is.null(s$portfolio_returns),  label = "portfolio_returns present")
  expect_true(!is.null(s$call),               label = "call present in summary")
  expect_true(!is.null(s$elapsed_time),       label = "elapsed_time present")
})

test_that("summary rebalance_dates equals index of summary weights", {
  s <- summary(opt.rebal)
  expect_equal(s$rebalance_dates, zoo::index(s$weights))
})

test_that("summary weights dimensions match extractWeights output", {
  s <- summary(opt.rebal)
  w <- extractWeights(opt.rebal)
  expect_equal(dim(s$weights), dim(w))
})

test_that("summary annualized_returns is a numeric scalar", {
  s <- summary(opt.rebal)
  expect_true(is.numeric(as.numeric(s$annualized_returns)))
  expect_equal(length(as.numeric(s$annualized_returns)), 1L)
})

# ---------------------------------------------------------------------------
# print.summary.optimize.portfolio.rebalancing
# ---------------------------------------------------------------------------

test_that("print.summary.optimize.portfolio.rebalancing produces output", {
  s <- summary(opt.rebal)
  expect_output(print(s), regexp = "PortfolioAnalytics")
})

test_that("print.summary output contains annualized return and std dev", {
  s <- summary(opt.rebal)
  out <- capture.output(print(s))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("Annualized", combined, ignore.case = TRUE))
})

# ---------------------------------------------------------------------------
# extractWeights on summary object (extractWeights.summary.*)
# ---------------------------------------------------------------------------

test_that("extractWeights on summary object returns the weights xts", {
  s <- summary(opt.rebal)
  w <- extractWeights(s)
  expect_true(xts::is.xts(w) || is.matrix(w))
  expect_equal(ncol(w), length(nms))
})

# ---------------------------------------------------------------------------
# opt.rebal.list — extractWeights, extractObjectiveMeasures, extractStats,
# and print dispatch methods
# ---------------------------------------------------------------------------

test_that("extractWeights.opt.rebal.list returns a list of xts matrices", {
  wlist <- extractWeights(opt.rebal.list.obj)
  expect_true(is.list(wlist),                       label = "result is a list")
  expect_equal(length(wlist), 2L,                   label = "list length equals 2")
  expect_true(xts::is.xts(wlist[[1]]) || is.matrix(wlist[[1]]),
              label = "each element is xts or matrix")
  expect_equal(ncol(wlist[[1]]), length(nms),
              label = "correct number of asset columns")
})

test_that("extractObjectiveMeasures.opt.rebal.list returns a list of xts/matrices", {
  obj_list <- extractObjectiveMeasures(opt.rebal.list.obj)
  expect_true(is.list(obj_list),                         label = "result is a list")
  expect_equal(length(obj_list), 2L,                     label = "list length equals 2")
  expect_true(xts::is.xts(obj_list[[1]]) || is.matrix(obj_list[[1]]),
              label = "each element is xts or matrix")
})

test_that("extractStats.opt.rebal.list returns a list of per-portfolio stat lists", {
  stats_list <- extractStats(opt.rebal.list.obj)
  expect_true(is.list(stats_list),            label = "result is a list")
  expect_equal(length(stats_list), 2L,        label = "list length equals 2")
  # Each element is itself a list (one entry per rebalancing period)
  expect_true(is.list(stats_list[[1]]),       label = "each element is a list")
  expect_true(length(stats_list[[1]]) > 0,   label = "inner list is non-empty")
})

test_that("print.opt.rebal.list produces output without error", {
  expect_output(print(opt.rebal.list.obj))
})

test_that("print.opt.rebal.list labels each optimization", {
  out <- capture.output(print(opt.rebal.list.obj))
  combined <- paste(out, collapse = "\n")
  expect_true(grepl("Optimization 1", combined))
  expect_true(grepl("Optimization 2", combined))
})
