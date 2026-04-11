###############################################################################
# tests/testthat/test-optimize-cvxr.R
#
# Tests for optimize.portfolio() via the CVXR solver path and extract_risk().
#
# Source files covered:
#   - R/optimize.portfolio.R  (lines ~2816-3156: CVXR solver dispatch)
#   - R/extractrisk.R         (extract_risk())
#   - R/generics.R            (print.optimize.portfolio.CVXR)
#   - R/extractstats.R        (extractStats.optimize.portfolio.CVXR)
#
# Key facts:
#   - optimize_method = "CVXR" triggers the CVXR path; result class is
#     "optimize.portfolio.CVXR" (also inherits "optimize.portfolio").
#   - For min-var, CVXR uses OSQP automatically; for ES/mean it uses CLARABEL.
#   - CVXR::psolve() returns value(problem) — a plain numeric scalar.
#     Therefore extract_risk()$ES, $CSM, $EQS are plain numeric scalars,
#     NOT lists; access them directly (not via $value).
#   - extract_risk()$StdDev is a 1×1 matrix; coerce with as.numeric() as needed.
#
# Shared fixtures from helper-portfolioanalytics.R (do NOT redefine):
#   edhec5, funds5, make_minvar_portf(), make_mines_portf(), make_maxret_portf(),
#   TOL_WSUM, expect_valid_weights(), expect_valid_opt_result()
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("CVXR")

library(CVXR)

# ---------------------------------------------------------------------------
# File-scope setup — runs once before any test_that() block.
# Suppress solver output that CVXR/CLARABEL/OSQP may emit during tests.
# ---------------------------------------------------------------------------

R <- edhec5   # 5-asset xts from helper-portfolioanalytics.R

# --- Min-variance with CVXR (auto-selects OSQP) ---
portf_var <- make_minvar_portf(R)
opt_var   <- suppressMessages(suppressWarnings(
  optimize.portfolio(R, portf_var, optimize_method = "CVXR")
))

# --- Min-ES with CVXR (auto-selects CLARABEL) ---
portf_es <- make_mines_portf(R)
opt_es   <- suppressMessages(suppressWarnings(
  optimize.portfolio(R, portf_es, optimize_method = "CVXR")
))

# --- Max-return with CVXR (auto-selects CLARABEL) ---
portf_ret <- make_maxret_portf(R)
opt_ret   <- suppressMessages(suppressWarnings(
  optimize.portfolio(R, portf_ret, optimize_method = "CVXR")
))

# --- Equal weights for extract_risk() tests ---
w_eq <- rep(1 / ncol(R), ncol(R))
names(w_eq) <- funds5

# --- Extreme weight vector: all weight on the highest-variance asset.
#     Concentrating fully in the most volatile asset should yield ES >= the
#     equal-weight portfolio ES (diversification reduces tail risk).
.var_each <- apply(as.matrix(R), 2, var)
w_extreme <- rep(0, ncol(R))
w_extreme[which.max(.var_each)] <- 1
names(w_extreme) <- funds5

# --- extract_risk() results (CVXR is used internally for ES / CSM / EQS) ---
er_eq      <- suppressMessages(suppressWarnings(
  extract_risk(as.matrix(R), w_eq)
))
er_extreme <- suppressMessages(suppressWarnings(
  extract_risk(as.matrix(R), w_extreme)
))


# ===========================================================================
# Min Variance (CVXR)
# ===========================================================================

test_that("minvar CVXR: result has class optimize.portfolio.CVXR", {
  expect_s3_class(opt_var, "optimize.portfolio.CVXR")
})

test_that("minvar CVXR: extractWeights returns a named numeric vector of length N", {
  w <- extractWeights(opt_var)
  expect_true(is.numeric(w))
  expect_equal(length(w), ncol(R))
  expect_false(is.null(names(w)))
})

test_that("minvar CVXR: weights are non-negative (long-only constraint satisfied)", {
  w <- extractWeights(opt_var)
  expect_true(all(w >= -TOL_WSUM))
})

test_that("minvar CVXR: weights sum to approximately 1 (full-investment satisfied)", {
  expect_equal(sum(extractWeights(opt_var)), 1, tolerance = TOL_WSUM)
})

test_that("minvar CVXR: no NA weights", {
  expect_false(any(is.na(extractWeights(opt_var))))
})


# ===========================================================================
# Min ES (CVXR)
# ===========================================================================

test_that("minES CVXR: result has class optimize.portfolio.CVXR", {
  expect_s3_class(opt_es, "optimize.portfolio.CVXR")
})

test_that("minES CVXR: extractWeights returns a named numeric vector", {
  w <- extractWeights(opt_es)
  expect_true(is.numeric(w))
  expect_false(is.null(names(w)))
})

test_that("minES CVXR: weights sum to approximately 1", {
  expect_equal(sum(extractWeights(opt_es)), 1, tolerance = TOL_WSUM)
})


# ===========================================================================
# Max Return (CVXR)
# ===========================================================================

test_that("maxret CVXR: result has class optimize.portfolio.CVXR", {
  expect_s3_class(opt_ret, "optimize.portfolio.CVXR")
})

test_that("maxret CVXR: weights are valid (numeric, no NA, long-only, full-investment)", {
  expect_valid_opt_result(opt_ret, "optimize.portfolio.CVXR")
})


# ===========================================================================
# CVXR vs ROI comparison — min-variance should give near-identical weights
# ===========================================================================

test_that("minvar CVXR and ROI give similar weights (max abs difference < 1e-3)", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  # Run the same portfolio spec through ROI (quadprog) for comparison.
  opt_var_roi <- optimize.portfolio(R, portf_var, optimize_method = "ROI")
  w_cvxr <- extractWeights(opt_var)
  w_roi  <- extractWeights(opt_var_roi)
  expect_true(max(abs(w_cvxr - w_roi)) < 1e-3)
})


# ===========================================================================
# print.optimize.portfolio.CVXR
# ===========================================================================

test_that("print.optimize.portfolio.CVXR outputs 'PortfolioAnalytics'", {
  expect_output(print(opt_var), "PortfolioAnalytics")
})


# ===========================================================================
# extract_risk()
# Note: CVXR::psolve() returns value(problem) — a plain numeric scalar.
#       $ES, $CSM, and $EQS in the result are therefore plain numeric scalars,
#       not lists.  Access them directly: er$ES, not er$ES$value.
# ===========================================================================

test_that("extract_risk returns a list with elements mean, StdDev, ES, CSM, EQS", {
  expect_true(is.list(er_eq))
  expect_true(all(c("mean", "StdDev", "ES", "CSM", "EQS") %in% names(er_eq)))
})

test_that("extract_risk: mean is a numeric scalar", {
  expect_true(is.numeric(er_eq$mean))
  expect_equal(length(as.numeric(er_eq$mean)), 1L)
})

test_that("extract_risk: StdDev is a positive numeric value", {
  # StdDev is returned as a 1x1 matrix; as.numeric() extracts the scalar.
  expect_true(is.numeric(er_eq$StdDev))
  expect_true(as.numeric(er_eq$StdDev) > 0)
})

test_that("extract_risk: ES is a positive numeric value (loss-side risk measure)", {
  # psolve() returns the optimal CVaR value directly as a numeric scalar.
  expect_true(is.numeric(er_eq$ES))
  expect_true(as.numeric(er_eq$ES) > 0)
})

test_that("extract_risk: extreme weight vector yields ES >= equal-weight ES", {
  # Concentrating all weight in the most volatile asset removes diversification
  # benefit; its ES should be at least as large as the equal-weight portfolio ES.
  expect_true(as.numeric(er_extreme$ES) >= as.numeric(er_eq$ES) - 1e-6)
})


# ===========================================================================
# extractStats()
# ===========================================================================

test_that("extractStats returns a named numeric vector", {
  stats <- extractStats(opt_var)
  expect_true(is.numeric(stats))
  expect_false(is.null(names(stats)))
})

test_that("extractStats contains a weight column for each asset", {
  stats <- extractStats(opt_var)
  # extractStats.optimize.portfolio.CVXR names weight columns "w.<asset_name>"
  expected_wt_names <- paste("w", funds5, sep = ".")
  expect_true(all(expected_wt_names %in% names(stats)))
})
