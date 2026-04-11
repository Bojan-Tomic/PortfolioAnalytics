###############################################################################
# tests/testthat/test-turnover.R
#
# Tests for the turnover constraint in optimize.portfolio() via the CVXR solver.
#
# Source files covered:
#   - R/optimize.portfolio.R  (CVXR turnover constraint path, lines ~3041-3064)
#   - R/constraints.R         (turnover_constraint(), add.constraint())
#
# Key facts:
#   - Turnover constraint: add.constraint(..., type="turnover",
#       turnover_target=<value>, weight_initial=<w0>)
#   - The constraint class is "turnover_constraint".
#   - When weight_initial is omitted it defaults internally to rep(1/N, N)
#     (equal weights) inside the CVXR solver path.
#   - Turnover is defined as sum(abs(w - w0)); the CVXR solver enforces
#     sum(abs(wts - w0)) <= turnover_target.
#   - Only optimize_method = "CVXR" supports the turnover constraint.
#   - CVXR::psolve() returns value(problem) — a plain numeric scalar.
#
# Shared fixtures from helper-portfolioanalytics.R (do NOT redefine):
#   edhec5, funds5, make_minvar_portf(), TOL_WSUM,
#   expect_valid_weights(), expect_valid_opt_result()
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
# Suppress solver output that CVXR / CLARABEL / OSQP may emit during tests.
# ---------------------------------------------------------------------------

R <- edhec5   # 5-asset xts from helper-portfolioanalytics.R

# Equal initial weights (used as weight_initial for portf_to10 and as the
# implicit default for portf_to50 when weight_initial is not supplied).
w0_eq <- rep(1 / 5, 5)
names(w0_eq) <- funds5

# ---------------------------------------------------------------------------
# Portfolio: loose turnover constraint (50%).
# weight_initial not supplied → defaults to rep(1/N, N) = w0_eq internally.
# ---------------------------------------------------------------------------
portf_to50 <- portfolio.spec(assets = funds5)
portf_to50 <- add.constraint(portf_to50, type = "full_investment")
portf_to50 <- add.constraint(portf_to50, type = "long_only")
portf_to50 <- add.constraint(portf_to50, type = "turnover",
                              turnover_target = 0.5)
portf_to50 <- add.objective(portf_to50, type = "risk", name = "var")

opt_to50 <- suppressMessages(suppressWarnings(
  optimize.portfolio(R, portf_to50, optimize_method = "CVXR")
))

# ---------------------------------------------------------------------------
# Portfolio: tight turnover constraint (10%) with explicit weight_initial.
# ---------------------------------------------------------------------------
portf_to10 <- portfolio.spec(assets = funds5)
portf_to10 <- add.constraint(portf_to10, type = "full_investment")
portf_to10 <- add.constraint(portf_to10, type = "long_only")
portf_to10 <- add.constraint(portf_to10, type = "turnover",
                              turnover_target = 0.1, weight_initial = w0_eq)
portf_to10 <- add.objective(portf_to10, type = "risk", name = "var")

opt_to10 <- suppressMessages(suppressWarnings(
  optimize.portfolio(R, portf_to10, optimize_method = "CVXR")
))

# ---------------------------------------------------------------------------
# Portfolio: ES objective with turnover constraint (30%).
# ---------------------------------------------------------------------------
portf_es_to <- portfolio.spec(assets = funds5)
portf_es_to <- add.constraint(portf_es_to, type = "full_investment")
portf_es_to <- add.constraint(portf_es_to, type = "long_only")
portf_es_to <- add.constraint(portf_es_to, type = "turnover",
                               turnover_target = 0.3)
portf_es_to <- add.objective(portf_es_to, type = "risk", name = "ES")

opt_es_to <- suppressMessages(suppressWarnings(
  optimize.portfolio(R, portf_es_to, optimize_method = "CVXR")
))

# ---------------------------------------------------------------------------
# Unconstrained min-variance reference (CVXR, no turnover).
# Used in the "tight constraint stays closer to w0" test.
# ---------------------------------------------------------------------------
opt_var_free <- suppressMessages(suppressWarnings(
  optimize.portfolio(R, make_minvar_portf(R), optimize_method = "CVXR")
))


# ===========================================================================
# Turnover constraint specification
# ===========================================================================

test_that("turnover constraint is present in portf_to50", {
  expect_true(
    any(sapply(portf_to50$constraints,
               function(x) inherits(x, "turnover_constraint")))
  )
})

test_that("turnover_target values are stored correctly in both portfolio specs", {
  to_loose <- Filter(function(x) inherits(x, "turnover_constraint"),
                     portf_to50$constraints)[[1]]
  to_tight <- Filter(function(x) inherits(x, "turnover_constraint"),
                     portf_to10$constraints)[[1]]
  expect_equal(to_loose$turnover_target, 0.5)
  expect_equal(to_tight$turnover_target, 0.1)
})


# ===========================================================================
# Optimization result — loose turnover (opt_to50)
# ===========================================================================

test_that("opt_to50: result has class optimize.portfolio.CVXR", {
  expect_s3_class(opt_to50, "optimize.portfolio.CVXR")
})

test_that("opt_to50: extractWeights returns a named numeric vector", {
  w <- extractWeights(opt_to50)
  expect_true(is.numeric(w))
  expect_false(is.null(names(w)))
})

test_that("opt_to50: weights are non-negative (long-only constraint satisfied)", {
  w <- extractWeights(opt_to50)
  expect_true(all(w >= -TOL_WSUM))
})

test_that("opt_to50: weights sum to approximately 1 (full-investment satisfied)", {
  expect_equal(sum(extractWeights(opt_to50)), 1, tolerance = TOL_WSUM)
})

test_that("opt_to50: no NA weights", {
  expect_false(any(is.na(extractWeights(opt_to50))))
})


# ===========================================================================
# Optimization result — tight turnover (opt_to10)
# ===========================================================================

test_that("opt_to10: result has class optimize.portfolio.CVXR", {
  expect_s3_class(opt_to10, "optimize.portfolio.CVXR")
})

test_that("opt_to10: extractWeights returns a named numeric vector", {
  w <- extractWeights(opt_to10)
  expect_true(is.numeric(w))
  expect_false(is.null(names(w)))
})

test_that("opt_to10: weights are non-negative (long-only constraint satisfied)", {
  w <- extractWeights(opt_to10)
  expect_true(all(w >= -TOL_WSUM))
})

test_that("opt_to10: weights sum to approximately 1 (full-investment satisfied)", {
  expect_equal(sum(extractWeights(opt_to10)), 1, tolerance = TOL_WSUM)
})


# ===========================================================================
# Turnover constraint satisfaction
# ===========================================================================

test_that("opt_to50 actual turnover does not exceed target 0.5 (+ small tolerance)", {
  w_to50 <- extractWeights(opt_to50)
  # Default weight_initial = rep(1/N, N) when not supplied
  w0     <- rep(1 / 5, 5)
  actual_to <- sum(abs(w_to50 - w0))
  expect_true(actual_to <= 0.5 + 0.01)
})

test_that("opt_to10 actual turnover does not exceed target 0.1 (+ small tolerance)", {
  w_to10    <- extractWeights(opt_to10)
  actual_to <- sum(abs(w_to10 - w0_eq))
  expect_true(actual_to <= 0.1 + 0.01)
})


# ===========================================================================
# Tighter constraint produces weights closer to initial weights
# ===========================================================================

test_that("tight turnover (10%) yields weights closer to w0_eq than unconstrained min-var", {
  # opt_var_free is the CVXR min-variance solution with NO turnover constraint.
  # The tight-turnover result must stay within 0.1 L1-distance of w0_eq, so
  # it should be no further from w0_eq than the unconstrained solution
  # (plus a small numerical tolerance).
  w_free  <- extractWeights(opt_var_free)
  w_tight <- extractWeights(opt_to10)
  to_free  <- sum(abs(w_free  - w0_eq))
  to_tight <- sum(abs(w_tight - w0_eq))
  expect_true(to_tight <= to_free + 0.02)
})


# ===========================================================================
# ES objective with turnover constraint
# ===========================================================================

test_that("ES + turnover: result has class optimize.portfolio.CVXR and weights are valid", {
  expect_s3_class(opt_es_to, "optimize.portfolio.CVXR")
  w <- extractWeights(opt_es_to)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
  expect_equal(sum(w), 1, tolerance = TOL_WSUM)
  expect_true(all(w >= -TOL_WSUM))
})
