###############################################################################
# tests/testthat/test-issue-12-random-portfolios-no-box.R
#
# Regression tests for:
#   Issue #12 — random_portfolios() (rp_method='sample') crashes with
#               "'from' must be a finite number" when no box constraint is
#               specified.
#
# Root cause: randomize_portfolio_v2() calls
#   generatesequence(min=min(constraints$min), max=max(constraints$max), ...)
# without checking whether constraints$min/max are -Inf/Inf (which is the
# default when no box constraint exists). seq(from=-Inf,...) throws the error.
# The same pattern exists in constraint_fn_map.R / rp_transform().
#
# Fix: guard the generatesequence() call to fall back to [0,1] whenever
# the bounds are not finite.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
skip_on_cran()

utils::data(edhec)
R4 <- edhec[, 1:4]

# ---------------------------------------------------------------------------
# Issue #12 — rp_method='sample' without box constraint
# ---------------------------------------------------------------------------

test_that("#12 random_portfolios rp_method='sample' works without box constraint", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(portfolio = pspec, type = "full_investment")

  # Before the fix this threw:
  # Error in seq.default(from = round(min, rounding), ...) :
  #   'from' must be a finite number
  expect_no_error(
    random_portfolios(portfolio = pspec, permutations = 100,
                      rp_method = "sample")
  )
})

test_that("#12 random_portfolios result is a matrix with correct dimensions", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(portfolio = pspec, type = "full_investment")

  set.seed(42)
  rp <- random_portfolios(portfolio = pspec, permutations = 100,
                          rp_method = "sample")
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), ncol(R4))
})

test_that("#12 random_portfolios result rows sum to approx 1 (full_investment)", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(portfolio = pspec, type = "full_investment")

  set.seed(42)
  rp <- random_portfolios(portfolio = pspec, permutations = 100,
                          rp_method = "sample")
  row_sums <- rowSums(rp)
  expect_true(all(abs(row_sums - 1) < 0.02),
              label = "all portfolio rows sum to ~1")
})

test_that("#12 random_portfolios rp_method='simplex' works without box constraint", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(portfolio = pspec, type = "full_investment")

  set.seed(42)
  expect_no_error(
    random_portfolios(portfolio = pspec, permutations = 100,
                      rp_method = "simplex")
  )
})

test_that("#12 random_portfolios rp_method='grid' works without box constraint", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(portfolio = pspec, type = "full_investment")

  set.seed(42)
  expect_no_error(
    random_portfolios(portfolio = pspec, permutations = 100,
                      rp_method = "grid")
  )
})

# ---------------------------------------------------------------------------
# Sanity check: with box constraints (existing behaviour must be preserved)
# ---------------------------------------------------------------------------

test_that("#12 random_portfolios still works correctly with explicit box constraint", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(portfolio = pspec, type = "full_investment")
  pspec <- add.constraint(portfolio = pspec, type = "box", min = 0.05, max = 0.60)

  set.seed(42)
  rp <- random_portfolios(portfolio = pspec, permutations = 100,
                          rp_method = "sample")
  expect_true(is.matrix(rp))
  expect_true(all(rp >= 0.04))      # box lower bound (with some tolerance)
  expect_true(all(rp <= 0.61))      # box upper bound (with some tolerance)
})

# ---------------------------------------------------------------------------
# Expanded coverage
# ---------------------------------------------------------------------------

test_that("#12 optimize.portfolio random method works without box constraint", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.objective(pspec, type = "risk", name = "StdDev")

  set.seed(42)
  expect_no_error(
    opt <- optimize.portfolio(R4, pspec, optimize_method = "random",
                              search_size = 200L, trace = FALSE)
  )
  w <- extractWeights(opt)
  expect_true(all(is.finite(w)))
  expect_equal(sum(w), 1, tolerance = 0.02)
})

test_that("#12 random_portfolios rp_method='sample' works with 8-asset portfolio", {
  R8 <- edhec[, 1:8]
  pspec <- portfolio.spec(assets = colnames(R8))
  pspec <- add.constraint(pspec, type = "full_investment")

  set.seed(42)
  expect_no_error(
    rp <- random_portfolios(pspec, permutations = 100, rp_method = "sample")
  )
  expect_equal(ncol(rp), 8L)
  expect_true(all(abs(rowSums(rp) - 1) < 0.02))
})

test_that("#12 random_portfolios long-short (weight_sum) works without explicit box", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(pspec, type = "weight_sum", min_sum = -0.3, max_sum = 1.3)

  set.seed(42)
  expect_no_error(
    rp <- random_portfolios(pspec, permutations = 100, rp_method = "sample")
  )
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), ncol(R4))
})

test_that("#12 rp_method='simplex' produces portfolios with correct column count", {
  pspec <- portfolio.spec(assets = colnames(R4))
  pspec <- add.constraint(pspec, type = "full_investment")

  set.seed(42)
  rp <- random_portfolios(pspec, permutations = 50, rp_method = "simplex")
  expect_equal(ncol(rp), ncol(R4))
  expect_true(all(abs(rowSums(rp) - 1) < 0.02))
})
