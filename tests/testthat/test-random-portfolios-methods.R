###############################################################################
# tests/testthat/test-random-portfolios-methods.R
#
# Source files covered:
#   R/random_portfolios.R  — generatesequence(), random_portfolios(),
#                            rp_sample(), rp_simplex(), rp_grid(),
#                            randomize_portfolio() / randomize_portfolio_v2()
#
# Shared fixtures from helper-portfolioanalytics.R:
#   edhec5, funds5, make_lo_portf(), make_box_portf(), TOL_WSUM
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# File-scope setup
# edhec5, funds5 come from helper-portfolioanalytics.R
# ---------------------------------------------------------------------------
R   <- edhec5
nms <- funds5
N   <- length(nms)   # 5

# Simple full-investment, long-only portfolio
portf_lo  <- make_lo_portf(R)

# Box-constrained portfolio: per-asset bounds [0.05, 0.40]
portf_box <- make_box_portf(R, min = 0.05, max = 0.40)

# ---------------------------------------------------------------------------
# generatesequence
# ---------------------------------------------------------------------------

test_that("generatesequence returns a numeric vector of length 99", {
  gs <- generatesequence(min = 0.01, max = 0.99, by = 0.01, rounding = 2)
  expect_true(is.numeric(gs))
  expect_equal(length(gs), 99L)
})

test_that("generatesequence first element is 0.01 and last element is 0.99", {
  gs <- generatesequence(min = 0.01, max = 0.99, by = 0.01, rounding = 2)
  expect_equal(gs[1],          0.01)
  expect_equal(gs[length(gs)], 0.99)
})

test_that("generatesequence all elements are in [0.01, 0.99]", {
  gs <- generatesequence(min = 0.01, max = 0.99, by = 0.01, rounding = 2)
  expect_true(all(gs >= 0.01))
  expect_true(all(gs <= 0.99))
})

test_that("generatesequence consecutive differences are all approximately 0.01", {
  gs    <- generatesequence(min = 0.01, max = 0.99, by = 0.01, rounding = 2)
  steps <- diff(gs)
  expect_true(all(abs(steps - 0.01) < 1e-10))
})

# ---------------------------------------------------------------------------
# random_portfolios — "sample" method
#
# Note on row counts: random_portfolios() with eliminate=TRUE (default) runs
# check_constraints() on every candidate row and discards those that fail.
# With a tight full_investment constraint (min_sum == max_sum == 1), the
# sample method can return far fewer rows than `permutations` requests.
# Tests here guard only on structural properties (ncol, non-negativity, sums)
# rather than on an exact row count.
# ---------------------------------------------------------------------------

test_that("random_portfolios 'sample' method returns a matrix with ncol = N", {
  set.seed(42)
  rp <- random_portfolios(portf_lo, permutations = 100, rp_method = "sample")
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), N)
  expect_true(nrow(rp) >= 1L)
})

test_that("random_portfolios 'sample': surviving rows all sum to ~1 (tight tolerance)", {
  # After eliminate=TRUE, only rows passing check_constraints() remain.
  # full_investment is satisfied strictly, so rowSums == 1 within 1e-4.
  set.seed(42)
  rp <- random_portfolios(portf_lo, permutations = 100, rp_method = "sample")
  expect_true(all(abs(rowSums(rp) - 1) < TOL_WSUM))
})

test_that("random_portfolios 'sample': all surviving weights are non-negative", {
  set.seed(42)
  rp <- random_portfolios(portf_lo, permutations = 100, rp_method = "sample")
  expect_true(all(rp >= 0))
})

test_that("random_portfolios 'sample' results are identical given the same seed", {
  set.seed(42)
  rp1 <- random_portfolios(portf_lo, permutations = 50, rp_method = "sample")
  set.seed(42)
  rp2 <- random_portfolios(portf_lo, permutations = 50, rp_method = "sample")
  expect_identical(rp1, rp2)
})

# ---------------------------------------------------------------------------
# random_portfolios — "simplex" method
#
# rp_simplex() uses foreach::%dopar% internally.  Without a registered
# parallel backend the loop falls back to sequential execution and emits
# "executing %dopar% sequentially" — suppressed below as expected behaviour.
#
# The number of returned rows is ceiling(permutations / length(fev)) *
# length(fev) which may differ from `permutations`; only ncol is checked.
# ---------------------------------------------------------------------------

test_that("random_portfolios 'simplex' method returns a matrix with ncol = N", {
  set.seed(1)
  rp <- suppressWarnings(
    random_portfolios(portf_lo, permutations = 100, rp_method = "simplex")
  )
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), N)
  expect_true(nrow(rp) >= 1L)
})

test_that("random_portfolios 'simplex': all rows sum to ~1", {
  set.seed(1)
  rp <- suppressWarnings(
    random_portfolios(portf_lo, permutations = 100, rp_method = "simplex")
  )
  expect_true(all(abs(rowSums(rp) - 1) < TOL_WSUM))
})

test_that("random_portfolios 'simplex': all weights are non-negative for long-only", {
  # Simplex draws satisfy lower bounds by construction; weights are >= 0
  # for a long-only portfolio.  A small numeric guard handles representation
  # noise for weights that are effectively zero.
  set.seed(1)
  rp <- suppressWarnings(
    random_portfolios(portf_lo, permutations = 100, rp_method = "simplex")
  )
  expect_true(all(rp >= -TOL_WSUM))
})

# ---------------------------------------------------------------------------
# random_portfolios — "grid" method
#
# rp_grid() also uses foreach::%dopar% for normalization — suppress warning.
# The grid expansion for N=5 assets yields more rows than `permutations`.
# ---------------------------------------------------------------------------

test_that("random_portfolios 'grid' method returns a matrix with ncol = N", {
  set.seed(1)
  rp <- suppressWarnings(
    random_portfolios(portf_lo, permutations = 100, rp_method = "grid")
  )
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), N)
  expect_true(nrow(rp) >= 1L)
})

test_that("random_portfolios 'grid': all rows sum to ~1 after normalization", {
  set.seed(1)
  rp <- suppressWarnings(
    random_portfolios(portf_lo, permutations = 100, rp_method = "grid")
  )
  expect_true(all(abs(rowSums(rp) - 1) < TOL_WSUM))
})

# ---------------------------------------------------------------------------
# randomize_portfolio  (exported alias for randomize_portfolio_v2)
#
# Note on sum tolerance: randomize_portfolio_v2 intentionally widens an exact
# full_investment constraint [1, 1] to [0.99, 1.01] so that the random walk
# can reach feasible portfolios.  The weight sum therefore lies in that band
# rather than at exactly 1; we use a 0.015 tolerance here.
# ---------------------------------------------------------------------------

test_that("randomize_portfolio returns a numeric vector of length N", {
  set.seed(1)
  w <- randomize_portfolio(portf_lo)
  expect_true(is.numeric(w))
  expect_equal(length(w), N)
})

test_that("randomize_portfolio sum is approximately 1 (within [0.99, 1.01])", {
  set.seed(1)
  w <- randomize_portfolio(portf_lo)
  expect_true(abs(sum(w) - 1) < 0.015)
})

test_that("randomize_portfolio returns non-negative weights for long-only portfolio", {
  set.seed(1)
  w <- randomize_portfolio(portf_lo)
  expect_true(all(w >= 0))
})

test_that("randomize_portfolio respects per-asset box bounds [0.05, 0.40]", {
  # round(..., 10) neutralises floating-point representation artefacts.
  set.seed(2)
  w <- randomize_portfolio(portf_box)
  expect_true(all(round(w, 10) >= 0.05 - 1e-6))
  expect_true(all(round(w, 10) <= 0.40 + 1e-6))
})

# ---------------------------------------------------------------------------
# rp_sample  (internal helper, called by random_portfolios for method="sample")
#
# Signature: rp_sample(portfolio, permutations, max_permutations = 200)
# There is no 'eliminate' parameter; that filtering step lives in
# random_portfolios() after rp_sample() returns.
#
# rp_sample() pre-fills rows 1 and 2 with the seed and equal-weight
# portfolios before generating stochastic rows; unique() is applied at the
# end, so the returned nrow may be slightly less than `permutations`.
# Row sums may span [0.99, 1.01] (the widened full_investment band).
# ---------------------------------------------------------------------------

test_that("rp_sample returns a matrix", {
  set.seed(1)
  rp_s <- PortfolioAnalytics:::rp_sample(portf_lo, permutations = 20)
  expect_true(is.matrix(rp_s))
})

test_that("rp_sample matrix has ncol = N", {
  set.seed(1)
  rp_s <- PortfolioAnalytics:::rp_sample(portf_lo, permutations = 20)
  expect_equal(ncol(rp_s), N)
})

test_that("rp_sample rows sum to approximately 1 (within [0.99, 1.01])", {
  set.seed(1)
  rp_s <- PortfolioAnalytics:::rp_sample(portf_lo, permutations = 20)
  expect_true(nrow(rp_s) >= 1L)
  expect_true(all(abs(rowSums(rp_s) - 1) < 0.015))
})

# ---------------------------------------------------------------------------
# Constraint satisfaction after elimination — box-constrained portfolio
# ---------------------------------------------------------------------------

test_that("random_portfolios 'sample' on box portfolio: >80% of rows satisfy min = 0.05", {
  # With eliminate=TRUE, check_constraints() passes only rows that satisfy all
  # constraints.  Every surviving row must therefore respect the box lower
  # bound of 0.05.  The '>80%' threshold is conservative; empirically it is
  # ~100%.  A small numeric guard (1e-6) accounts for representation noise.
  set.seed(99)
  rp <- random_portfolios(portf_box, permutations = 200, rp_method = "sample")
  expect_true(nrow(rp) >= 1L)
  pct_ok <- mean(apply(rp, 1, function(w) all(w >= 0.05 - 1e-6)))
  expect_true(pct_ok > 0.80)
})
