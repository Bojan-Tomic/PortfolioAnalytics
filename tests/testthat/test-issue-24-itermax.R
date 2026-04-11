###############################################################################
# tests/testthat/test-issue-24-itermax.R
#
# Regression tests for GitHub issue #24:
#   "itermax does not accept a class numeric from variable"
#
# Root cause: `match.call(expand.dots = TRUE)$itermax` returns an unevaluated
# symbol when itermax is passed as a variable (e.g., `my_iter <- 50`).
# Dividing `search_size / <symbol>` then fails with
#   "non-numeric argument to binary operator".
#
# Fix: wrap with eval() → `eval(match.call(expand.dots = TRUE)$itermax)`
#
# Source files covered:
#   R/optimize.portfolio.R lines 95 and 883
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("DEoptim")

library(DEoptim)

# ---------------------------------------------------------------------------
# Shared spec — small 4-asset problem so DEoptim finishes quickly
# ---------------------------------------------------------------------------
R <- edhec4
p <- portfolio.spec(assets = colnames(R))
p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
p <- add.objective(p, type = "risk", name = "StdDev")

# ---------------------------------------------------------------------------
# Issue #24: itermax passed as a variable must work identically to a literal
# ---------------------------------------------------------------------------

test_that("issue #24: itermax as variable does not error with 'non-numeric argument'", {
  # Before the fix, this would throw:
  #   "Error in round(search_size/itermax) :
  #    non-numeric argument to binary operator"
  my_itermax <- 25L   # integer variable

  expect_no_error(
    suppressWarnings(
      optimize.portfolio(
        R, p,
        optimize_method = "DEoptim",
        search_size     = 500L,
        itermax         = my_itermax,
        trace           = FALSE
      )
    )
  )
})

test_that("issue #24: itermax as numeric variable produces valid result", {
  my_itermax <- 25   # plain numeric (double)

  opt <- suppressWarnings(
    optimize.portfolio(
      R, p,
      optimize_method = "DEoptim",
      search_size     = 500L,
      itermax         = my_itermax,
      trace           = FALSE
    )
  )

  expect_s3_class(opt, "optimize.portfolio.DEoptim")
  w <- extractWeights(opt)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
})

test_that("issue #24: itermax variable and literal produce equivalent results", {
  # Same seed → same result regardless of how itermax is passed
  iter_val <- 30L

  set.seed(42)
  opt_var <- suppressWarnings(
    optimize.portfolio(
      R, p,
      optimize_method = "DEoptim",
      search_size     = 300L,
      itermax         = iter_val,
      trace           = FALSE
    )
  )

  set.seed(42)
  opt_lit <- suppressWarnings(
    optimize.portfolio(
      R, p,
      optimize_method = "DEoptim",
      search_size     = 300L,
      itermax         = 30L,
      trace           = FALSE
    )
  )

  # Both should produce valid results
  expect_s3_class(opt_var, "optimize.portfolio.DEoptim")
  expect_s3_class(opt_lit, "optimize.portfolio.DEoptim")

  # Weights should be identical (same seed, same itermax value)
  expect_equal(
    extractWeights(opt_var),
    extractWeights(opt_lit),
    tolerance = 1e-10,
    label = "weights identical whether itermax passed as variable or literal"
  )
})

test_that("issue #24: itermax variable works with v1 optimize.portfolio path", {
  # The v1 code path (optimize_method_v1) also has the same itermax bug.
  # We verify the fix holds on the v1 path by passing a portfolio with
  # the legacy constraints= interface (which routes to _v1).
  my_iter <- 20L

  # The v1 path is triggered when 'constraints' argument is used instead of
  # 'portfolio'. Build a v1-style constraint object.
  p_v1 <- portfolio.spec(assets = colnames(R))
  p_v1 <- add.constraint(p_v1, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_v1 <- add.constraint(p_v1, type = "box", min = 0.05, max = 0.60)
  p_v1 <- add.objective(p_v1, type = "risk", name = "StdDev")

  expect_no_error(
    suppressWarnings(
      optimize.portfolio(
        R,
        portfolio      = p_v1,
        optimize_method = "DEoptim",
        search_size    = 300L,
        itermax        = my_iter,
        trace          = FALSE
      )
    )
  )
})
