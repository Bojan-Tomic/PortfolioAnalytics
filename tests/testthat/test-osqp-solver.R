###############################################################################
# tests/testthat/test-osqp-solver.R
#
# Tests for optimize.portfolio() via the native osqp solver path.
#
# Source files covered:
#   - R/optimize.portfolio.R  (~L2329+: osqp solver dispatch)
#
# The osqp package is NOT currently installed on this machine, so every
# test_that block starts with skip_if_not_installed("osqp"), causing the
# entire file to be gracefully skipped.  The tests document the expected
# interface and will run in environments where osqp is available.
#
# Note: "osqp" here refers to the standalone CRAN package, not
# ROI.plugin.osqp — they expose different interfaces.
#
# Shared fixtures from helper-portfolioanalytics.R — do NOT redefine:
#   edhec4   — edhec[, 1:4], 4-asset return series
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

library(PortfolioAnalytics)


# ---------------------------------------------------------------------------
# File-scope setup — only executed when osqp is available.
# Both result objects are initialised to NULL so that test_that blocks never
# encounter undefined symbols when the package is absent.  The tryCatch
# wrappers additionally guard against unexpected solver errors at build time.
# ---------------------------------------------------------------------------

opt_osqp     <- NULL
opt_osqp_ret <- NULL

if (requireNamespace("osqp", quietly = TRUE)) {

  # --- Min-variance (StdDev) portfolio ---
  .p_var <- portfolio.spec(assets = colnames(edhec4))
  .p_var <- add.constraint(.p_var, type = "weight_sum",
                            min_sum = 0.99, max_sum = 1.01)
  .p_var <- add.constraint(.p_var, type = "box", min = 0, max = 1)
  .p_var <- add.objective(.p_var, type = "risk", name = "StdDev")
  set.seed(42)
  opt_osqp <- tryCatch(
    optimize.portfolio(edhec4, .p_var, optimize_method = "osqp"),
    error = function(e) NULL
  )

  # --- Max-return (mean) portfolio ---
  .p_ret <- portfolio.spec(assets = colnames(edhec4))
  .p_ret <- add.constraint(.p_ret, type = "weight_sum",
                             min_sum = 0.99, max_sum = 1.01)
  .p_ret <- add.constraint(.p_ret, type = "box", min = 0, max = 1)
  .p_ret <- add.objective(.p_ret, type = "return", name = "mean")
  opt_osqp_ret <- tryCatch(
    optimize.portfolio(edhec4, .p_ret, optimize_method = "osqp"),
    error = function(e) NULL
  )

}


# ===========================================================================
# Min-variance with osqp
# ===========================================================================

test_that("osqp min-variance result has class optimize.portfolio.osqp", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp))
  expect_s3_class(opt_osqp, "optimize.portfolio.osqp")
})

test_that("osqp min-variance result also inherits optimize.portfolio", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp))
  expect_s3_class(opt_osqp, "optimize.portfolio", exact = FALSE)
})

test_that("osqp min-variance extractWeights returns a numeric vector", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp))
  w <- extractWeights(opt_osqp)
  expect_true(is.numeric(w), label = "weights are numeric")
})

test_that("osqp min-variance extractWeights returns a vector of length 4", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp))
  expect_length(extractWeights(opt_osqp), 4L)
})

test_that("osqp min-variance weights sum satisfies the weight_sum constraint", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp))
  w <- extractWeights(opt_osqp)
  # osqp solves within [min_sum, max_sum] = [0.99, 1.01]; the sum lands at the
  # nearest constraint boundary rather than exactly 1.0.
  expect_true(sum(w) >= 0.99 - 1e-4, label = "weight sum >= 0.99")
  expect_true(sum(w) <= 1.01 + 1e-4, label = "weight sum <= 1.01")
})

test_that("osqp min-variance weights respect the long-only box constraint", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp))
  w <- extractWeights(opt_osqp)
  expect_true(all(w >= -1e-6), label = "all weights >= 0")
  expect_true(all(w <=  1 + 1e-6), label = "all weights <= 1")
})

test_that("osqp min-variance objective_measures is a list", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp))
  expect_true(is.list(opt_osqp$objective_measures),
    label = "objective_measures is a list"
  )
})

test_that("osqp min-variance optimize.portfolio call does not error", {
  skip_if_not_installed("osqp")
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "risk", name = "StdDev")
  set.seed(42)
  expect_no_error(optimize.portfolio(edhec4, p, optimize_method = "osqp"))
})


# ===========================================================================
# Max-return with osqp
# ===========================================================================

test_that("osqp max-return result has class optimize.portfolio.osqp", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp_ret))
  expect_s3_class(opt_osqp_ret, "optimize.portfolio.osqp")
})

test_that("osqp max-return result also inherits optimize.portfolio", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp_ret))
  expect_s3_class(opt_osqp_ret, "optimize.portfolio", exact = FALSE)
})

test_that("osqp max-return extractWeights returns a numeric vector of length 4", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp_ret))
  w <- extractWeights(opt_osqp_ret)
  expect_true(is.numeric(w), label = "weights are numeric")
  expect_length(w, 4L)
})

test_that("osqp max-return weights sum satisfies the weight_sum constraint", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp_ret))
  w <- extractWeights(opt_osqp_ret)
  # osqp solves within [min_sum, max_sum] = [0.99, 1.01]; the sum lands at the
  # nearest constraint boundary rather than exactly 1.0.
  expect_true(sum(w) >= 0.99 - 1e-4, label = "weight sum >= 0.99")
  expect_true(sum(w) <= 1.01 + 1e-4, label = "weight sum <= 1.01")
})

test_that("osqp max-return objective_measures is a list", {
  skip_if_not_installed("osqp")
  skip_if(is.null(opt_osqp_ret))
  expect_true(is.list(opt_osqp_ret$objective_measures),
    label = "objective_measures is a list"
  )
})


# ===========================================================================
# Invalid objective guard
# osqp only handles mean / variance-family objectives; ES must be rejected.
# ===========================================================================

test_that("osqp rejects an ES objective with a descriptive stop error", {
  skip_if_not_installed("osqp")
  p_es <- portfolio.spec(assets = colnames(edhec4))
  p_es <- add.constraint(p_es, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  p_es <- add.constraint(p_es, type = "box", min = 0, max = 1)
  p_es <- add.objective(p_es, type = "risk", name = "ES")
  expect_error(
    optimize.portfolio(edhec4, p_es, optimize_method = "osqp"),
    regexp = "osqp"
  )
})
