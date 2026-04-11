###############################################################################
# tests/testthat/test-mco-solver.R
#
# Source files covered:
#   R/optimize.portfolio.R  — optimize_method = "mco" dispatch (mco::nsga2)
#
# Tests the mco (Multi-Criteria Optimisation) solver via mco::nsga2.
# All fixture-based tests use opt_mco from helper-portfolioanalytics.R.
# The trace-specific test builds a local object inside its test_that block.
#
# Note: there is no custom print.optimize.portfolio.mco method in generics.R;
# print() falls back to print.default / the base list printer.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

skip_if_not_installed("mco")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# 1  S3 class
# ---------------------------------------------------------------------------

test_that("opt_mco has the correct s3 class hierarchy", {
  skip_if(is.null(opt_mco))
  expect_s3_class(opt_mco, "optimize.portfolio.mco")
  expect_s3_class(opt_mco, "optimize.portfolio")
})

# ---------------------------------------------------------------------------
# 2  Weight validity
# ---------------------------------------------------------------------------

test_that("extractWeights returns a valid numeric weight vector for opt_mco", {
  skip_if(is.null(opt_mco))
  w <- extractWeights(opt_mco)
  expect_true(is.numeric(w),
    label = "weights are numeric"
  )
  expect_length(w, 4L)
  expect_true(
    sum(w) >= 0.99 - 1e-4,
    label = "weights sum satisfies min_sum constraint (>= 0.99)"
  )
  expect_true(
    sum(w) <= 1.01 + 1e-4,
    label = "weights sum satisfies max_sum constraint (<= 1.01)"
  )
})

# ---------------------------------------------------------------------------
# 3  objective_measures slot
# ---------------------------------------------------------------------------

test_that("opt_mco objective_measures slot is non-null and list or named numeric", {
  skip_if(is.null(opt_mco))
  om <- opt_mco$objective_measures
  expect_false(is.null(om))
  expect_true(is.list(om) || is.numeric(om),
    label = "objective_measures is a list or named numeric"
  )
})

# ---------------------------------------------------------------------------
# 4  out slot
# ---------------------------------------------------------------------------

test_that("opt_mco out slot is a non-null numeric value", {
  skip_if(is.null(opt_mco))
  out_val <- opt_mco$out
  expect_false(is.null(out_val))
  expect_true(is.numeric(out_val),
    label = "out slot is numeric"
  )
})

# ---------------------------------------------------------------------------
# 5–6  trace = TRUE populates mcooutput with element par
# ---------------------------------------------------------------------------

test_that("running mco with trace equals true populates the mcooutput slot as a list with element par", {
  skip_if_not_installed("mco")
  set.seed(123)
  opt_mco_trace <- optimize.portfolio(
    edhec4,
    .portf_stoch4,
    optimize_method = "mco",
    trace           = TRUE
  )
  expect_false(is.null(opt_mco_trace$mcooutput))
  expect_true(is.list(opt_mco_trace$mcooutput),
    label = "mcooutput is a list (raw nsga2 output)"
  )
  expect_true(!is.null(opt_mco_trace$mcooutput$par),
    label = "mcooutput contains element par"
  )
})

# ---------------------------------------------------------------------------
# 7  print falls back gracefully (no custom print method for mco)
# ---------------------------------------------------------------------------

test_that("print does not error for opt_mco", {
  skip_if(is.null(opt_mco))
  expect_no_error(print(opt_mco))
})
