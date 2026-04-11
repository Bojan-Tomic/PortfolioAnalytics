###############################################################################
# tests/testthat/test-gensa-solver.R
#
# Source files covered:
#   R/optimize.portfolio.R  — optimize_method = "GenSA" dispatch
#   R/generics.R            — print.optimize.portfolio.GenSA
#   R/extractstats.R        — extractStats.optimize.portfolio.GenSA
#
# Tests the GenSA (Generalized Simulated Annealing) solver via GenSA::GenSA.
# All fixture-based tests use opt_gensa from helper-portfolioanalytics.R.
# The trace-specific test builds a local object inside its test_that block.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

skip_if_not_installed("GenSA")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# 1  S3 class
# ---------------------------------------------------------------------------

test_that("opt_gensa has the correct s3 class hierarchy", {
  skip_if(is.null(opt_gensa))
  expect_s3_class(opt_gensa, "optimize.portfolio.GenSA")
  expect_s3_class(opt_gensa, "optimize.portfolio")
})

# ---------------------------------------------------------------------------
# 2  Weight validity
# ---------------------------------------------------------------------------

test_that("extractWeights returns a valid numeric weight vector for opt_gensa", {
  skip_if(is.null(opt_gensa))
  w <- extractWeights(opt_gensa)
  expect_true(is.numeric(w),
    label = "weights are numeric"
  )
  expect_length(w, 4L)
  expect_true(
    sum(w) >= 0.99 - 1e-4,
    label = "weights sum >= min_sum (0.99)"
  )
  expect_true(
    sum(w) <= 1.01 + 1e-4,
    label = "weights sum <= max_sum (1.01)"
  )
  expect_true(
    all(w >= 0.05 - 1e-4),
    label = "all weights satisfy the box lower bound of 0.05"
  )
})

# ---------------------------------------------------------------------------
# 3  print method
# ---------------------------------------------------------------------------

test_that("print.optimize.portfolio.GenSA produces output containing PortfolioAnalytics", {
  skip_if(is.null(opt_gensa))
  expect_output(print(opt_gensa), "PortfolioAnalytics")
})

# ---------------------------------------------------------------------------
# 4  objective_measures slot
# ---------------------------------------------------------------------------

test_that("opt_gensa objective_measures slot is a non-null list", {
  skip_if(is.null(opt_gensa))
  om <- opt_gensa$objective_measures
  expect_false(is.null(om))
  expect_true(is.list(om))
})

# ---------------------------------------------------------------------------
# 5–6  trace = TRUE populates GenSAoutput
# ---------------------------------------------------------------------------

test_that("running GenSA with trace equals true populates the GenSAoutput slot as a list", {
  skip_if_not_installed("GenSA")
  set.seed(123)
  opt_gensa_trace <- optimize.portfolio(
    edhec4,
    .portf_stoch4,
    optimize_method = "GenSA",
    maxit           = 30,
    trace           = TRUE
  )
  expect_false(is.null(opt_gensa_trace$GenSAoutput))
  expect_true(is.list(opt_gensa_trace$GenSAoutput))
})
