###############################################################################
# tests/testthat/test-pso-solver.R
#
# Source files covered:
#   R/optimize.portfolio.R  — optimize_method = "pso" dispatch
#   R/generics.R            — print.optimize.portfolio.pso
#   R/extractstats.R        — extractStats.optimize.portfolio.pso
#
# Tests the PSO (Particle Swarm Optimisation) solver via pso::psoptim.
# All fixture-based tests use opt_pso from helper-portfolioanalytics.R.
# The trace-specific test builds a local object inside its test_that block.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

skip_if_not_installed("pso")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# 1  S3 class
# ---------------------------------------------------------------------------

test_that("opt_pso has the correct s3 class hierarchy", {
  skip_if(is.null(opt_pso))
  expect_s3_class(opt_pso, "optimize.portfolio.pso")
  expect_s3_class(opt_pso, "optimize.portfolio")
})

# ---------------------------------------------------------------------------
# 2  Weight validity
# ---------------------------------------------------------------------------

test_that("extractWeights returns a valid numeric weight vector for opt_pso", {
  skip_if(is.null(opt_pso))
  w <- extractWeights(opt_pso)
  expect_true(is.numeric(w),
    label = "weights are numeric"
  )
  expect_length(w, 4L)
  expect_true(
    sum(w) >= 0.99 - 1e-4 & sum(w) <= 1.01 + 1e-4,
    label = "weights sum within portfolio bounds [0.99, 1.01]"
  )
  # Note: PSO is a heuristic solver — it minimises a penalised objective but
  # does not guarantee that every weight strictly satisfies the box lower
  # bound.  We only verify weights are finite (not that they hit 0.05).
  expect_true(all(is.finite(w)), label = "all weights are finite")
})

# ---------------------------------------------------------------------------
# 3  print method
# ---------------------------------------------------------------------------

test_that("print.optimize.portfolio.pso produces output containing PortfolioAnalytics", {
  skip_if(is.null(opt_pso))
  expect_output(print(opt_pso), "PortfolioAnalytics")
})

# ---------------------------------------------------------------------------
# 4  objective_measures slot
# ---------------------------------------------------------------------------

test_that("opt_pso objective_measures slot is a non-null list", {
  skip_if(is.null(opt_pso))
  om <- opt_pso$objective_measures
  expect_false(is.null(om))
  expect_true(is.list(om))
})

# ---------------------------------------------------------------------------
# 5–6  trace = TRUE populates PSOoutput
# ---------------------------------------------------------------------------

test_that("running pso with trace equals true populates the PSOoutput slot as a list", {
  skip_if_not_installed("pso")
  set.seed(123)
  opt_pso_trace <- optimize.portfolio(
    edhec4,
    .portf_stoch4,
    optimize_method = "pso",
    maxit           = 50,
    trace           = TRUE
  )
  expect_false(is.null(opt_pso_trace$PSOoutput))
  expect_true(is.list(opt_pso_trace$PSOoutput))
})
