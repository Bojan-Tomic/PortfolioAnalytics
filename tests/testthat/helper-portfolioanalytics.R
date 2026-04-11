###############################################################################
# tests/testthat/helper-portfolioanalytics.R
#
# Shared fixtures and helper functions for the PortfolioAnalytics test suite.
# testthat sources all helper-*.R files automatically before running any tests,
# so every test file has access to the objects and functions defined here
# without needing to repeat the setup.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

# ---------------------------------------------------------------------------
# Standard return series slices from the built-in edhec dataset
# ---------------------------------------------------------------------------
utils::data(edhec)

edhec4 <- edhec[, 1:4] # 4-asset subset  (used by backwards-compat tests)
edhec5 <- edhec[, 1:5] # 5-asset subset  (default for most unit tests)
edhec8 <- edhec[, 1:8] # 8-asset subset  (used by efficient-frontier tests)
edhec13 <- edhec # full 13-column dataset

# Convenience aliases matching column names
funds4 <- colnames(edhec4)
funds5 <- colnames(edhec5)
funds8 <- colnames(edhec8)
funds13 <- colnames(edhec13)

# ---------------------------------------------------------------------------
# Portfolio spec factory helpers
# Each returns a fresh portfolio.spec object with a standard constraint set.
# All helpers accept an optional return-series R so callers can substitute
# a different dataset without changing the helper.
# ---------------------------------------------------------------------------

#' Minimal full-investment, long-only portfolio (no objectives)
make_lo_portf <- function(R = edhec5) {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p
}

#' Full-investment, box-constrained portfolio (no objectives)
#' @param min Per-asset lower bound (scalar or vector)
#' @param max Per-asset upper bound (scalar or vector)
make_box_portf <- function(R = edhec5, min = 0.05, max = 0.40) {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = min, max = max)
  p
}

#' Full-investment, long-only, min-StdDev portfolio spec
make_minvar_portf <- function(R = edhec5) {
  p <- make_lo_portf(R)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
}

#' Full-investment, long-only, min-ES portfolio spec
make_mines_portf <- function(R = edhec5) {
  p <- make_lo_portf(R)
  p <- add.objective(p, type = "risk", name = "ES")
  p
}

#' Full-investment, long-only, max-mean-return portfolio spec
make_maxret_portf <- function(R = edhec5) {
  p <- make_lo_portf(R)
  p <- add.objective(p, type = "return", name = "mean")
  p
}

# ---------------------------------------------------------------------------
# Tolerance constants
# ---------------------------------------------------------------------------

# Numerical tolerance for deterministic (ROI/quadprog/glpk) solver tests
TOL_OPT <- 1e-6

# Tolerance for weight-sum checks (full_investment satisfied within 1e-4)
TOL_WSUM <- 1e-4

# ---------------------------------------------------------------------------
# Assertion helpers
# ---------------------------------------------------------------------------

#' Check that a weight vector satisfies full-investment and box constraints.
#'
#' Calls testthat expectations directly, so it must be used inside a
#' test_that() block.
#'
#' @param w       Named numeric weight vector.
#' @param min_sum Minimum allowable weight sum (default 0.99).
#' @param max_sum Maximum allowable weight sum (default 1.01).
#' @param lb      Per-asset lower bound (scalar or vector, default 0).
#' @param ub      Per-asset upper bound (scalar or vector, default 1).
expect_valid_weights <- function(w,
                                 min_sum = 0.99,
                                 max_sum = 1.01,
                                 lb = 0,
                                 ub = 1) {
  expect_true(is.numeric(w),
    label = "weights are numeric"
  )
  expect_true(sum(w) >= min_sum - TOL_WSUM,
    label = paste("weight sum >=", min_sum)
  )
  expect_true(sum(w) <= max_sum + TOL_WSUM,
    label = paste("weight sum <=", max_sum)
  )
  expect_true(all(w >= lb - TOL_WSUM),
    label = "all weights >= lower bound"
  )
  expect_true(all(w <= ub + TOL_WSUM),
    label = "all weights <= upper bound"
  )
}

#' Check that an optimize.portfolio result has the expected structure.
#'
#' @param result   Object returned by optimize.portfolio().
#' @param cls      Expected S3 class string (e.g. "optimize.portfolio.ROI").
expect_valid_opt_result <- function(result, cls) {
  expect_s3_class(result, cls)
  expect_true(is.numeric(extractWeights(result)),
    label = "extractWeights returns numeric"
  )
  expect_false(any(is.na(extractWeights(result))),
    label = "no NA weights"
  )
}

# ---------------------------------------------------------------------------
# Phase 7: Stochastic / alternative solver result fixtures
#
# Each block is guarded with requireNamespace() so a missing package never
# breaks the helper.  tryCatch ensures that even a solver failure (e.g. an
# infeasible problem on a particular R version) cannot prevent the rest of the
# test suite from loading.
#
# Solver tests use skip_if(is.null(opt_<solver>)) to skip gracefully.
# Chart tests (Phase 6) reuse these same objects to avoid re-running
# expensive optimizations.
# ---------------------------------------------------------------------------

# Shared minimal 4-asset spec for stochastic solvers (fast with small maxit).
.portf_stoch4 <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
})

# PSO (Particle Swarm Optimisation) ----------------------------------------
opt_pso <- NULL
if (requireNamespace("pso", quietly = TRUE)) {
  opt_pso <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, .portf_stoch4,
                       optimize_method = "pso",
                       maxit = 100)
  }, error = function(e) NULL)
}

# GenSA (Generalized Simulated Annealing) -----------------------------------
opt_gensa <- NULL
if (requireNamespace("GenSA", quietly = TRUE)) {
  opt_gensa <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, .portf_stoch4,
                       optimize_method = "GenSA",
                       maxit = 50)
  }, error = function(e) NULL)
}

# mco (Multi-Criteria Optimisation) ----------------------------------------
opt_mco <- NULL
if (requireNamespace("mco", quietly = TRUE)) {
  opt_mco <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, .portf_stoch4,
                       optimize_method = "mco")
  }, error = function(e) NULL)
}

# optimize.portfolio.parallel (requires foreach + a parallel backend) ------
opt_parallel <- NULL
if (requireNamespace("doParallel", quietly = TRUE) &&
    requireNamespace("foreach",    quietly = TRUE)) {
  opt_parallel <- tryCatch({
    cl <- parallel::makeCluster(2L)
    doParallel::registerDoParallel(cl)
    set.seed(42)
    res <- optimize.portfolio.parallel(
      edhec4, .portf_stoch4,
      optimize_method = "random",
      nodes          = 2L,
      search_size    = 200L,
      trace          = TRUE
    )
    parallel::stopCluster(cl)
    foreach::registerDoSEQ()
    res
  }, error = function(e) {
    tryCatch(foreach::registerDoSEQ(), error = function(e2) NULL)
    NULL
  })
}

# ---------------------------------------------------------------------------
# Phase 6: Chart test fixtures (trace = TRUE result objects)
#
# Chart functions that call applyFUN(), extractStats(), or scatterFUN()
# all require object$R, which is only stored when trace = TRUE.
#
# opt_roi_trace  — used by: test-charts-roi.R, test-charts-other.R
# opt_rp_trace   — used by: test-charts-rp.R, test-charts-other.R
#
# Both specs include a mean (return) objective AND a risk objective so that
# extractStats() output contains "mean" and the risk column — a requirement
# for chart.EfficientFrontier and chart.Concentration.
# ---------------------------------------------------------------------------

# Portfolio spec: mean + StdDev, long-only, full-investment (ROI-solvable).
.portf_meansd4 <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "StdDev")
  p
})

# ROI min-StdDev result with trace = TRUE (deterministic, very fast).
opt_roi_trace <- tryCatch(
  optimize.portfolio(edhec4, .portf_meansd4,
                     optimize_method = "ROI",
                     trace = TRUE),
  error = function(e) NULL
)

# Portfolio spec: mean + ES, long-only, full-investment (random / RP).
.portf_meanES4 <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "ES")
  p
})

# Random portfolio result with trace = TRUE (small search_size for speed).
opt_rp_trace <- tryCatch({
  set.seed(42)
  optimize.portfolio(edhec4, .portf_meanES4,
                     optimize_method = "random",
                     trace       = TRUE,
                     search_size = 500L)
}, error = function(e) NULL)
