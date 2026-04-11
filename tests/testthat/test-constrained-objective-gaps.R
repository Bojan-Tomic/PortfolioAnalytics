###############################################################################
# tests/testthat/test-constrained-objective-gaps.R
#
# Source file targeted: R/constrained_objective.R
#
# Coverage gaps addressed (not hit by any prior test file):
#
#  Gap 1 — turnover_objective with numeric target: the penalty branch
#           out += penalty * |multiplier| * |tmp_measure - target|
#           at lines ~667-669 of constrained_objective_v2.
#           Existing test-constrained-objective.R L1136-1144 checks only
#           is.numeric(res); no comparison shows the penalty is actually
#           larger when turnover deviates from the target.
#
#  Gap 2 — risk_budget_objective with min_concentration=TRUE: the HHI-based
#           penalty branch at lines ~722-728 (distinct from min_difference).
#
#  Gap 3 — constrained_objective_v2 normalize=FALSE under-sum penalty:
#           line ~402: sum(w) < min_sum => out += penalty*(min_sum - sum(w)).
#           Only the over-sum branch was tested in prior test files.
#
#  Gap 4 — CSM empty switch arm (line ~605): a known latent bug.
#           The switch arm `CSM = {}` does nothing, leaving `fun` unassigned.
#           The subsequent `if(is.function(fun))` at line ~614 then throws
#           "object 'fun' not found" because there is no prior default
#           assignment of `fun` in this code path.
#           These tests document the known buggy behavior so that a future
#           fix (pre-initialising `fun <- NULL` before the switch, or adding
#           a `next` in the CSM arm) will be detected by a test-suite change.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------
utils::data(edhec)
R5 <- edhec[, 1:5]
w5 <- rep(0.2, 5)


# ===========================================================================
# Gap 1: turnover_objective with numeric target — penalty comparison
#
# turnover() computes sum(|wts.init - weights|) / N from an equal-weight
# baseline when wts.init is NULL.
#
#   Equal weights  (w5):     turnover = 0
#   Concentrated   (w_conc): turnover = (0.6 + 4*0.15) / 5 = 0.24
#
# With target = 0.3:
#   Concentrated: |0.24 - 0.3| = 0.06 → penalty branch fires
#   No-target:    no penalty term, only base multiplier * 0.24
# => with-target out > no-target out for concentrated weights.
#
# With target = 0.0:
#   Equal weights:   |0 - 0| = 0 → no penalty deviation; base term = 0
#   Concentrated: |0.24 - 0| = 0.24 → penalty = 1e4 * 0.24 + base 0.24
# => concentrated weights produce MORE out than equal weights.
# ===========================================================================

# Portfolio with turnover objective AND a numeric target of 0.3.
portf_to_tgt <- portfolio.spec(assets = colnames(R5))
portf_to_tgt <- add.constraint(portf_to_tgt, type = "weight_sum",
                                min_sum = 0.99, max_sum = 1.01)
portf_to_tgt <- add.constraint(portf_to_tgt, type = "box", min = 0, max = 1)
portf_to_tgt <- add.objective(portf_to_tgt,
  type       = "turnover",
  name       = "turnover",
  target     = 0.3,   # realistic target: 30% two-way turnover
  multiplier = 1
)

# Same portfolio but NO target — only the base multiplier * tmp_measure term.
portf_to_notgt <- portfolio.spec(assets = colnames(R5))
portf_to_notgt <- add.constraint(portf_to_notgt, type = "weight_sum",
                                  min_sum = 0.99, max_sum = 1.01)
portf_to_notgt <- add.constraint(portf_to_notgt, type = "box", min = 0, max = 1)
portf_to_notgt <- add.objective(portf_to_notgt,
  type       = "turnover",
  name       = "turnover",
  multiplier = 1          # no target
)

# Portfolio with turnover target = 0.0 so any non-zero turnover is penalised.
portf_to_tgt0 <- portfolio.spec(assets = colnames(R5))
portf_to_tgt0 <- add.constraint(portf_to_tgt0, type = "weight_sum",
                                  min_sum = 0.99, max_sum = 1.01)
portf_to_tgt0 <- add.constraint(portf_to_tgt0, type = "box", min = 0, max = 1)
portf_to_tgt0 <- add.objective(portf_to_tgt0,
  type       = "turnover",
  name       = "turnover",
  target     = 0.0,   # zero-turnover target
  multiplier = 1
)

# Concentrated weights produce large (non-zero) turnover vs equal-weight baseline.
w_conc <- c(0.80, 0.05, 0.05, 0.05, 0.05)

test_that("turnover_objective with target: returns numeric scalar", {
  result <- constrained_objective(
    w = w_conc, R = R5, portfolio = portf_to_tgt,
    trace = FALSE, normalize = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1L)
})

test_that("turnover_objective with target: penalty branch produces larger out than no-target", {
  # With a numeric target (0.3) the concentrated weights deviate by |0.24-0.3|=0.06.
  # The penalty term `penalty * |multiplier| * |tmp_measure - target|` is added
  # on top of the base `|multiplier| * tmp_measure` term.  Without a target only
  # the base term is added.  => with-target result must be strictly larger.
  result_tgt   <- constrained_objective(
    w = w_conc, R = R5, portfolio = portf_to_tgt,
    trace = FALSE, normalize = FALSE
  )
  result_notgt <- constrained_objective(
    w = w_conc, R = R5, portfolio = portf_to_notgt,
    trace = FALSE, normalize = FALSE
  )
  expect_gt(result_tgt, result_notgt)
})

test_that("turnover_objective with target: trace=TRUE includes turnover in objective_measures", {
  result <- constrained_objective(
    w = w_conc, R = R5, portfolio = portf_to_tgt,
    trace = TRUE, normalize = FALSE
  )
  expect_true("turnover" %in% names(result$objective_measures))
})

test_that("turnover_objective with target=0: concentrated weights produce higher out than equal", {
  # target = 0: equal weights (turnover=0) meet the target exactly so only the
  # base multiplier*0 = 0 term is added.  Concentrated weights (turnover=0.24)
  # deviate by 0.24 => penalty = 1e4 * 0.24 added on top of base 0.24.
  # Concentrated out is unambiguously larger.
  result_eq   <- constrained_objective(
    w = w5, R = R5, portfolio = portf_to_tgt0,
    trace = FALSE, normalize = FALSE
  )
  result_conc <- constrained_objective(
    w = w_conc, R = R5, portfolio = portf_to_tgt0,
    trace = FALSE, normalize = FALSE
  )
  expect_gt(result_conc, result_eq)
})


# ===========================================================================
# Gap 2: risk_budget_objective with min_concentration=TRUE
#
# The min_concentration path (lines 722-728) uses the Herfindahl index of
# percent risk contributions.  It is separate from min_difference=TRUE which
# uses sqrt(sum(prc^2)).  A concentrated portfolio should incur a higher
# HHI penalty than an equal-weight portfolio.
# ===========================================================================

portf_rb_mc <- portfolio.spec(assets = colnames(R5))
portf_rb_mc <- add.constraint(portf_rb_mc, type = "full_investment")
portf_rb_mc <- add.constraint(portf_rb_mc, type = "long_only")
portf_rb_mc <- add.objective(portf_rb_mc,
  type              = "risk_budget",
  name              = "ES",
  arguments         = list(p = 0.95),
  min_concentration = TRUE
)

test_that("risk_budget min_concentration=TRUE: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_mc, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1L)
})

test_that("risk_budget min_concentration=TRUE: trace=TRUE includes ES in objective_measures", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_mc, trace = TRUE
  )
  expect_true("ES" %in% names(result$objective_measures))
})

test_that("risk_budget min_concentration=TRUE: concentrated weights produce higher out", {
  # A portfolio concentrated in one asset has a much higher HHI of risk
  # contributions than an equal-weight portfolio.
  w_conc2 <- c(0.96, 0.01, 0.01, 0.01, 0.01)
  result_conc <- constrained_objective(
    w = w_conc2, R = R5, portfolio = portf_rb_mc, trace = FALSE
  )
  result_eq <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_mc, trace = FALSE
  )
  expect_gt(result_conc, result_eq)
})

test_that("risk_budget min_concentration=TRUE: result is non-negative", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_mc, trace = FALSE
  )
  expect_gte(result, 0)
})


# ===========================================================================
# Gap 3: constrained_objective_v2 normalize=FALSE — under-sum penalty
#
# Line ~402: if(sum(w) < min_sum) out += penalty * (min_sum - sum(w))
# Trigger: weights that sum to less than min_sum=0.99 while normalize=FALSE.
# The over-sum branch (sum > max_sum) was tested in prior test files;
# this file covers the under-sum branch.
# ===========================================================================

portf_v2_norm_false <- portfolio.spec(assets = colnames(R5))
portf_v2_norm_false <- add.constraint(portf_v2_norm_false, type = "weight_sum",
                                       min_sum = 0.99, max_sum = 1.01)
portf_v2_norm_false <- add.constraint(portf_v2_norm_false, type = "box",
                                       min = 0, max = 1)
portf_v2_norm_false <- add.objective(portf_v2_norm_false,
  type = "risk", name = "StdDev"
)

# Weights that sum to 0.50 — well below min_sum=0.99.
w_undersum <- rep(0.10, 5)   # sum = 0.50

# Weights that are within [min_sum, max_sum] — no sum penalty.
w_ok <- rep(0.2, 5)          # sum = 1.00

test_that("normalize=FALSE under-sum: returns numeric scalar", {
  result <- constrained_objective(
    w = w_undersum, R = R5, portfolio = portf_v2_norm_false,
    trace = FALSE, normalize = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1L)
})

test_that("normalize=FALSE under-sum: incurs higher out than in-range weights", {
  result_under <- constrained_objective(
    w = w_undersum, R = R5, portfolio = portf_v2_norm_false,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_ok, R = R5, portfolio = portf_v2_norm_false,
    trace = FALSE, normalize = FALSE
  )
  expect_gt(result_under, result_ok)
})

test_that("normalize=FALSE under-sum: penalty scales with violation size", {
  # Smaller sum => larger violation => larger penalty.
  w_tiny <- rep(0.02, 5)      # sum = 0.10 => much larger under-sum violation
  result_tiny <- constrained_objective(
    w = w_tiny, R = R5, portfolio = portf_v2_norm_false,
    trace = FALSE, normalize = FALSE
  )
  result_under <- constrained_objective(
    w = w_undersum, R = R5, portfolio = portf_v2_norm_false,
    trace = FALSE, normalize = FALSE
  )
  expect_gt(result_tiny, result_under)
})


# ===========================================================================
# Gap 4: CSM empty switch arm — BUG-6 fixed
#
# In constrained_objective_v2, the switch on objective$name has an empty arm
# for "CSM" (line ~605):
#
#   CSM = {}, ## xinran
#
# Fix applied: `fun <- NULL` is initialised before the switch, and the
# `do.call` is guarded with `if (is.function(fun)) { ... } else { next }`.
# This means a CSM objective is silently skipped (contributes 0 to `out`)
# rather than throwing an error.
#
# Test updated to verify the fixed behavior: call succeeds and returns a
# finite numeric.
# ===========================================================================

portf_csm <- portfolio.spec(assets = colnames(R5))
portf_csm <- add.constraint(portf_csm, type = "full_investment")
portf_csm <- add.constraint(portf_csm, type = "long_only")
# Manually inject a portfolio_risk_objective named "CSM".
portf_csm$objectives <- list(
  structure(
    list(
      name       = "CSM",
      target     = NULL,
      arguments  = list(),
      multiplier = 1,
      enabled    = TRUE
    ),
    class = c("portfolio_risk_objective", "objective")
  )
)

test_that("CSM objective name [BUG-6 fixed]: constrained_objective succeeds and returns numeric", {
  # BUG-6 is fixed: fun <- NULL before switch + do.call guarded by is.function(fun).
  # The CSM arm is silently skipped (contributes 0 to out).
  result <- constrained_objective(w = w5, R = R5, portfolio = portf_csm, trace = FALSE)
  expect_true(is.numeric(result))
  expect_true(is.finite(result))
})
