###############################################################################
# tests/testthat/test-constrained-objective-branches.R
#
# Source files hit: R/constrained_objective.R
#
# Targeted branches not yet covered by test-constrained-objective.R:
#
#   constrained_objective_v2 (== constrained_objective):
#     - minmax_objective: tmp_measure > objective$max  (penalty path)
#     - minmax_objective: tmp_measure < objective$min  (penalty path)
#     - risk_budget_objective: numeric target (line ~693)
#     - risk_budget_objective: min_difference=TRUE (line ~715)
#     - var objective alias (name="var") — tmp_objname substitution to "StdDev"
#
#   constrained_objective_v1:
#     - entire function exercised (v1 API)
#     - trace=TRUE list return
#     - normalize=FALSE penalty paths
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
R5  <- edhec[, 1:5]
w5  <- rep(0.2, 5)


# ===========================================================================
# 1. minmax_objective — penalty branches
# ===========================================================================

# Build a portfolio with a minmax objective on mean return.
# mean return of equal-weight edhec[,1:5] is roughly 0.006 per month.
# Set max = 0.001 so tmp_measure > max is triggered for equal weights.
portf_mm_over <- portfolio.spec(assets = colnames(R5))
portf_mm_over <- add.constraint(portf_mm_over, type = "full_investment")
portf_mm_over <- add.constraint(portf_mm_over, type = "long_only")
portf_mm_over$objectives <- list(
  minmax_objective(name = "mean", min = 0.0001, max = 0.001)
)

# Set min = 0.999 so tmp_measure < min is triggered.
portf_mm_under <- portfolio.spec(assets = colnames(R5))
portf_mm_under <- add.constraint(portf_mm_under, type = "full_investment")
portf_mm_under <- add.constraint(portf_mm_under, type = "long_only")
portf_mm_under$objectives <- list(
  minmax_objective(name = "mean", min = 0.999, max = 1.0)
)

# A portfolio where mean is comfortably inside [min, max] — no penalty.
portf_mm_ok <- portfolio.spec(assets = colnames(R5))
portf_mm_ok <- add.constraint(portf_mm_ok, type = "full_investment")
portf_mm_ok <- add.constraint(portf_mm_ok, type = "long_only")
portf_mm_ok$objectives <- list(
  minmax_objective(name = "mean", min = 0.001, max = 0.02)
)

test_that("minmax_objective: tmp_measure > max incurs penalty (higher out)", {
  result_over <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_over, trace = FALSE
  )
  result_ok <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_ok, trace = FALSE
  )
  expect_true(result_over > result_ok)
})

test_that("minmax_objective: tmp_measure > max returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_over, trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("minmax_objective: tmp_measure < min incurs penalty (higher out)", {
  result_under <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_under, trace = FALSE
  )
  result_ok <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_ok, trace = FALSE
  )
  expect_true(result_under > result_ok)
})

test_that("minmax_objective: tmp_measure < min returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_under, trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("minmax_objective: in-range measure returns numeric (no penalty)", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_ok, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("minmax_objective: trace=TRUE includes mean in objective_measures", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_ok, trace = TRUE
  )
  expect_true("mean" %in% names(result$objective_measures))
})


# ===========================================================================
# 2. risk_budget_objective with numeric target
# ===========================================================================

# Numeric target triggers the univariate penalty path at ~line 693.
portf_rb_tgt <- portfolio.spec(assets = colnames(R5))
portf_rb_tgt <- add.constraint(portf_rb_tgt, type = "full_investment")
portf_rb_tgt <- add.constraint(portf_rb_tgt, type = "long_only")
portf_rb_tgt <- add.objective(portf_rb_tgt,
  type = "risk_budget", name = "ES",
  arguments = list(p = 0.95),
  max_prisk = 0.5, target = 0  # impossible ES target => penalty
)

# Same but no target => only max_prisk penalty applies.
portf_rb_notgt <- portfolio.spec(assets = colnames(R5))
portf_rb_notgt <- add.constraint(portf_rb_notgt, type = "full_investment")
portf_rb_notgt <- add.constraint(portf_rb_notgt, type = "long_only")
portf_rb_notgt <- add.objective(portf_rb_notgt,
  type = "risk_budget", name = "ES",
  arguments = list(p = 0.95),
  max_prisk = 0.5
)

test_that("risk_budget with numeric target: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_tgt, trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("risk_budget with numeric target: higher out than no-target version", {
  result_tgt <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_tgt, trace = FALSE
  )
  result_notgt <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_notgt, trace = FALSE
  )
  expect_true(result_tgt > result_notgt)
})

test_that("risk_budget with numeric target: trace=TRUE includes ES in objective_measures", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_tgt, trace = TRUE
  )
  expect_true("ES" %in% names(result$objective_measures))
})


# ===========================================================================
# 3. risk_budget_objective with min_difference=TRUE
# ===========================================================================

portf_rb_md <- portfolio.spec(assets = colnames(R5))
portf_rb_md <- add.constraint(portf_rb_md, type = "full_investment")
portf_rb_md <- add.constraint(portf_rb_md, type = "long_only")
portf_rb_md <- add.objective(portf_rb_md,
  type = "risk_budget", name = "ES",
  arguments = list(p = 0.95),
  min_difference = TRUE
)

test_that("risk_budget min_difference=TRUE: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_md, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("risk_budget min_difference=TRUE: trace=TRUE includes ES", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_md, trace = TRUE
  )
  expect_true("ES" %in% names(result$objective_measures))
})

test_that("risk_budget min_difference=TRUE: concentrated weights produce higher out", {
  w_conc <- c(0.96, 0.01, 0.01, 0.01, 0.01)
  result_conc <- constrained_objective(
    w = w_conc, R = R5, portfolio = portf_rb_md, trace = FALSE
  )
  result_eq <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_md, trace = FALSE
  )
  # concentrated portfolio has more unequal risk contributions => higher penalty
  expect_true(result_conc > result_eq)
})


# ===========================================================================
# 4. "var" objective — tmp_objname substitution to "StdDev" in trace
# ===========================================================================

portf_var <- portfolio.spec(assets = colnames(R5))
portf_var <- add.constraint(portf_var, type = "full_investment")
portf_var <- add.constraint(portf_var, type = "long_only")
portf_var <- add.objective(portf_var, type = "risk", name = "var")

test_that("var objective: trace=TRUE renames 'var' to 'StdDev' in objective_measures", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_var, trace = TRUE
  )
  expect_true("StdDev" %in% names(result$objective_measures))
  expect_false("var" %in% names(result$objective_measures))
})

test_that("var objective: constrained_objective returns positive numeric scalar", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_var, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_true(result > 0)
})


# ===========================================================================
# 5. constrained_objective_v1 — v1 API
# ===========================================================================

# Build a v1_constraint object using constraint_v1() — the original v1 constructor
v1_constr <- constraint_v1(assets = colnames(R5), min = 0, max = 1,
                            min_sum = 0.99, max_sum = 1.01,
                            weight_seq = generatesequence())
v1_constr <- add.objective_v1(v1_constr, type = "risk", name = "StdDev")

test_that("constrained_objective_v1: returns numeric scalar (trace=FALSE)", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("constrained_objective_v1: result is non-negative", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr, trace = FALSE
  )
  expect_true(result >= 0)
})

test_that("constrained_objective_v1: trace=TRUE returns a list", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr, trace = TRUE
  )
  expect_true(is.list(result))
})

test_that("constrained_objective_v1: trace=TRUE list has out, weights, objective_measures", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr, trace = TRUE
  )
  expect_true(!is.null(result$out))
  expect_true(!is.null(result$weights))
  expect_true(!is.null(result$objective_measures))
})

test_that("constrained_objective_v1: normalize=FALSE returns numeric scalar", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr,
    trace = FALSE, normalize = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("constrained_objective_v1: errors on non-constraint input", {
  expect_error(
    constrained_objective_v1(w = w5, R = R5, constraints = list()),
    regexp = "constraint"
  )
})

test_that("constrained_objective_v1: normalize=FALSE over-sum penalty works", {
  w_over <- rep(0.3, 5) # sum = 1.5 > max_sum = 1.01
  result_over <- constrained_objective_v1(
    w = w_over, R = R5, constraints = v1_constr,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_over > result_ok)
})
