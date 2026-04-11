###############################################################################
# tests/testthat/test-constraints-roi.R
#
# Source file covered: R/constraints_ROI.R
#
#   constraint_ROI() — constructor for portfolios that bypass the normal
#   PortfolioAnalytics optimization pipeline and directly pass a pre-built
#   ROI OP object as the complete optimization problem.
#
# Tests cover:
#   1.  assets = integer scalar     — class, length, solver, auto-naming
#   2.  assets = character vector   — names preserved, equal-weight seed sums to 1
#   3.  assets = named numeric vec  — weights stored unchanged
#   4.  weight_seq argument         — slot is stored correctly
#   5.  OP object stored            — constrainted_objective slot is class OP
#   6.  default solver              — "glpk" when solver not specified
#   7.  error: op.problem = NULL
#   8.  error: assets = NULL
#   9.  error: op.problem not of class OP
#
# Shared fixtures from helper-portfolioanalytics.R (do NOT redefine):
#   edhec4 (edhec[, 1:4])
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# ---------------------------------------------------------------------------
# File-scope setup — runs once before any test_that() block.
# edhec is already loaded by helper-portfolioanalytics.R; we slice 4 columns
# to match the 4-asset OP object built below.
# ---------------------------------------------------------------------------

R     <- edhec[, 1:4]
N     <- ncol(R)          # 4
Sigma <- cov(R)

# Minimal quadratic program: min  w' Sigma w
#                            s.t. sum(w) == 1,  0 <= w <= 1
op <- ROI::OP(
  objective   = ROI::Q_objective(Q = 2 * Sigma, L = rep(0, N)),
  constraints = ROI::L_constraint(
    L   = matrix(rep(1, N), nrow = 1),
    dir = "==",
    rhs = 1
  ),
  bounds = ROI::V_bound(lb = rep(0, N), ub = rep(1, N))
)

# ===========================================================================
# 1. assets = integer scalar: class, length, solver, auto-naming
# ===========================================================================

test_that("constraint_ROI with integer scalar assets has correct class and structure", {
  result <- expect_output(
    constraint_ROI(assets = N, op.problem = op, solver = "quadprog"),
    "You chose"
  )
  expect_s3_class(result, "constraint_ROI")
  expect_s3_class(result, "constraint")
  expect_equal(length(result$assets), N)
  expect_equal(result$solver, "quadprog")
  expect_equal(names(result$assets), paste("Asset", seq_len(N), sep = "."))
})

# ===========================================================================
# 2. assets = character vector: names preserved, equal-weight seed sums to 1
# ===========================================================================

test_that("constraint_ROI with character asset names stores names and seed weights sum to 1", {
  result <- expect_output(
    constraint_ROI(assets = colnames(R), op.problem = op, solver = "quadprog"),
    "You chose"
  )
  expect_equal(names(result$assets), colnames(R))
  expect_equal(sum(result$assets), 1, tolerance = 1e-10)
})

# ===========================================================================
# 3. assets = named numeric vector: weights stored unchanged
# ===========================================================================

test_that("constraint_ROI with named numeric vector stores weights unchanged", {
  w        <- rep(1 / N, N)
  names(w) <- colnames(R)
  result   <- expect_output(
    constraint_ROI(assets = w, op.problem = op, solver = "quadprog"),
    "You chose"
  )
  expect_equal(result$assets, w)
})

# ===========================================================================
# 4. weight_seq argument is stored in the returned list
# ===========================================================================

test_that("constraint_ROI stores weight_seq in the returned object", {
  ws     <- seq(0, 1, by = 0.05)
  result <- expect_output(
    constraint_ROI(
      assets     = N,
      op.problem = op,
      solver     = "quadprog",
      weight_seq = ws
    ),
    "You chose"
  )
  expect_equal(result$weight_seq, ws)
})

# ===========================================================================
# 5. The OP object is stored in the constrainted_objective slot
# ===========================================================================

test_that("constraint_ROI stores the OP object in constrainted_objective", {
  result <- expect_output(
    constraint_ROI(assets = N, op.problem = op, solver = "quadprog"),
    "You chose"
  )
  expect_s3_class(result$constrainted_objective, "OP")
})

# ===========================================================================
# 6. Default solver is "glpk" when solver argument is omitted
# ===========================================================================

test_that("constraint_ROI defaults to glpk solver when solver is not specified", {
  skip_if_not_installed("ROI.plugin.glpk")
  library(ROI.plugin.glpk)
  result <- expect_output(
    constraint_ROI(assets = N, op.problem = op),
    "You chose"
  )
  expect_equal(result$solver, "glpk")
})

# ===========================================================================
# 7. Error: op.problem = NULL triggers "Need to pass in optimization problem"
# ===========================================================================

test_that("constraint_ROI errors when op.problem is NULL", {
  expect_error(
    constraint_ROI(assets = N, op.problem = NULL, solver = "quadprog"),
    "Need to pass in optimization problem"
  )
})

# ===========================================================================
# 8. Error: assets = NULL triggers "You must specify the assets"
# ===========================================================================

test_that("constraint_ROI errors when assets is NULL", {
  expect_error(
    constraint_ROI(assets = NULL, op.problem = op, solver = "quadprog"),
    "You must specify"
  )
})

# ===========================================================================
# 9. Error: op.problem is a plain list (not an OP object)
# ===========================================================================

test_that("constraint_ROI errors when op.problem is not an OP object", {
  expect_error(
    constraint_ROI(assets = N, op.problem = list(x = 1), solver = "quadprog"),
    "Need to pass in optimization problem"
  )
})
