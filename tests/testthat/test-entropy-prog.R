###############################################################################
# tests/testthat/test-entropy-prog.R
#
# Source files hit: R/EntropyProg.R
#
# Functions covered:
#   EntropyProg()  — equality-only branch (K_==0)
#                  — inequality+equality branch (K_>0)
#                  — verbose=TRUE path
#                  — error: no constraints (K_+K==0)
#                  — error: p does not sum to 1
#                  — error: Aeq/beq row mismatch
#                  — error: A/b row mismatch
#   pHist()        — basic call (returns f and x)
#                  — freq=TRUE path
#
# Requires nloptr.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("nloptr")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Shared fixtures
#
# Use J=5 equally-spaced scenarios and a uniform prior.
# The "sum-to-1" constraint (Aeq = 1', beq = 1) is always feasible and
# has an analytic solution: p* = p0 (prior is already a valid distribution).
# A second equality constraint pinning E[X] = 0 (the prior mean) is
# likewise trivially feasible and tests the multi-constraint path.
# ---------------------------------------------------------------------------

J   <- 5L
p0  <- rep(1 / J, J)
X   <- c(-0.02, -0.01, 0.00, 0.01, 0.02)   # symmetric — prior mean = 0

# Single equality: probabilities sum to 1
Aeq1 <- matrix(rep(1, J), nrow = 1)
beq1 <- matrix(1, nrow = 1)

# Two equalities: sum=1 AND E[X]=0 (prior mean)
Aeq2 <- rbind(rep(1, J), X)
beq2 <- matrix(c(1, 0), nrow = 2)


# ===========================================================================
# 1. Error paths
# ===========================================================================

test_that("EntropyProg: error when no constraints provided (K_+K == 0)", {
  expect_error(
    EntropyProg(p = p0, A = NULL, b = NULL,
                Aeq = matrix(nrow = 0, ncol = J),
                beq = matrix(nrow = 0, ncol = 1)),
    "at least one equality or inequality constraint"
  )
})

test_that("EntropyProg: error when p does not sum to 1", {
  bad_p <- rep(0.05, J)   # sums to 0.25
  expect_error(
    EntropyProg(p = bad_p, A = NULL, b = NULL,
                Aeq = Aeq1, beq = beq1),
    "sum of probabilities"
  )
})

test_that("EntropyProg: error when Aeq and beq row counts differ", {
  bad_beq <- matrix(c(1, 0), nrow = 2)  # 2 rows but Aeq1 has 1 row
  expect_error(
    EntropyProg(p = p0, A = NULL, b = NULL,
                Aeq = Aeq1, beq = bad_beq),
    "number of inequality constraints in matrix Aeq"
  )
})

test_that("EntropyProg: error when A and b row counts differ", {
  A_bad <- matrix(X, nrow = 1)
  b_bad <- matrix(c(0, 0.01), nrow = 2)  # 2 rows but A has 1 row
  expect_error(
    EntropyProg(p = p0, A = A_bad, b = b_bad,
                Aeq = Aeq1, beq = beq1),
    "number of equality constraints in matrix A"
  )
})


# ===========================================================================
# 2. Equality-only branch (K_ == 0, single constraint)
# ===========================================================================

res_eq1 <- EntropyProg(p = p0, A = NULL, b = NULL,
                       Aeq = Aeq1, beq = beq1)

test_that("EntropyProg equality-only: returns a list", {
  expect_true(is.list(res_eq1))
})

test_that("EntropyProg equality-only: list has p_ and optimizationPerformance", {
  expect_false(is.null(res_eq1$p_))
  expect_false(is.null(res_eq1$optimizationPerformance))
})

test_that("EntropyProg equality-only: p_ has length J", {
  expect_equal(length(as.numeric(res_eq1$p_)), J)
})

test_that("EntropyProg equality-only: p_ sums to ~1", {
  expect_equal(sum(res_eq1$p_), 1, tolerance = 1e-3)
})

test_that("EntropyProg equality-only: p_ entries are non-negative", {
  expect_true(all(res_eq1$p_ >= 0))
})

test_that("EntropyProg equality-only: sum constraint satisfied", {
  achieved <- as.numeric(Aeq1 %*% res_eq1$p_)
  expect_equal(achieved, 1, tolerance = 1e-4)
})

test_that("EntropyProg equality-only: converged is TRUE", {
  expect_true(res_eq1$optimizationPerformance$converged)
})

test_that("EntropyProg equality-only: optimizationPerformance$sumOfProbabilities ~ 1", {
  expect_equal(res_eq1$optimizationPerformance$sumOfProbabilities, 1, tolerance = 1e-3)
})


# ===========================================================================
# 3. Equality-only branch — two constraints, verbose=TRUE
# ===========================================================================

res_eq2 <- EntropyProg(p = p0, A = NULL, b = NULL,
                       Aeq = Aeq2, beq = beq2)

test_that("EntropyProg two-equality: p_ sums to ~1", {
  expect_equal(sum(res_eq2$p_), 1, tolerance = 1e-3)
})

test_that("EntropyProg two-equality: both constraints satisfied", {
  achieved <- as.numeric(Aeq2 %*% res_eq2$p_)
  expect_equal(achieved[1], 1, tolerance = 1e-4)   # sum = 1
  expect_equal(achieved[2], 0, tolerance = 1e-4)   # E[X] = 0
})

test_that("EntropyProg verbose=TRUE: produces output and returns valid list", {
  res_v <- capture.output(
    result <- EntropyProg(p = p0, A = NULL, b = NULL,
                          Aeq = Aeq1, beq = beq1,
                          verbose = TRUE)
  )
  expect_true(is.list(result))
  expect_equal(sum(result$p_), 1, tolerance = 1e-3)
  # verbose mode should produce some printed output
  expect_true(length(res_v) > 0)
})


# ===========================================================================
# 4. Inequality + equality branch (K_ > 0)
#
# Inequality: E[X] >= -0.005  i.e.  -E[X] <= 0.005
# Equality:   sum(p) = 1
# ===========================================================================

A_ineq <- matrix(-X, nrow = 1)     # -E[X] <= 0.005
b_ineq <- matrix(0.005, nrow = 1)

res_ineq <- EntropyProg(p = p0, A = A_ineq, b = b_ineq,
                        Aeq = Aeq1, beq = beq1)

test_that("EntropyProg ineq+eq: returns a list", {
  expect_true(is.list(res_ineq))
})

test_that("EntropyProg ineq+eq: p_ has length J", {
  expect_equal(length(as.numeric(res_ineq$p_)), J)
})

test_that("EntropyProg ineq+eq: p_ sums to ~1", {
  expect_equal(sum(res_ineq$p_), 1, tolerance = 1e-3)
})

test_that("EntropyProg ineq+eq: p_ entries are non-negative", {
  expect_true(all(res_ineq$p_ >= 0))
})

test_that("EntropyProg ineq+eq: equality constraint satisfied (sum = 1)", {
  expect_equal(as.numeric(Aeq1 %*% res_ineq$p_), 1, tolerance = 1e-4)
})

test_that("EntropyProg ineq+eq: inequality constraint satisfied (E[X] >= -0.005)", {
  achieved_mean <- as.numeric(t(X) %*% res_ineq$p_)
  expect_true(achieved_mean >= -0.005 - 1e-4)
})


# ===========================================================================
# 5. pHist() — basic call (produces a base graphics plot)
# ===========================================================================

test_that("pHist: returns a list with f and x elements", {
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)
  res_ph <- PortfolioAnalytics:::pHist(X = X,
                                       p = as.numeric(res_eq1$p_),
                                       nBins = 4)
  expect_true(is.list(res_ph))
  expect_false(is.null(res_ph$f))
  expect_false(is.null(res_ph$x))
})

test_that("pHist: f and x have equal length", {
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)
  res_ph <- PortfolioAnalytics:::pHist(X = X,
                                       p = as.numeric(res_eq1$p_),
                                       nBins = 4)
  expect_equal(length(res_ph$f), length(res_ph$x))
})

test_that("pHist: freq=TRUE runs without error", {
  pdf(file = NULL)
  on.exit(dev.off(), add = TRUE)
  res_ph <- PortfolioAnalytics:::pHist(X = X,
                                       p = as.numeric(res_eq1$p_),
                                       nBins = 4,
                                       freq = TRUE)
  expect_true(is.list(res_ph))
})
