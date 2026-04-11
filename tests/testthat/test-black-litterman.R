###############################################################################
# tests/testthat/test-black-litterman.R
#
# Source files hit: R/black_litterman.R
#
# Functions covered:
#   BlackLittermanFormula()   — direct posterior-moment calculation
#   black.litterman()         — wrapper: Mu=NULL, Sigma=NULL, Views=NULL,
#                               explicit args, and error branches
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

set.seed(42)
N  <- 4L
TT <- 60L
R  <- matrix(rnorm(TT * N, mean = 5e-3, sd = 2e-2), nrow = TT, ncol = N)
colnames(R) <- paste0("A", seq_len(N))

Mu_true    <- colMeans(R)
Sigma_true <- cov(R)

# Simple 1 x N equal-weight pick matrix
P1 <- matrix(rep(1 / N, N), nrow = 1)

# Two-view pick matrix
P2 <- rbind(
  c(1, -1,  0,  0),   # view: A1 outperforms A2
  c(0,  0,  1, -1)    # view: A3 outperforms A4
)

Omega1 <- tcrossprod(P1 %*% Sigma_true, P1)
v1     <- as.numeric(sqrt(diag(Omega1)))

Omega2 <- tcrossprod(P2 %*% Sigma_true, P2)
v2     <- as.numeric(sqrt(diag(Omega2)))


# ===========================================================================
# 1. BlackLittermanFormula — direct formula tests
# ===========================================================================

bl_formula_1view <- PortfolioAnalytics:::BlackLittermanFormula(
  Mu_true, Sigma_true, P1, v1, Omega1
)

test_that("BlackLittermanFormula: returns a list", {
  expect_true(is.list(bl_formula_1view))
})

test_that("BlackLittermanFormula: list has BLMu and BLSigma", {
  expect_true(!is.null(bl_formula_1view$BLMu))
  expect_true(!is.null(bl_formula_1view$BLSigma))
})

test_that("BlackLittermanFormula: BLMu is a numeric vector of length N", {
  expect_equal(length(as.numeric(bl_formula_1view$BLMu)), N)
})

test_that("BlackLittermanFormula: BLSigma is an N x N matrix", {
  expect_true(is.matrix(bl_formula_1view$BLSigma))
  expect_equal(dim(bl_formula_1view$BLSigma), c(N, N))
})

test_that("BlackLittermanFormula: BLSigma is symmetric", {
  expect_true(isSymmetric(bl_formula_1view$BLSigma, tol = 1e-10))
})

test_that("BlackLittermanFormula: BLMu values differ from prior Mu", {
  # Posterior should be influenced by views
  expect_false(isTRUE(all.equal(as.numeric(bl_formula_1view$BLMu), Mu_true)))
})

test_that("BlackLittermanFormula: BLSigma differs from prior Sigma (posterior shrinks)", {
  # BL update: Sigma_posterior = Sigma - Sigma P' (P Sigma P' + Omega)^{-1} P Sigma
  expect_false(isTRUE(all.equal(bl_formula_1view$BLSigma, Sigma_true)))
})

# Two-view version
bl_formula_2view <- PortfolioAnalytics:::BlackLittermanFormula(
  Mu_true, Sigma_true, P2, v2, Omega2
)

test_that("BlackLittermanFormula 2-view: BLMu has length N", {
  expect_equal(length(as.numeric(bl_formula_2view$BLMu)), N)
})

test_that("BlackLittermanFormula 2-view: BLSigma is N x N", {
  expect_equal(dim(bl_formula_2view$BLSigma), c(N, N))
})


# ===========================================================================
# 2. black.litterman — Mu=NULL, Sigma=NULL (sample estimates used)
# ===========================================================================

bl_default <- black.litterman(R, P1)

test_that("black.litterman Mu=NULL Sigma=NULL: returns a list", {
  expect_true(is.list(bl_default))
})

test_that("black.litterman Mu=NULL Sigma=NULL: has BLMu and BLSigma", {
  expect_true(!is.null(bl_default$BLMu))
  expect_true(!is.null(bl_default$BLSigma))
})

test_that("black.litterman Mu=NULL Sigma=NULL: BLMu has length N", {
  expect_equal(length(as.numeric(bl_default$BLMu)), N)
})

test_that("black.litterman Mu=NULL Sigma=NULL: BLSigma is N x N", {
  expect_equal(dim(bl_default$BLSigma), c(N, N))
})

test_that("black.litterman Mu=NULL Sigma=NULL: BLSigma is symmetric", {
  expect_true(isSymmetric(bl_default$BLSigma, tol = 1e-10))
})

# Result should match BlackLittermanFormula called with sample estimates
bl_manual <- PortfolioAnalytics:::BlackLittermanFormula(
  Mu    = colMeans(R),
  Sigma = cov(R),
  P     = P1,
  v     = as.numeric(sqrt(diag(tcrossprod(P1 %*% cov(R), P1)))),
  Omega = tcrossprod(P1 %*% cov(R), P1)
)

test_that("black.litterman Mu=NULL: BLMu matches manual BlackLittermanFormula call", {
  expect_equal(as.numeric(bl_default$BLMu), as.numeric(bl_manual$BLMu),
               tolerance = 1e-12)
})

test_that("black.litterman Mu=NULL: BLSigma matches manual BlackLittermanFormula call", {
  expect_equal(bl_default$BLSigma, bl_manual$BLSigma, tolerance = 1e-12)
})


# ===========================================================================
# 3. black.litterman — explicit Mu and Sigma provided
# ===========================================================================

bl_explicit <- black.litterman(R, P1, Mu = Mu_true, Sigma = Sigma_true)

test_that("black.litterman explicit Mu+Sigma: BLMu has length N", {
  expect_equal(length(as.numeric(bl_explicit$BLMu)), N)
})

test_that("black.litterman explicit Mu+Sigma: BLSigma is N x N", {
  expect_equal(dim(bl_explicit$BLSigma), c(N, N))
})

test_that("black.litterman explicit Mu+Sigma: result equals direct formula call", {
  expect_equal(as.numeric(bl_explicit$BLMu),
               as.numeric(bl_formula_1view$BLMu), tolerance = 1e-12)
  expect_equal(bl_explicit$BLSigma, bl_formula_1view$BLSigma, tolerance = 1e-12)
})


# ===========================================================================
# 4. black.litterman — explicit Views provided
# ===========================================================================

custom_views <- rep(1e-3, nrow(P1))
bl_views <- black.litterman(R, P1, Views = custom_views)

test_that("black.litterman explicit Views: BLMu has length N", {
  expect_equal(length(as.numeric(bl_views$BLMu)), N)
})

test_that("black.litterman explicit Views: BLSigma is N x N", {
  expect_equal(dim(bl_views$BLSigma), c(N, N))
})

test_that("black.litterman explicit Views: BLMu differs from default-Views result", {
  # Different views → different posterior mean
  expect_false(isTRUE(all.equal(as.numeric(bl_views$BLMu),
                                as.numeric(bl_default$BLMu))))
})


# ===========================================================================
# 5. black.litterman — two-view pick matrix
# ===========================================================================

bl_2view <- black.litterman(R, P2)

test_that("black.litterman 2-view: BLMu has length N", {
  expect_equal(length(as.numeric(bl_2view$BLMu)), N)
})

test_that("black.litterman 2-view: BLSigma is N x N and symmetric", {
  expect_equal(dim(bl_2view$BLSigma), c(N, N))
  expect_true(isSymmetric(bl_2view$BLSigma, tol = 1e-10))
})


# ===========================================================================
# 6. black.litterman — error branches
# ===========================================================================

test_that("black.litterman: error when length(Mu) != NCOL(R)", {
  expect_error(
    black.litterman(R, P1, Mu = c(0.001, 0.002)),
    "length of Mu must equal"
  )
})

test_that("black.litterman: error when dim(Sigma) wrong", {
  bad_sigma <- matrix(1:9, 3, 3)
  expect_error(
    black.litterman(R, P1, Sigma = bad_sigma),
    "dimensions of Sigma must equal"
  )
})

test_that("black.litterman: error for non-square Sigma", {
  non_sq <- matrix(0, nrow = N, ncol = N + 1)
  expect_error(
    black.litterman(R, P1, Sigma = non_sq),
    "dimensions of Sigma must equal"
  )
})
