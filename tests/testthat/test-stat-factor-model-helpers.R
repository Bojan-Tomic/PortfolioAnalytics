###############################################################################
# tests/testthat/test-stat-factor-model-helpers.R
#
# Tests for the lower-level single-factor and multi-factor comoment helpers
# in R/stat.factor.model.R that are not covered by test-stat-factor-model.R:
#
#   covarianceSF()     — covariance from single-factor model
#   coskewnessSF()     — coskewness from single-factor model
#   cokurtosisSF()     — cokurtosis from single-factor model
#   covarianceMF()     — covariance from multi-factor model
#   coskewnessMF()     — coskewness from multi-factor model
#   cokurtosisMF()     — cokurtosis from multi-factor model
#
# All functions are internal (not exported); accessed via `:::`.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Fixtures: derive inputs directly from statistical.factor.model() so that
# the low-level helpers can be verified to match extractCovariance() etc.,
# using exactly the same moment formulas as those wrappers.
# ---------------------------------------------------------------------------

N  <- 5L          # assets
m  <- 100L        # observations (m > N to avoid the guard)

set.seed(42)
R_raw  <- matrix(rnorm(m * N, mean = 0.001, sd = 0.02), nrow = m, ncol = N)
dates  <- seq(as.Date("2010-01-01"), by = "month", length.out = m)
R_xts  <- xts::xts(R_raw, order.by = dates)
colnames(R_xts) <- paste0("A", seq_len(N))

sfm1 <- statistical.factor.model(R_xts, k = 1)   # single-factor
sfm2 <- statistical.factor.model(R_xts, k = 2)   # two-factor

# --------------------------------------------------------------------------
# Helper: extract moment ingredients exactly as extractCovariance/Coskewness/
# Cokurtosis do internally (same formulas, so roundtrip tests are exact).
# --------------------------------------------------------------------------
.mk1 <- function(model) {
  beta    <- model$factor_loadings
  f       <- model$factor_realizations
  res     <- model$residuals
  m       <- model$m
  k       <- model$k
  denom   <- m - k - 1
  list(
    beta     = beta,
    stockM2  = colSums(res^2) / denom,
    stockM3  = colSums(res^3) / denom,
    stockM4  = colSums(res^4) / denom,
    factorM2 = as.numeric(cov(f)),
    factorM3 = as.numeric(PerformanceAnalytics::M3.MM(f)),
    factorM4 = as.numeric(PerformanceAnalytics::M4.MM(f))
  )
}

.mk2 <- function(model) {
  beta    <- model$factor_loadings
  f       <- model$factor_realizations
  res     <- model$residuals
  m       <- model$m
  k       <- model$k
  denom   <- m - k - 1
  list(
    beta     = beta,
    stockM2  = colSums(res^2) / denom,
    stockM3  = colSums(res^3) / denom,
    stockM4  = colSums(res^4) / denom,
    factorM2 = cov(f),
    factorM3 = PerformanceAnalytics::M3.MM(f),
    factorM4 = PerformanceAnalytics::M4.MM(f)
  )
}

i1 <- .mk1(sfm1)
i2 <- .mk2(sfm2)


# ===========================================================================
# covarianceSF()
# ===========================================================================

test_that("covarianceSF() returns an N x N symmetric matrix", {
  S <- PortfolioAnalytics:::covarianceSF(i1$beta, i1$stockM2, i1$factorM2)
  expect_true(is.matrix(S))
  expect_equal(dim(S), c(N, N))
  expect_true(isSymmetric(S, tol = 1e-10))
})

test_that("covarianceSF() result is positive semi-definite", {
  S    <- PortfolioAnalytics:::covarianceSF(i1$beta, i1$stockM2, i1$factorM2)
  eigs <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
  expect_true(min(eigs) >= -1e-8)
})

test_that("covarianceSF() diagonal equals systematic + idiosyncratic variance", {
  S   <- PortfolioAnalytics:::covarianceSF(i1$beta, i1$stockM2, i1$factorM2)
  b   <- as.numeric(i1$beta)
  sys <- b^2 * i1$factorM2
  expect_equal(diag(S), as.numeric(sys + i1$stockM2), tolerance = 1e-10)
})

test_that("covarianceSF() errors when beta and stockM2 lengths differ", {
  expect_error(
    PortfolioAnalytics:::covarianceSF(i1$beta, i1$stockM2[-1], i1$factorM2),
    regexp = "dimensions"
  )
})

test_that("covarianceSF() matches extractCovariance() for k=1", {
  S_low  <- PortfolioAnalytics:::covarianceSF(i1$beta, i1$stockM2, i1$factorM2)
  S_high <- extractCovariance(sfm1)
  expect_equal(S_low, S_high, tolerance = 1e-10)
})


# ===========================================================================
# coskewnessSF()
# ===========================================================================

test_that("coskewnessSF() returns an N x N^2 matrix", {
  M3 <- PortfolioAnalytics:::coskewnessSF(i1$beta, i1$stockM3, i1$factorM3)
  expect_true(is.matrix(M3))
  expect_equal(dim(M3), c(N, N^2))
})

test_that("coskewnessSF() errors when beta and stockM3 lengths differ", {
  expect_error(
    PortfolioAnalytics:::coskewnessSF(i1$beta, i1$stockM3[-1], i1$factorM3),
    regexp = "dimensions"
  )
})

test_that("coskewnessSF() result is numeric", {
  M3 <- PortfolioAnalytics:::coskewnessSF(i1$beta, i1$stockM3, i1$factorM3)
  expect_true(is.numeric(M3))
})

test_that("coskewnessSF() matches extractCoskewness() for k=1", {
  M3_low  <- PortfolioAnalytics:::coskewnessSF(i1$beta, i1$stockM3, i1$factorM3)
  M3_high <- extractCoskewness(sfm1)
  expect_equal(M3_low, M3_high, tolerance = 1e-10)
})


# ===========================================================================
# cokurtosisSF()
# ===========================================================================

test_that("cokurtosisSF() returns an N x N^3 matrix", {
  M4 <- PortfolioAnalytics:::cokurtosisSF(i1$beta, i1$stockM2, i1$stockM4,
                                           i1$factorM2, i1$factorM4)
  expect_true(is.matrix(M4))
  expect_equal(dim(M4), c(N, N^3))
})

test_that("cokurtosisSF() errors when beta and stockM2 lengths differ", {
  expect_error(
    PortfolioAnalytics:::cokurtosisSF(i1$beta, i1$stockM2[-1], i1$stockM4,
                                      i1$factorM2, i1$factorM4),
    regexp = "dimensions"
  )
})

test_that("cokurtosisSF() errors when beta and stockM4 lengths differ", {
  expect_error(
    PortfolioAnalytics:::cokurtosisSF(i1$beta, i1$stockM2, i1$stockM4[-1],
                                      i1$factorM2, i1$factorM4),
    regexp = "dimensions"
  )
})

test_that("cokurtosisSF() result is numeric", {
  M4 <- PortfolioAnalytics:::cokurtosisSF(i1$beta, i1$stockM2, i1$stockM4,
                                           i1$factorM2, i1$factorM4)
  expect_true(is.numeric(M4))
})

test_that("cokurtosisSF() matches extractCokurtosis() for k=1", {
  M4_low  <- PortfolioAnalytics:::cokurtosisSF(i1$beta, i1$stockM2, i1$stockM4,
                                                i1$factorM2, i1$factorM4)
  M4_high <- extractCokurtosis(sfm1)
  expect_equal(M4_low, M4_high, tolerance = 1e-10)
})


# ===========================================================================
# covarianceMF()
# ===========================================================================

test_that("covarianceMF() returns an N x N symmetric matrix for k=2", {
  S <- PortfolioAnalytics:::covarianceMF(i2$beta, i2$stockM2, i2$factorM2)
  expect_true(is.matrix(S))
  expect_equal(dim(S), c(N, N))
  expect_true(isSymmetric(S, tol = 1e-10))
})

test_that("covarianceMF() result is positive semi-definite for k=2", {
  S    <- PortfolioAnalytics:::covarianceMF(i2$beta, i2$stockM2, i2$factorM2)
  eigs <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
  expect_true(min(eigs) >= -1e-8)
})

test_that("covarianceMF() errors when beta is not a matrix", {
  expect_error(
    PortfolioAnalytics:::covarianceMF(as.numeric(i2$beta), i2$stockM2, i2$factorM2),
    regexp = "matrix"
  )
})

test_that("covarianceMF() errors when factorM2 is not a matrix", {
  expect_error(
    PortfolioAnalytics:::covarianceMF(i2$beta, i2$stockM2, as.numeric(i2$factorM2)),
    regexp = "matrix"
  )
})

test_that("covarianceMF() errors when stockM2 length doesn't match N", {
  expect_error(
    PortfolioAnalytics:::covarianceMF(i2$beta, i2$stockM2[-1], i2$factorM2),
    regexp = "dimensions"
  )
})

test_that("covarianceMF() matches extractCovariance() for k=2", {
  S_low  <- PortfolioAnalytics:::covarianceMF(i2$beta, i2$stockM2, i2$factorM2)
  S_high <- extractCovariance(sfm2)
  expect_equal(S_low, S_high, tolerance = 1e-10)
})


# ===========================================================================
# coskewnessMF()
# ===========================================================================

test_that("coskewnessMF() returns an N x N^2 matrix for k=2", {
  M3 <- PortfolioAnalytics:::coskewnessMF(i2$beta, i2$stockM3, i2$factorM3)
  expect_true(is.matrix(M3))
  expect_equal(dim(M3), c(N, N^2))
})

test_that("coskewnessMF() errors when beta is not a matrix", {
  expect_error(
    PortfolioAnalytics:::coskewnessMF(as.numeric(i2$beta), i2$stockM3, i2$factorM3),
    regexp = "matrix"
  )
})

test_that("coskewnessMF() errors when factorM3 is not a matrix", {
  expect_error(
    PortfolioAnalytics:::coskewnessMF(i2$beta, i2$stockM3, as.numeric(i2$factorM3)),
    regexp = "matrix"
  )
})

test_that("coskewnessMF() errors when stockM3 length doesn't match N", {
  expect_error(
    PortfolioAnalytics:::coskewnessMF(i2$beta, i2$stockM3[-1], i2$factorM3),
    regexp = "dimensions"
  )
})

test_that("coskewnessMF() result is numeric", {
  M3 <- PortfolioAnalytics:::coskewnessMF(i2$beta, i2$stockM3, i2$factorM3)
  expect_true(is.numeric(M3))
})

test_that("coskewnessMF() matches extractCoskewness() for k=2", {
  M3_low  <- PortfolioAnalytics:::coskewnessMF(i2$beta, i2$stockM3, i2$factorM3)
  M3_high <- extractCoskewness(sfm2)
  expect_equal(M3_low, M3_high, tolerance = 1e-10)
})


# ===========================================================================
# cokurtosisMF()
# ===========================================================================

test_that("cokurtosisMF() returns an N x N^3 matrix for k=2", {
  M4 <- PortfolioAnalytics:::cokurtosisMF(i2$beta, i2$stockM2, i2$stockM4,
                                           i2$factorM2, i2$factorM4)
  expect_true(is.matrix(M4))
  expect_equal(dim(M4), c(N, N^3))
})

test_that("cokurtosisMF() errors when beta is not a matrix", {
  expect_error(
    PortfolioAnalytics:::cokurtosisMF(as.numeric(i2$beta), i2$stockM2, i2$stockM4,
                                      i2$factorM2, i2$factorM4),
    regexp = "matrix"
  )
})

test_that("cokurtosisMF() errors when stockM2 length doesn't match N", {
  expect_error(
    PortfolioAnalytics:::cokurtosisMF(i2$beta, i2$stockM2[-1], i2$stockM4,
                                      i2$factorM2, i2$factorM4),
    regexp = "dimensions"
  )
})

test_that("cokurtosisMF() errors when stockM4 length doesn't match N", {
  expect_error(
    PortfolioAnalytics:::cokurtosisMF(i2$beta, i2$stockM2, i2$stockM4[-1],
                                      i2$factorM2, i2$factorM4),
    regexp = "dimensions"
  )
})

test_that("cokurtosisMF() errors when factorM2 is not a matrix", {
  expect_error(
    PortfolioAnalytics:::cokurtosisMF(i2$beta, i2$stockM2, i2$stockM4,
                                      as.numeric(i2$factorM2), i2$factorM4),
    regexp = "matrix"
  )
})

test_that("cokurtosisMF() errors when factorM4 is not a matrix", {
  expect_error(
    PortfolioAnalytics:::cokurtosisMF(i2$beta, i2$stockM2, i2$stockM4,
                                      i2$factorM2, as.numeric(i2$factorM4)),
    regexp = "matrix"
  )
})

test_that("cokurtosisMF() result is numeric", {
  M4 <- PortfolioAnalytics:::cokurtosisMF(i2$beta, i2$stockM2, i2$stockM4,
                                           i2$factorM2, i2$factorM4)
  expect_true(is.numeric(M4))
})

test_that("cokurtosisMF() matches extractCokurtosis() for k=2", {
  M4_low  <- PortfolioAnalytics:::cokurtosisMF(i2$beta, i2$stockM2, i2$stockM4,
                                                i2$factorM2, i2$factorM4)
  M4_high <- extractCokurtosis(sfm2)
  expect_equal(M4_low, M4_high, tolerance = 1e-10)
})
