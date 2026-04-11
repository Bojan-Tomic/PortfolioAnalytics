###############################################################################
# tests/testthat/test-stat-factor-model-extra.R
#
# Additional tests targeting previously uncovered branches in
# R/stat.factor.model.R, pushing coverage from ~86.5% toward ~93%.
#
# Targeted lines / branches:
#   Lines 39-41  : statistical.factor.model() — non-xts coercible input (happy
#                  path) and non-coercible input (try-error path)
#   Line  52     : k <= 0 guard in statistical.factor.model()
#   Lines 252-256: .residualcokurtosisSF() — NN coercion, mfactorM2 coercion,
#                  and all three length-mismatch error guards
#   Lines 298-299: covarianceMF() — wrong factorM2 dimension error
#   Lines 344-346: coskewnessMF() — wrong factorM3 dimension error
#   Lines 405-406: cokurtosisMF() — wrong factorM2 dimension error
#   Lines 411-412: cokurtosisMF() — wrong factorM4 dimension error
#   Lines 436-439: .residualcokurtosisMF() — NN coercion and all three
#                  length-mismatch error guards
#   Line  458    : extractCovariance() — wrong class error guard
#   Lines 481-483: extractCovariance() — invalid k (k == 0) → message + NULL
#   Line  500    : extractCoskewness() — wrong class error guard
#   Lines 527-529: extractCoskewness() — invalid k → message + NULL
#   Line  546    : extractCokurtosis() — wrong class error guard
#   Lines 575-577: extractCokurtosis() — invalid k → message + NULL
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

# ---------------------------------------------------------------------------
# File-scope fixtures — built once, reused across all test_that() blocks.
# ---------------------------------------------------------------------------

# Small synthetic dataset: m=20 obs, N=3 assets (m > N required by the guard)
.N_extra  <- 3L
.m_extra  <- 20L
set.seed(99)
.R_raw    <- matrix(rnorm(.m_extra * .N_extra, mean = 0.001, sd = 0.02),
                    nrow = .m_extra, ncol = .N_extra)
.dates    <- seq(as.Date("2015-01-01"), by = "month", length.out = .m_extra)
.R_xts    <- xts::xts(.R_raw, order.by = .dates)
colnames(.R_xts) <- paste0("X", seq_len(.N_extra))

# k=2 model for multi-factor helper checks
.sfm2_extra <- statistical.factor.model(.R_xts, k = 2L)

# Moment ingredients for k=2 (same formulas as extract* internals)
.beta2    <- .sfm2_extra$factor_loadings     # N x k matrix
.f2       <- .sfm2_extra$factor_realizations # m x k matrix
.res2     <- .sfm2_extra$residuals           # m x N matrix
.k2       <- .sfm2_extra$k                  # 2L
.denom2   <- .m_extra - .k2 - 1L
.stockM2  <- colSums(.res2^2) / .denom2      # length N
.stockM4  <- colSums(.res2^4) / .denom2      # length N
.factorM2 <- cov(.f2)                        # k x k
.factorM4 <- PerformanceAnalytics::M4.MM(.f2) # k x k^3

# Pre-build an 'stfm' object with k=0 to trigger the "invalid k" branches
# inside extractCovariance / extractCoskewness / extractCokurtosis.
# (The constructor rejects k<=0, so we craft it by hand.)
.sfm_badk <- structure(
  list(
    factor_loadings    = matrix(0, nrow = .N_extra, ncol = 1L),
    factor_realizations = matrix(0, nrow = .m_extra, ncol = 1L),
    residuals          = matrix(0, nrow = .m_extra, ncol = .N_extra),
    m  = .m_extra,
    k  = 0L,
    N  = .N_extra
  ),
  class = "stfm"
)

# Helper: a minimal plain-matrix with date rownames (coercible to xts)
.make_coercible_matrix <- function(m = 20L, N = 3L) {
  set.seed(7)
  mat <- matrix(rnorm(m * N, 0, 0.01), nrow = m, ncol = N)
  rownames(mat) <- as.character(
    seq(as.Date("2000-01-01"), by = "month", length.out = m)
  )
  colnames(mat) <- paste0("P", seq_len(N))
  mat
}


# ===========================================================================
# statistical.factor.model() — non-xts input paths  (lines 39-41, 52)
# ===========================================================================

test_that("statistical.factor.model() accepts a plain matrix with date rownames (coercible to xts)", {
  skip_on_cran()
  mat <- .make_coercible_matrix()
  result <- statistical.factor.model(mat, k = 1L)
  expect_s3_class(result, "stfm")
  expect_equal(result$N, 3L)
})

test_that("statistical.factor.model() stops with informative error on non-coercible input", {
  skip_on_cran()
  # A plain matrix with no date index — as.xts() will fail → try-error → stop()
  bad <- matrix(rnorm(30), nrow = 10, ncol = 3)
  colnames(bad) <- c("A", "B", "C")
  expect_error(
    statistical.factor.model(bad, k = 1L),
    regexp = "coercible"
  )
})

test_that("statistical.factor.model() stops with informative error when given a character string", {
  skip_on_cran()
  expect_error(
    statistical.factor.model("not a matrix", k = 1L),
    regexp = "coercible"
  )
})

test_that("statistical.factor.model() errors when k == 0", {
  skip_on_cran()
  expect_error(
    statistical.factor.model(.R_xts, k = 0),
    regexp = "positive integer"
  )
})

test_that("statistical.factor.model() errors when k is negative", {
  skip_on_cran()
  expect_error(
    statistical.factor.model(.R_xts, k = -2),
    regexp = "positive integer"
  )
})


# ===========================================================================
# .residualcokurtosisSF() — internal helper  (lines 252-256)
# ===========================================================================

test_that(".residualcokurtosisSF() coerces numeric NN to integer without error", {
  skip_on_cran()
  NN  <- 3L
  sM2 <- rep(0.01, NN)
  sM4 <- rep(0.0001, NN)
  b   <- rep(0.5, NN)
  # Pass NN as a plain double (3.0) — should auto-coerce on line 252
  r   <- PortfolioAnalytics:::.residualcokurtosisSF(3.0, sM2, sM4, 0.04, b)
  expect_equal(dim(r), c(NN, NN^3))
})

test_that(".residualcokurtosisSF() coerces non-double mfactorM2 to double", {
  skip_on_cran()
  NN  <- 3L
  sM2 <- rep(0.01, NN)
  sM4 <- rep(0.0001, NN)
  b   <- rep(0.5, NN)
  # Pass mfactorM2 as an integer — should auto-coerce on line 255
  r   <- PortfolioAnalytics:::.residualcokurtosisSF(NN, sM2, sM4, 4L, b)
  expect_equal(dim(r), c(NN, NN^3))
})

test_that(".residualcokurtosisSF() errors when sstockM2 length != NN", {
  skip_on_cran()
  NN  <- 3L
  sM2 <- rep(0.01, NN)
  sM4 <- rep(0.0001, NN)
  b   <- rep(0.5, NN)
  expect_error(
    PortfolioAnalytics:::.residualcokurtosisSF(NN, sM2[-1L], sM4, 0.04, b),
    regexp = "sstockM2 must be a vector of length NN"
  )
})

test_that(".residualcokurtosisSF() errors when sstockM4 length != NN", {
  skip_on_cran()
  NN  <- 3L
  sM2 <- rep(0.01, NN)
  sM4 <- rep(0.0001, NN)
  b   <- rep(0.5, NN)
  expect_error(
    PortfolioAnalytics:::.residualcokurtosisSF(NN, sM2, sM4[-1L], 0.04, b),
    regexp = "sstockM4 must be a vector of length NN"
  )
})

test_that(".residualcokurtosisSF() errors when bbeta length != NN", {
  skip_on_cran()
  NN  <- 3L
  sM2 <- rep(0.01, NN)
  sM4 <- rep(0.0001, NN)
  b   <- rep(0.5, NN)
  expect_error(
    PortfolioAnalytics:::.residualcokurtosisSF(NN, sM2, sM4, 0.04, b[-1L]),
    regexp = "bbeta must be a vector of length NN"
  )
})


# ===========================================================================
# covarianceMF() — wrong factorM2 dimension  (lines 298-299)
# ===========================================================================

test_that("covarianceMF() errors when factorM2 has wrong dimensions for beta", {
  skip_on_cran()
  # beta is N x k=2; factorM2 should be 2x2 but we pass 3x3
  factorM2_bad <- matrix(rnorm(9L), nrow = 3L, ncol = 3L)
  expect_error(
    PortfolioAnalytics:::covarianceMF(.beta2, .stockM2, factorM2_bad),
    regexp = "dimensions do not match"
  )
})

test_that("covarianceMF() errors when factorM2 is square but wrong size", {
  skip_on_cran()
  # k=2 so factorM2 needs to be 2x2; pass a 1x1 matrix
  factorM2_bad <- matrix(1.0, nrow = 1L, ncol = 1L)
  expect_error(
    PortfolioAnalytics:::covarianceMF(.beta2, .stockM2, factorM2_bad),
    regexp = "dimensions do not match"
  )
})


# ===========================================================================
# coskewnessMF() — wrong factorM3 dimension  (lines 344-346)
# ===========================================================================

test_that("coskewnessMF() errors when factorM3 has wrong row count", {
  skip_on_cran()
  k <- .k2  # 2
  # factorM3 should be k x k^2 = 2 x 4; pass 3 x 4 (wrong rows)
  factorM3_bad <- matrix(rnorm(12L), nrow = 3L, ncol = 4L)
  stockM3      <- colSums(.res2^3) / .denom2
  expect_error(
    PortfolioAnalytics:::coskewnessMF(.beta2, stockM3, factorM3_bad),
    regexp = "dimensions do not match"
  )
})

test_that("coskewnessMF() errors when factorM3 has wrong column count", {
  skip_on_cran()
  k <- .k2  # 2
  # factorM3 should be k x k^2 = 2 x 4; pass 2 x 5 (wrong cols)
  factorM3_bad <- matrix(rnorm(10L), nrow = 2L, ncol = 5L)
  stockM3      <- colSums(.res2^3) / .denom2
  expect_error(
    PortfolioAnalytics:::coskewnessMF(.beta2, stockM3, factorM3_bad),
    regexp = "dimensions do not match"
  )
})


# ===========================================================================
# cokurtosisMF() — wrong factorM2 / factorM4 dimensions (lines 405-406, 411-412)
# ===========================================================================

test_that("cokurtosisMF() errors when factorM2 has wrong dimensions", {
  skip_on_cran()
  # factorM2 should be k x k = 2 x 2; pass 3 x 3
  factorM2_bad <- matrix(rnorm(9L), nrow = 3L, ncol = 3L)
  expect_error(
    PortfolioAnalytics:::cokurtosisMF(.beta2, .stockM2, .stockM4,
                                      factorM2_bad, .factorM4),
    regexp = "dimensions do not match"
  )
})

test_that("cokurtosisMF() errors when factorM4 has wrong dimensions", {
  skip_on_cran()
  k <- .k2  # 2
  # factorM4 should be k x k^3 = 2 x 8; pass 2 x 5 (wrong cols)
  factorM4_bad <- matrix(rnorm(10L), nrow = 2L, ncol = 5L)
  expect_error(
    PortfolioAnalytics:::cokurtosisMF(.beta2, .stockM2, .stockM4,
                                      .factorM2, factorM4_bad),
    regexp = "dimensions do not match"
  )
})

test_that("cokurtosisMF() errors when factorM4 has wrong row count", {
  skip_on_cran()
  k <- .k2  # 2
  # factorM4 should be k x k^3 = 2 x 8; pass 3 x 8 (wrong rows)
  factorM4_bad <- matrix(rnorm(24L), nrow = 3L, ncol = 8L)
  expect_error(
    PortfolioAnalytics:::cokurtosisMF(.beta2, .stockM2, .stockM4,
                                      .factorM2, factorM4_bad),
    regexp = "dimensions do not match"
  )
})


# ===========================================================================
# .residualcokurtosisMF() — internal helper  (lines 436-439)
# ===========================================================================

test_that(".residualcokurtosisMF() coerces numeric NN to integer without error", {
  skip_on_cran()
  NN      <- 3L
  sM2     <- rep(0.01, NN)
  sM4     <- rep(0.0001, NN)
  betacov <- rep(0.001, NN * NN)
  # Pass NN as 3.0 (double) — auto-coerced on line 436
  r <- PortfolioAnalytics:::.residualcokurtosisMF(3.0, sM2, sM4, betacov)
  expect_equal(dim(r), c(NN, NN^3))
})

test_that(".residualcokurtosisMF() errors when sstockM2 length != NN", {
  skip_on_cran()
  NN      <- 3L
  sM2     <- rep(0.01, NN)
  sM4     <- rep(0.0001, NN)
  betacov <- rep(0.001, NN * NN)
  expect_error(
    PortfolioAnalytics:::.residualcokurtosisMF(NN, sM2[-1L], sM4, betacov),
    regexp = "sstockM2 must be a vector of length NN"
  )
})

test_that(".residualcokurtosisMF() errors when sstockM4 length != NN", {
  skip_on_cran()
  NN      <- 3L
  sM2     <- rep(0.01, NN)
  sM4     <- rep(0.0001, NN)
  betacov <- rep(0.001, NN * NN)
  expect_error(
    PortfolioAnalytics:::.residualcokurtosisMF(NN, sM2, sM4[-1L], betacov),
    regexp = "sstockM4 must be a vector of length NN"
  )
})

test_that(".residualcokurtosisMF() errors when bbetacov length != NN*NN", {
  skip_on_cran()
  NN      <- 3L
  sM2     <- rep(0.01, NN)
  sM4     <- rep(0.0001, NN)
  betacov <- rep(0.001, NN * NN)
  expect_error(
    PortfolioAnalytics:::.residualcokurtosisMF(NN, sM2, sM4, betacov[-1L]),
    regexp = "bbetacov must be a vector of length NN\\*NN"
  )
})


# ===========================================================================
# extractCovariance() — class guard and invalid-k branch  (lines 458, 481-483)
# ===========================================================================

test_that("extractCovariance() errors when model is not of class 'stfm'", {
  skip_on_cran()
  not_stfm <- list(a = 1)
  expect_error(
    extractCovariance(not_stfm),
    regexp = "model must be of class 'stfm'"
  )
})

test_that("extractCovariance() emits a message and returns NULL when k == 0", {
  skip_on_cran()
  expect_message(
    out <- extractCovariance(.sfm_badk),
    regexp = "invalid k"
  )
  expect_null(out)
})


# ===========================================================================
# extractCoskewness() — class guard and invalid-k branch  (lines 500, 527-529)
# ===========================================================================

test_that("extractCoskewness() errors when model is not of class 'stfm'", {
  skip_on_cran()
  not_stfm <- list(a = 1)
  expect_error(
    extractCoskewness(not_stfm),
    regexp = "model must be of class 'stfm'"
  )
})

test_that("extractCoskewness() emits a message and returns NULL when k == 0", {
  skip_on_cran()
  expect_message(
    out <- extractCoskewness(.sfm_badk),
    regexp = "invalid k"
  )
  expect_null(out)
})


# ===========================================================================
# extractCokurtosis() — class guard and invalid-k branch  (lines 546, 575-577)
# ===========================================================================

test_that("extractCokurtosis() errors when model is not of class 'stfm'", {
  skip_on_cran()
  not_stfm <- list(a = 1)
  expect_error(
    extractCokurtosis(not_stfm),
    regexp = "model must be of class 'stfm'"
  )
})

test_that("extractCokurtosis() emits a message and returns NULL when k == 0", {
  skip_on_cran()
  expect_message(
    out <- extractCokurtosis(.sfm_badk),
    regexp = "invalid k"
  )
  expect_null(out)
})
