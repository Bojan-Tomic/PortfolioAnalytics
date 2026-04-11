###############################################################################
# tests/testthat/test-moment-functions.R
#
# Tests for R/moment.functions.R and R/black_litterman.R
#
# Covers:
#   set.portfolio.moments()   — sample, boudt, black_litterman, meucci methods
#   black.litterman()         — Black-Litterman posterior moments
#   BlackLittermanFormula()   — lower-level BL formula (via :::)
#   CCCgarch.MM()             — CCC-GARCH conditional moment estimates
#                               (requires fGarch; skipped gracefully if absent)
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# File-scope setup — runs once before any test_that() block.
# edhec5, make_minvar_portf(), make_mines_portf() are provided by
# helper-portfolioanalytics.R and do NOT need to be redefined here.
# ---------------------------------------------------------------------------

N <- ncol(edhec5)   # 5 assets

portf_var <- make_minvar_portf(edhec5)   # full_investment + long_only + StdDev
portf_es  <- make_mines_portf(edhec5)    # full_investment + long_only + ES

# Single-row equal-weight pick matrix used by black.litterman tests
P <- matrix(rep(1 / N, N), nrow = 1)

# Compute moments under each method once so test blocks stay fast and focused
moments_sample_var <- set.portfolio.moments(edhec5, portf_var, method = "sample")
moments_sample_es  <- set.portfolio.moments(edhec5, portf_es,  method = "sample")
moments_boudt_es   <- set.portfolio.moments(edhec5, portf_es,  method = "boudt")
moments_bl         <- set.portfolio.moments(edhec5, portf_var, method = "black_litterman")
moments_meucci     <- set.portfolio.moments(edhec5, portf_var, method = "meucci")

# Black-Litterman model called directly (not through set.portfolio.moments)
bl_result <- black.litterman(edhec5, P)


# ===========================================================================
# set.portfolio.moments() — sample method, StdDev objective
# ===========================================================================

test_that("set.portfolio.moments(method='sample') returns a list", {
  expect_true(is.list(moments_sample_var))
})

test_that("set.portfolio.moments(method='sample') result has mu and sigma elements", {
  expect_true(!is.null(moments_sample_var$mu),    label = "sample moments has mu")
  expect_true(!is.null(moments_sample_var$sigma), label = "sample moments has sigma")
})

test_that("sample mu has length N", {
  expect_equal(length(moments_sample_var$mu), N)
})

test_that("sample sigma is an N x N matrix", {
  sig <- moments_sample_var$sigma
  expect_true(is.matrix(sig))
  expect_equal(dim(sig), c(N, N))
})

test_that("sample mu equals colMeans(R)", {
  expect_equal(
    as.numeric(moments_sample_var$mu),
    as.numeric(colMeans(edhec5)),
    tolerance = 1e-10
  )
})

test_that("sample sigma equals cov(R, use='pairwise.complete.obs')", {
  expect_equal(
    moments_sample_var$sigma,
    cov(edhec5, use = "pairwise.complete.obs"),
    tolerance = 1e-10
  )
})

test_that("sample method with StdDev objective does not populate m3 or m4", {
  # Higher moments are not needed for a pure variance/StdDev objective
  expect_true(is.null(moments_sample_var$m3), label = "no m3 for StdDev objective")
  expect_true(is.null(moments_sample_var$m4), label = "no m4 for StdDev objective")
})

test_that("sample method with ES objective populates m3 and m4", {
  expect_true(!is.null(moments_sample_es$m3), label = "m3 present for ES objective")
  expect_true(!is.null(moments_sample_es$m4), label = "m4 present for ES objective")
})


# ===========================================================================
# set.portfolio.moments() — boudt method, ES objective (factor-model moments)
# ===========================================================================

test_that("set.portfolio.moments(method='boudt') returns a list with mu, sigma, m3, m4", {
  expect_true(is.list(moments_boudt_es))
  expect_true(!is.null(moments_boudt_es$mu),    label = "boudt ES has mu")
  expect_true(!is.null(moments_boudt_es$sigma), label = "boudt ES has sigma")
  expect_true(!is.null(moments_boudt_es$m3),    label = "boudt ES has m3")
  expect_true(!is.null(moments_boudt_es$m4),    label = "boudt ES has m4")
})

test_that("boudt sigma is an N x N matrix", {
  sig <- moments_boudt_es$sigma
  expect_true(is.matrix(sig))
  expect_equal(dim(sig), c(N, N))
})

test_that("boudt m3 has dimensions N x N^2", {
  m3 <- moments_boudt_es$m3
  expect_true(is.matrix(m3))
  expect_equal(nrow(m3), N)
  expect_equal(ncol(m3), N^2)
})

test_that("boudt m4 has dimensions N x N^3", {
  m4 <- moments_boudt_es$m4
  expect_true(is.matrix(m4))
  expect_equal(nrow(m4), N)
  expect_equal(ncol(m4), N^3)
})

test_that("boudt sigma differs from sample cov (factor model vs raw sample)", {
  # The statistical factor model estimate is not identical to sample covariance
  expect_false(
    isTRUE(all.equal(moments_boudt_es$sigma, cov(edhec5))),
    label = "boudt covariance is not equal to sample covariance"
  )
})


# ===========================================================================
# set.portfolio.moments() — black_litterman method
# ===========================================================================

test_that("set.portfolio.moments(method='black_litterman') returns a list", {
  expect_true(is.list(moments_bl))
})

test_that("black_litterman moments has mu and sigma", {
  expect_true(!is.null(moments_bl$mu),    label = "BL moments has mu")
  expect_true(!is.null(moments_bl$sigma), label = "BL moments has sigma")
})

test_that("black_litterman mu has length N", {
  expect_equal(length(as.numeric(moments_bl$mu)), N)
})

test_that("black_litterman sigma is an N x N matrix", {
  sig <- moments_bl$sigma
  expect_true(is.matrix(sig))
  expect_equal(dim(sig), c(N, N))
})


# ===========================================================================
# set.portfolio.moments() — meucci method
# ===========================================================================

test_that("set.portfolio.moments(method='meucci') returns a list", {
  expect_true(is.list(moments_meucci))
})

test_that("meucci moments has mu and sigma", {
  expect_true(!is.null(moments_meucci$mu),    label = "meucci moments has mu")
  expect_true(!is.null(moments_meucci$sigma), label = "meucci moments has sigma")
})

test_that("meucci mu has length N", {
  expect_equal(length(as.numeric(moments_meucci$mu)), N)
})

test_that("meucci sigma is an N x N symmetric matrix", {
  sig <- moments_meucci$sigma
  expect_true(is.matrix(sig))
  expect_equal(dim(sig), c(N, N))
  expect_true(isSymmetric(sig, tol = 1e-10), label = "meucci sigma is symmetric")
})


# ===========================================================================
# black.litterman() — called directly
# ===========================================================================

test_that("black.litterman() returns a list", {
  expect_true(is.list(bl_result))
})

test_that("black.litterman() result has BLMu and BLSigma elements", {
  expect_true(!is.null(bl_result$BLMu),    label = "BL result has BLMu")
  expect_true(!is.null(bl_result$BLSigma), label = "BL result has BLSigma")
})

test_that("BLMu has length equal to ncol(R)", {
  expect_equal(length(as.numeric(bl_result$BLMu)), N)
})

test_that("BLSigma is an N x N matrix", {
  sig <- bl_result$BLSigma
  expect_true(is.matrix(sig))
  expect_equal(dim(sig), c(N, N))
})

test_that("BLSigma is symmetric", {
  expect_true(
    isSymmetric(bl_result$BLSigma, tol = 1e-8),
    label = "BLSigma is symmetric"
  )
})

test_that("BLMu is a numeric vector/matrix with no NAs", {
  expect_true(is.numeric(as.numeric(bl_result$BLMu)))
  expect_false(anyNA(bl_result$BLMu))
})

test_that("black.litterman() with an explicit non-trivial view modifies the prior mean", {
  # A forced 1% monthly return view is unlikely to equal the sample mean
  bl_with_view <- black.litterman(edhec5, P, Views = matrix(0.01, nrow = 1))
  expect_false(
    isTRUE(all.equal(as.numeric(bl_with_view$BLMu), colMeans(edhec5))),
    label = "BL posterior mean is blended away from the raw sample prior"
  )
})

test_that("black.litterman() errors when length(Mu) does not match ncol(R)", {
  # Pass a Mu vector of the wrong length (3 instead of N=5)
  expect_error(
    black.litterman(edhec5, P, Mu = c(0.01, 0.02, 0.03)),
    "length of Mu must equal"
  )
})

test_that("black.litterman() accepts explicit Mu and Sigma without error", {
  prior_mu    <- colMeans(edhec5)
  prior_sigma <- cov(edhec5)
  expect_no_error(black.litterman(edhec5, P, Mu = prior_mu, Sigma = prior_sigma))
})

test_that("BlackLittermanFormula() returns the same posterior as black.litterman()", {
  prior_mu    <- colMeans(edhec5)
  prior_sigma <- cov(edhec5)
  Omega       <- tcrossprod(P %*% prior_sigma, P)
  views       <- as.numeric(sqrt(diag(Omega)))

  bl_direct   <- black.litterman(edhec5, P)
  bl_formula  <- PortfolioAnalytics:::BlackLittermanFormula(
    prior_mu, prior_sigma, P, views, Omega
  )

  expect_equal(
    as.numeric(bl_direct$BLMu),
    as.numeric(bl_formula$BLMu),
    tolerance = 1e-10
  )
  expect_equal(
    bl_direct$BLSigma,
    bl_formula$BLSigma,
    tolerance = 1e-10
  )
})


# ===========================================================================
# CCCgarch.MM() — CCC-GARCH conditional moments
# Requires fGarch; everything from this point is skipped gracefully if absent.
# ===========================================================================

skip_if_not_installed("fGarch")

# Fit once at file scope so individual test blocks remain lightweight
garch_moments <- CCCgarch.MM(edhec5)

test_that("CCCgarch.MM() returns a list", {
  expect_true(is.list(garch_moments))
})

test_that("CCCgarch.MM() result has mu, sigma, m3, and m4 elements", {
  expect_true(!is.null(garch_moments$mu),    label = "GARCH moments has mu")
  expect_true(!is.null(garch_moments$sigma), label = "GARCH moments has sigma")
  expect_true(!is.null(garch_moments$m3),    label = "GARCH moments has m3")
  expect_true(!is.null(garch_moments$m4),    label = "GARCH moments has m4")
})

test_that("CCCgarch.MM() mu has length N", {
  expect_equal(length(garch_moments$mu), N)
})

test_that("CCCgarch.MM() sigma is an N x N matrix", {
  sig <- garch_moments$sigma
  expect_true(is.matrix(sig))
  expect_equal(dim(sig), c(N, N))
})
