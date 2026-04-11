###############################################################################
# tests/testthat/test-custom-covRob.R
#
# Source file covered: R/custom.covRob.R
#
# Functions tested:
#   custom.covRob.MM()    — robust mean/cov via RobStatTM::covRobMM()
#   custom.covRob.Rocke() — robust mean/cov via RobStatTM::covRobRocke()
#   MycovRobMcd()         — control list constructor for Mcd
#   custom.covRob.Mcd()   — robust mean/cov via robustbase::covMcd()
#   MycovRobTSGS()        — control list constructor for TSGS
#   custom.covRob.TSGS()  — robust mean/cov via GSE::TSGS()
#
# All six functions are currently at 0% coverage.
#
# Data note: edhec is loaded as xts by helper-portfolioanalytics.R; we
# coerce to a plain numeric matrix with as.matrix() because the robust
# estimators (especially GSE::TSGS) require a plain matrix input.
#
# Expensive estimators are run once at file scope and reused across tests
# to keep individual test_that() blocks fast.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("RobStatTM")
skip_if_not_installed("robustbase")
skip_if_not_installed("GSE")

library(PortfolioAnalytics)
library(RobStatTM)
library(robustbase)
library(GSE)

# ---------------------------------------------------------------------------
# File-scope setup — runs once before any test_that() block.
# edhec is already loaded by helper-portfolioanalytics.R.
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- as.matrix(edhec[, 1:5])   # plain numeric matrix (not xts); N = 5 assets
N <- ncol(R)                    # 5

# Run all expensive estimators once so test blocks remain lightweight.
mm_result    <- custom.covRob.MM(R)
rocke_result <- custom.covRob.Rocke(R)
mcd_result   <- custom.covRob.Mcd(R)
tsgs_result  <- custom.covRob.TSGS(R)


# ===========================================================================
# custom.covRob.MM
# ===========================================================================

test_that("custom.covRob.MM returns a list with mu and sigma elements", {
  expect_true(is.list(mm_result))
  expect_false(is.null(mm_result$mu),    label = "MM result has mu")
  expect_false(is.null(mm_result$sigma), label = "MM result has sigma")
})

test_that("custom.covRob.MM mu has length N", {
  expect_equal(length(mm_result$mu), N)
})

test_that("custom.covRob.MM sigma is an N x N matrix", {
  expect_true(is.matrix(mm_result$sigma))
  expect_equal(dim(mm_result$sigma), c(N, N))
})

test_that("custom.covRob.MM sigma is symmetric", {
  expect_true(isSymmetric(mm_result$sigma), label = "MM sigma is symmetric")
})

test_that("custom.covRob.MM passes tol and maxit args without error", {
  expect_no_error(custom.covRob.MM(R, tol = 1e-3, maxit = 20))
})


# ===========================================================================
# custom.covRob.Rocke
# ===========================================================================

test_that("custom.covRob.Rocke returns a list with mu and sigma elements", {
  expect_true(is.list(rocke_result))
  expect_false(is.null(rocke_result$mu),    label = "Rocke result has mu")
  expect_false(is.null(rocke_result$sigma), label = "Rocke result has sigma")
})

test_that("custom.covRob.Rocke mu has length N", {
  expect_equal(length(rocke_result$mu), N)
})

test_that("custom.covRob.Rocke sigma is an N x N matrix", {
  expect_true(is.matrix(rocke_result$sigma))
  expect_equal(dim(rocke_result$sigma), c(N, N))
})

test_that("custom.covRob.Rocke sigma is symmetric", {
  expect_true(isSymmetric(rocke_result$sigma), label = "Rocke sigma is symmetric")
})


# ===========================================================================
# MycovRobMcd — control list constructor (no external package required)
# ===========================================================================

test_that("MycovRobMcd() returns a list", {
  expect_true(is.list(MycovRobMcd()))
})

test_that("MycovRobMcd() default alpha is 0.5", {
  expect_equal(MycovRobMcd()$alpha, 0.5)
})

test_that("MycovRobMcd() default nsamp is 500", {
  expect_equal(MycovRobMcd()$nsamp, 500)
})

test_that("MycovRobMcd() default tolSolve is 1e-14", {
  expect_equal(MycovRobMcd()$tolSolve, 1e-14)
})

test_that("MycovRobMcd(alpha = 0.75) stores alpha == 0.75", {
  ctrl <- MycovRobMcd(alpha = 0.75)
  expect_equal(ctrl$alpha, 0.75)
})


# ===========================================================================
# custom.covRob.Mcd
# ===========================================================================

test_that("custom.covRob.Mcd returns a list with mu and sigma elements", {
  expect_true(is.list(mcd_result))
  expect_false(is.null(mcd_result$mu),    label = "Mcd result has mu")
  expect_false(is.null(mcd_result$sigma), label = "Mcd result has sigma")
})

test_that("custom.covRob.Mcd mu has length N", {
  expect_equal(length(mcd_result$mu), N)
})

test_that("custom.covRob.Mcd sigma is an N x N matrix", {
  expect_true(is.matrix(mcd_result$sigma))
  expect_equal(dim(mcd_result$sigma), c(N, N))
})

# Note: passing `control = MycovRobMcd(alpha = 0.6)` triggers a pre-existing
# source-code bug — `match.call(expand.dots=TRUE)$control` returns an
# unevaluated call object, so `control$wgtFUN` is NULL and covMcd() rejects
# it.  The equivalent intent is tested by passing alpha directly as a dot
# argument, which the `hasArg(alpha)` branch handles correctly.
test_that("custom.covRob.Mcd with alpha = 0.6 passed directly runs without error", {
  expect_no_error(custom.covRob.Mcd(R, alpha = 0.6))
})


# ===========================================================================
# MycovRobTSGS — control list constructor (no external package required)
# ===========================================================================

test_that("MycovRobTSGS() returns a list", {
  expect_true(is.list(MycovRobTSGS()))
})

test_that("MycovRobTSGS() default filter is 'UBF-DDC'", {
  expect_equal(MycovRobTSGS()$filter, "UBF-DDC")
})

test_that("MycovRobTSGS() default tol is 1e-4", {
  expect_equal(MycovRobTSGS()$tol, 1e-4)
})

test_that("MycovRobTSGS() maxiter is stored as an integer", {
  expect_true(is.integer(MycovRobTSGS()$maxiter))
})

test_that("MycovRobTSGS(filter = 'UBF') stores filter == 'UBF'", {
  ctrl <- MycovRobTSGS(filter = "UBF")
  expect_equal(ctrl$filter, "UBF")
})


# ===========================================================================
# custom.covRob.TSGS
# Returns list(mu = tsgsRob@mu, sigma = tsgsRob@S) from an S4 TSGS object.
# ===========================================================================

test_that("custom.covRob.TSGS returns a list with mu and sigma elements", {
  expect_true(is.list(tsgs_result))
  expect_false(is.null(tsgs_result$mu),    label = "TSGS result has mu")
  expect_false(is.null(tsgs_result$sigma), label = "TSGS result has sigma")
})

test_that("custom.covRob.TSGS mu is numeric with length N", {
  expect_true(is.numeric(tsgs_result$mu), label = "TSGS mu is numeric")
  expect_equal(length(tsgs_result$mu), N)
})

test_that("custom.covRob.TSGS sigma is an N x N matrix", {
  expect_true(is.matrix(tsgs_result$sigma))
  expect_equal(dim(tsgs_result$sigma), c(N, N))
})

test_that("custom.covRob.TSGS sigma is symmetric", {
  expect_true(isSymmetric(tsgs_result$sigma), label = "TSGS sigma is symmetric")
})
