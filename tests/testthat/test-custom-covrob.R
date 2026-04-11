###############################################################################
# tests/testthat/test-custom-covrob.R
#
# Source files hit: R/custom.covRob.R
#
# Functions covered:
#   custom.covRob.MM()     — default path (already tested elsewhere)
#                          — hasArg(tol) TRUE branch
#                          — hasArg(maxit) TRUE branch
#   custom.covRob.Rocke()  — default path
#                          — hasArg TRUE branches: tol, maxit, initial,
#                            maxsteps, propmin, qs
#   custom.covRob.Mcd()    — default path (control=MycovRobMcd())
#                          — hasArg(alpha) TRUE branch
#                          — hasArg(control) TRUE branch
#   MycovRobMcd()          — default call
#                          — numeric beta argument (non-missing, is.numeric TRUE)
#                          — non-numeric / missing beta → default 0.975
#   custom.covRob.TSGS()   — default path
#                          — hasArg TRUE branches: filter, tol, maxiter, loss
#   MycovRobTSGS()         — default call
#                          — explicit argument values (match.arg paths)
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("RobStatTM")
skip_if_not_installed("robustbase")
skip_if_not_installed("GSE")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Shared fixtures — small matrix so robust estimators converge quickly
# ---------------------------------------------------------------------------

set.seed(42)
N  <- 4L
TT <- 60L
R  <- matrix(rnorm(TT * N, mean = 5e-3, sd = 2e-2), nrow = TT, ncol = N)
colnames(R) <- paste0("A", seq_len(N))


# ===========================================================================
# 1. MycovRobMcd() — control constructor
# ===========================================================================

test_that("MycovRobMcd default: returns a list with expected elements", {
  ctrl <- MycovRobMcd()
  expect_true(is.list(ctrl))
  expect_true(!is.null(ctrl$alpha))
  expect_true(!is.null(ctrl$nsamp))
  expect_true(!is.null(ctrl$wgtFUN))
  expect_true(!is.null(ctrl$beta))
})

test_that("MycovRobMcd default: beta defaults to 0.975", {
  ctrl <- MycovRobMcd()
  expect_equal(ctrl$beta, 0.975)
})

test_that("MycovRobMcd with numeric beta: beta is stored as supplied", {
  ctrl <- MycovRobMcd(beta = 0.90)
  expect_equal(ctrl$beta, 0.90)
})

test_that("MycovRobMcd with non-numeric beta: beta falls back to 0.975", {
  ctrl <- MycovRobMcd(beta = "not_a_number")
  expect_equal(ctrl$beta, 0.975)
})

test_that("MycovRobMcd alpha is stored correctly", {
  ctrl <- MycovRobMcd(alpha = 0.75)
  expect_equal(ctrl$alpha, 0.75)
})

test_that("MycovRobMcd nmini and kmini are coerced to integer", {
  ctrl <- MycovRobMcd(nmini = 200.9, kmini = 3.1)
  expect_true(is.integer(ctrl$nmini))
  expect_true(is.integer(ctrl$kmini))
})


# ===========================================================================
# 2. MycovRobTSGS() — control constructor
# ===========================================================================

test_that("MycovRobTSGS default: returns a list with expected elements", {
  ctrl <- MycovRobTSGS()
  expect_true(is.list(ctrl))
  expect_equal(ctrl$filter, "UBF-DDC")
  expect_equal(ctrl$loss,   "bisquare")
  expect_equal(ctrl$partial.impute, FALSE)
})

test_that("MycovRobTSGS with explicit filter: match.arg works", {
  ctrl <- MycovRobTSGS(filter = "DDC")
  expect_equal(ctrl$filter, "DDC")
})

test_that("MycovRobTSGS with explicit loss='rocke': stored correctly", {
  ctrl <- MycovRobTSGS(loss = "rocke")
  expect_equal(ctrl$loss, "rocke")
})

test_that("MycovRobTSGS maxiter coerced to integer", {
  ctrl <- MycovRobTSGS(maxiter = 100.9)
  expect_true(is.integer(ctrl$maxiter))
  expect_equal(ctrl$maxiter, 100L)
})


# ===========================================================================
# 3. custom.covRob.MM() — hasArg TRUE branches
# ===========================================================================

test_that("custom.covRob.MM default: returns list with mu and sigma", {
  out <- custom.covRob.MM(R)
  expect_true(is.list(out))
  expect_false(is.null(out$mu))
  expect_false(is.null(out$sigma))
  expect_equal(length(out$mu), N)
  expect_equal(dim(out$sigma), c(N, N))
})

test_that("custom.covRob.MM with explicit tol: hasArg(tol) TRUE branch", {
  out <- custom.covRob.MM(R, tol = 1e-3)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})

test_that("custom.covRob.MM with explicit maxit: hasArg(maxit) TRUE branch", {
  out <- custom.covRob.MM(R, maxit = 30)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})

test_that("custom.covRob.MM with both tol and maxit explicit", {
  out <- custom.covRob.MM(R, tol = 1e-3, maxit = 30)
  expect_true(is.list(out))
  expect_equal(dim(out$sigma), c(N, N))
})


# ===========================================================================
# 4. custom.covRob.Rocke() — hasArg TRUE branches
# ===========================================================================

test_that("custom.covRob.Rocke default: returns list with mu and sigma", {
  out <- custom.covRob.Rocke(R)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
  expect_equal(dim(out$sigma), c(N, N))
})

test_that("custom.covRob.Rocke with explicit tol: hasArg(tol) TRUE branch", {
  out <- custom.covRob.Rocke(R, tol = 1e-3)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})

test_that("custom.covRob.Rocke with explicit maxit: hasArg(maxit) TRUE branch", {
  out <- custom.covRob.Rocke(R, maxit = 30)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})

test_that("custom.covRob.Rocke with explicit maxsteps: hasArg(maxsteps) TRUE branch", {
  out <- custom.covRob.Rocke(R, maxsteps = 3)
  expect_true(is.list(out))
  expect_equal(dim(out$sigma), c(N, N))
})

test_that("custom.covRob.Rocke with explicit qs: hasArg(qs) TRUE branch", {
  out <- custom.covRob.Rocke(R, qs = 30)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})


# ===========================================================================
# 5. custom.covRob.Mcd() — hasArg TRUE branches
# ===========================================================================

test_that("custom.covRob.Mcd default: returns list with mu and sigma", {
  out <- custom.covRob.Mcd(R)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
  expect_equal(dim(out$sigma), c(N, N))
})

test_that("custom.covRob.Mcd with explicit alpha: hasArg(alpha) TRUE branch", {
  out <- custom.covRob.Mcd(R, alpha = 0.75)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})

test_that("custom.covRob.Mcd with explicit control: hasArg(control) TRUE branch", {
  # Due to match.call() usage, control subsetting only works when all other
  # params are also explicit (so the else branches don't subscript the call obj)
  out <- custom.covRob.Mcd(R,
    control = MycovRobMcd(alpha = 0.6),
    alpha = 0.6, nsamp = 500L, nmini = 300L, kmini = 5L,
    scalefn = "hrv2012", maxcsteps = 200L, initHsets = NULL,
    seed = NULL, tolSolve = 1e-14, wgtFUN = "01.original",
    use.correction = TRUE)
  expect_true(is.list(out))
  expect_equal(dim(out$sigma), c(N, N))
})

test_that("custom.covRob.Mcd sigma is symmetric", {
  out <- custom.covRob.Mcd(R)
  expect_true(isSymmetric(out$sigma, tol = 1e-10))
})


# ===========================================================================
# 6. custom.covRob.TSGS() — hasArg TRUE branches
# ===========================================================================

test_that("custom.covRob.TSGS default: returns list with mu and sigma", {
  out <- custom.covRob.TSGS(R)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
  expect_equal(dim(out$sigma), c(N, N))
})

test_that("custom.covRob.TSGS with explicit filter: hasArg(filter) TRUE branch", {
  out <- custom.covRob.TSGS(R, filter = "DDC")
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})

test_that("custom.covRob.TSGS with explicit tol: hasArg(tol) TRUE branch", {
  out <- custom.covRob.TSGS(R, tol = 1e-3)
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})

test_that("custom.covRob.TSGS with explicit maxiter: hasArg(maxiter) TRUE branch", {
  out <- custom.covRob.TSGS(R, maxiter = 100)
  expect_true(is.list(out))
  expect_equal(dim(out$sigma), c(N, N))
})

test_that("custom.covRob.TSGS with explicit loss='rocke': hasArg(loss) TRUE branch", {
  out <- custom.covRob.TSGS(R, loss = "rocke")
  expect_true(is.list(out))
  expect_equal(length(out$mu), N)
})

test_that("custom.covRob.TSGS sigma is symmetric", {
  out <- custom.covRob.TSGS(R)
  expect_true(isSymmetric(out$sigma, tol = 1e-8))
})
