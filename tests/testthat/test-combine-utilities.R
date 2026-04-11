###############################################################################
# tests/testthat/test-combine-utilities.R
#
# Tests for portfolio combination utilities and alternative weighting schemes.
#
# Source files covered:
#   R/opt.outputMvo.R             — opt.outputMvo()
#   R/utility.combine.R           — combine.portfolios(), combine.optimizations()
#   R/equal.weight.R              — equal.weight()
#   R/inverse.volatility.weight.R — inverse.volatility.weight()
#   R/generics.R                  — print.portfolio.list, print.opt.list
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("ROI.plugin.glpk")

library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

# ---------------------------------------------------------------------------
# File-scope setup — built once for the whole file
# ---------------------------------------------------------------------------

R   <- edhec5
nms <- funds5
N   <- length(nms)

# Three ROI optimizations with distinct objectives
portf_var <- make_minvar_portf(R)
opt_var   <- optimize.portfolio(R, portf_var, optimize_method = "ROI")

portf_es  <- make_mines_portf(R)
opt_es    <- optimize.portfolio(R, portf_es, optimize_method = "ROI")

portf_ret <- make_maxret_portf(R)
opt_ret   <- optimize.portfolio(R, portf_ret, optimize_method = "ROI")

# Equal-weight and inverse-volatility-weight (no solver required)
opt_eq    <- equal.weight(R, make_lo_portf(R))
opt_invol <- inverse.volatility.weight(R, make_lo_portf(R))

# Combined portfolio list (3 portfolio specs)
port_list <- combine.portfolios(list(portf_var, portf_es, portf_ret))

# Named combined optimizations list
opt_list <- combine.optimizations(list(
  minvar = opt_var,
  mines  = opt_es,
  maxret = opt_ret
))


# ===========================================================================
# opt.outputMvo()
# ===========================================================================

test_that("opt.outputMvo() returns a list with slots Wgts, Mean, StdDev, SR", {
  out <- opt.outputMvo(opt_var, R)
  expect_true(is.list(out))
  expect_true(all(c("Wgts", "Mean", "StdDev", "SR") %in% names(out)))
})

test_that("opt.outputMvo() Wgts is a numeric vector of length N summing to 1", {
  out <- opt.outputMvo(opt_var, R)
  expect_true(is.numeric(out$Wgts))
  expect_equal(length(out$Wgts), N)
  expect_equal(sum(out$Wgts), 1, tolerance = 1e-10)
})

test_that("opt.outputMvo() Mean and StdDev are positive scalars", {
  out <- opt.outputMvo(opt_var, R)
  expect_true(is.numeric(out$Mean)   && length(out$Mean)   == 1L)
  expect_true(is.numeric(out$StdDev) && length(out$StdDev) == 1L)
  expect_true(out$Mean   > 0)
  expect_true(out$StdDev > 0)
})

test_that("opt.outputMvo() SR is a numeric scalar", {
  out <- opt.outputMvo(opt_var, R)
  expect_true(is.numeric(out$SR) && length(out$SR) == 1L)
})

test_that("opt.outputMvo() with annualize=FALSE Mean equals raw weighted portfolio mean", {
  out          <- opt.outputMvo(opt_var, R, annualize = FALSE)
  expected_mean <- sum(opt_var$weights * colMeans(as.matrix(R)))
  expect_equal(out$Mean, expected_mean, tolerance = 1e-10)
})

test_that("opt.outputMvo() with digits=4 all outputs are rounded to 4 decimal places", {
  out_d4 <- opt.outputMvo(opt_var, R, digits = 4)
  expect_equal(out_d4$Mean,   round(out_d4$Mean,   4))
  expect_equal(out_d4$StdDev, round(out_d4$StdDev, 4))
  expect_equal(out_d4$SR,     round(out_d4$SR,     4))
  expect_equal(out_d4$Wgts,   round(out_d4$Wgts,   4))
})


# ===========================================================================
# combine.portfolios()
# ===========================================================================

test_that("combine.portfolios() on two specs returns class 'portfolio.list'", {
  pl <- combine.portfolios(list(portf_var, portf_es))
  expect_s3_class(pl, "portfolio.list")
})

test_that("port_list has length 3", {
  expect_equal(length(port_list), 3L)
})

test_that("print(port_list) produces output without error", {
  expect_output(print(port_list))
})


# ===========================================================================
# combine.optimizations() and opt.list
# ===========================================================================

test_that("opt_list has class 'opt.list'", {
  expect_s3_class(opt_list, "opt.list")
})

test_that("opt_list has length 3", {
  expect_equal(length(opt_list), 3L)
})

test_that("extractWeights(opt_list) returns a matrix with 3 rows and N columns", {
  wmat <- extractWeights(opt_list)
  expect_true(is.matrix(wmat))
  expect_equal(nrow(wmat), 3L)
  expect_equal(ncol(wmat), N)
})

test_that("each row of extractWeights(opt_list) sums to approximately 1", {
  wmat     <- extractWeights(opt_list)
  row_sums <- rowSums(wmat)
  expect_true(all(abs(row_sums - 1) < TOL_WSUM))
})

test_that("row names of extractWeights(opt_list) match the supplied names", {
  wmat <- extractWeights(opt_list)
  expect_equal(rownames(wmat), c("minvar", "mines", "maxret"))
})

test_that("print(opt_list) produces output without error", {
  expect_output(print(opt_list))
})


# ===========================================================================
# equal.weight()
# ===========================================================================

test_that("opt_eq has class 'optimize.portfolio.eqwt' and inherits 'optimize.portfolio'", {
  expect_s3_class(opt_eq, "optimize.portfolio.eqwt")
  expect_s3_class(opt_eq, "optimize.portfolio")
})

test_that("extractWeights(opt_eq) is a named numeric vector of length N", {
  w <- extractWeights(opt_eq)
  expect_true(is.numeric(w))
  expect_equal(length(w), N)
  expect_false(is.null(names(w)))
})

test_that("all equal weights equal exactly 1/N", {
  w <- extractWeights(opt_eq)
  expect_equal(as.numeric(w), rep(1 / N, N), tolerance = 1e-10)
})

test_that("equal weights sum to 1", {
  w <- extractWeights(opt_eq)
  expect_equal(sum(w), 1, tolerance = 1e-10)
})


# ===========================================================================
# inverse.volatility.weight()
# ===========================================================================

test_that("opt_invol has class 'optimize.portfolio.invol' and inherits 'optimize.portfolio'", {
  expect_s3_class(opt_invol, "optimize.portfolio.invol")
  expect_s3_class(opt_invol, "optimize.portfolio")
})

test_that("extractWeights(opt_invol) is a named numeric vector of length N", {
  w <- extractWeights(opt_invol)
  expect_true(is.numeric(w))
  expect_equal(length(w), N)
  expect_false(is.null(names(w)))
})

test_that("inverse volatility weights sum to approximately 1", {
  w <- extractWeights(opt_invol)
  expect_equal(sum(w), 1, tolerance = TOL_WSUM)
})

test_that("all inverse volatility weights are non-negative", {
  w <- extractWeights(opt_invol)
  expect_true(all(w >= 0))
})

test_that("inverse volatility weights are proportional to 1/StdDev of each asset", {
  inv_vol    <- 1 / apply(as.matrix(R), 2, sd)
  expected_w <- as.numeric(inv_vol / sum(inv_vol))
  actual_w   <- as.numeric(extractWeights(opt_invol))
  expect_equal(actual_w, expected_w, tolerance = 1e-6)
})
