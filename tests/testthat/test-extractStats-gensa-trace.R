###############################################################################
# tests/testthat/test-extractStats-gensa-trace.R
#
# Source files hit: R/extractstats.R
#
# Functions covered:
#   extractStats.optimize.portfolio.GenSA()  — success path (trace=TRUE)
#   name.replace()                           — all substitution branches
#
# The extractStats.optimize.portfolio.GenSA error path (trace=FALSE) is
# already covered in test-extract-functions.R.  This file covers the
# success path and the name.replace() utility directly.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("GenSA")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Shared portfolio spec — reuse edhec4 from helper
# ---------------------------------------------------------------------------

portf_gensa <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
})

set.seed(7)
opt_gensa_trace <- optimize.portfolio(
  edhec4, portf_gensa,
  optimize_method = "GenSA",
  maxit           = 30,
  trace           = TRUE
)

N <- ncol(edhec4)

# ===========================================================================
# 1. extractStats.optimize.portfolio.GenSA — trace = TRUE success path
# ===========================================================================

test_that("extractStats on GenSA trace=TRUE returns a named numeric vector", {
  stats <- extractStats(opt_gensa_trace)
  expect_true(is.numeric(stats))
  expect_false(is.null(names(stats)))
})

test_that("extractStats GenSA: result contains 'out'", {
  stats <- extractStats(opt_gensa_trace)
  expect_true("out" %in% names(stats))
})

test_that("extractStats GenSA: result contains weight columns (bare asset names)", {
  stats <- extractStats(opt_gensa_trace)
  # GenSA extractStats uses bare asset names (no 'w.' prefix)
  asset_names <- colnames(edhec4)
  expect_true(all(asset_names %in% names(stats)))
})

test_that("extractStats GenSA: result contains StdDev objective measure", {
  stats <- extractStats(opt_gensa_trace)
  expect_true("StdDev" %in% names(stats))
})

test_that("extractStats GenSA: 'out' value is finite", {
  stats <- extractStats(opt_gensa_trace)
  expect_true(is.finite(stats["out"]))
})

test_that("extractStats GenSA: StdDev value is positive", {
  stats <- extractStats(opt_gensa_trace)
  expect_true(stats["StdDev"] > 0)
})

test_that("extractStats GenSA: weight values sum to approximately 1", {
  stats <- extractStats(opt_gensa_trace)
  asset_names <- colnames(edhec4)
  expect_equal(sum(stats[asset_names]), 1, tolerance = 0.02)
})

test_that("extractStats errors on GenSA object without trace", {
  set.seed(8)
  opt_no_trace <- optimize.portfolio(
    edhec4, portf_gensa,
    optimize_method = "GenSA",
    maxit = 10,
    trace = FALSE
  )
  expect_error(extractStats(opt_no_trace), "trace=TRUE")
})


# ===========================================================================
# 2. name.replace() — direct tests of every substitution branch
# ===========================================================================

# name.replace is an internal utility; access via :::
nr <- PortfolioAnalytics:::name.replace

test_that("name.replace: 'mean.mean' → 'mean'", {
  expect_equal(nr("mean.mean"), "mean")
})

test_that("name.replace: 'median.median' → 'median'", {
  expect_equal(nr("median.median"), "median")
})

test_that("name.replace: 'ES.ES' → 'ES'", {
  expect_equal(nr("ES.ES"), "ES")
})

test_that("name.replace: 'ETL.ETL' → 'ETL'", {
  expect_equal(nr("ETL.ETL"), "ETL")
})

test_that("name.replace: 'CVaR.ES' → 'CVaR'", {
  expect_equal(nr("CVaR.ES"), "CVaR")
})

test_that("name.replace: 'ES.MES' → 'ES'", {
  expect_equal(nr("ES.MES"), "ES")
})

test_that("name.replace: 'ETL.MES' → 'ETL'", {
  expect_equal(nr("ETL.MES"), "ETL")
})

test_that("name.replace: 'CVaR.MES' → 'CVaR'", {
  expect_equal(nr("CVaR.MES"), "CVaR")
})

test_that("name.replace: 'VaR.MVaR' → 'VaR'", {
  expect_equal(nr("VaR.MVaR"), "VaR")
})

test_that("name.replace: 'maxDrawdown.maxDrawdown' → 'maxDrawdown'", {
  expect_equal(nr("maxDrawdown.maxDrawdown"), "maxDrawdown")
})

test_that("name.replace: 'sd.sd' → 'StdDev'", {
  expect_equal(nr("sd.sd"), "StdDev")
})

test_that("name.replace: 'StdDev.StdDev' → 'StdDev'", {
  expect_equal(nr("StdDev.StdDev"), "StdDev")
})

test_that("name.replace: 'objective_measures.' prefix is stripped", {
  expect_equal(nr("objective_measures.mean.mean"), "mean")
})

test_that("name.replace: unknown name passes through unchanged", {
  expect_equal(nr("some.other.name"), "some.other.name")
})

test_that("name.replace: vector input — replaces only the matching entry", {
  v <- c("StdDev.StdDev", "w.Asset1", "out")
  result <- nr(v)
  expect_equal(result[1], "StdDev")
  expect_equal(result[2], "w.Asset1")
  expect_equal(result[3], "out")
})

test_that("name.replace: vector with multiple matchable names", {
  v <- c("mean.mean", "ES.ES", "w.A1")
  result <- nr(v)
  expect_equal(result[1], "mean")
  expect_equal(result[2], "ES")
  expect_equal(result[3], "w.A1")
})
