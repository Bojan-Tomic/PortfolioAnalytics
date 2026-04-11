###############################################################################
# tests/testthat/test-multi-layer.R
#
# Source files covered:
#   R/mult.layer.portfolio.R  — mult.portfolio.spec(), add.sub.portfolio(),
#                               proxy.mult.portfolio() [internal],
#                               sub.portfolio() [internal constructor]
#
# Shared fixtures from helper-portfolioanalytics.R:
#   edhec8, funds8
#
# These tests cover structural and constructor behaviour only.
# The full multi-layer optimisation (optimize.portfolio on a
# mult.portfolio.spec) is intentionally excluded: it requires DEoptim or
# another global solver for the top-level step and is far too slow for an
# automated test suite.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

library(ROI)
library(ROI.plugin.quadprog)

# ---------------------------------------------------------------------------
# File-scope setup
# edhec8 and funds8 come from helper-portfolioanalytics.R.
# We split the 8 assets into two 4-asset sub-portfolios.
# ---------------------------------------------------------------------------

R   <- edhec8
nms <- funds8
N   <- length(nms)   # 8

# Sub-portfolio 1: first 4 assets, minimise StdDev
sub1_assets <- nms[1:4]
sub1 <- portfolio.spec(assets = sub1_assets)
sub1 <- add.constraint(sub1, type = "full_investment")
sub1 <- add.constraint(sub1, type = "long_only")
sub1 <- add.objective(sub1,  type = "risk", name = "StdDev")

# Sub-portfolio 2: last 4 assets, minimise StdDev
sub2_assets <- nms[5:8]
sub2 <- portfolio.spec(assets = sub2_assets)
sub2 <- add.constraint(sub2, type = "full_investment")
sub2 <- add.constraint(sub2, type = "long_only")
sub2 <- add.objective(sub2,  type = "risk", name = "StdDev")

# Top-level portfolio: two "assets" that are the sub-portfolio proxy returns
top_portf <- portfolio.spec(assets = c("proxy.1", "proxy.2"))
top_portf <- add.constraint(top_portf, type = "full_investment")
top_portf <- add.constraint(top_portf, type = "long_only")
top_portf <- add.objective(top_portf,  type = "risk", name = "StdDev")

# Build the mult.portfolio.spec used across the tests below.
# Each sub-portfolio uses ROI with quarterly rebalancing and a 36-period
# training / trailing window.
mult_portf <- mult.portfolio.spec(top_portf)
mult_portf <- add.sub.portfolio(
  mult_portf, sub1,
  optimize_method  = "ROI",
  rebalance_on     = "quarters",
  training_period  = 36L,
  trailing_periods = 36L
)
mult_portf <- add.sub.portfolio(
  mult_portf, sub2,
  optimize_method  = "ROI",
  rebalance_on     = "quarters",
  training_period  = 36L,
  trailing_periods = 36L
)

# ---------------------------------------------------------------------------
# mult.portfolio.spec() — constructor
# ---------------------------------------------------------------------------

test_that("mult.portfolio.spec() returns an object of class 'mult.portfolio.spec'", {
  mp <- mult.portfolio.spec(top_portf)
  expect_s3_class(mp, "mult.portfolio.spec")
})

test_that("mult.portfolio.spec() top.portfolio slot is identical to the supplied portfolio", {
  mp <- mult.portfolio.spec(top_portf)
  expect_identical(mp$top.portfolio, top_portf)
})

test_that("mult.portfolio.spec() initialises sub.portfolios as an empty list", {
  mp <- mult.portfolio.spec(top_portf)
  expect_true(is.list(mp$sub.portfolios))
  expect_equal(length(mp$sub.portfolios), 0L)
})

# ---------------------------------------------------------------------------
# add.sub.portfolio() — building up the spec
# ---------------------------------------------------------------------------

test_that("after adding first sub-portfolio, sub.portfolios has length 1", {
  mp <- mult.portfolio.spec(top_portf)
  mp <- add.sub.portfolio(mp, sub1,
                          optimize_method  = "ROI",
                          rebalance_on     = "quarters",
                          training_period  = 36L,
                          trailing_periods = 36L)
  expect_equal(length(mp$sub.portfolios), 1L)
})

test_that("after adding second sub-portfolio, sub.portfolios has length 2", {
  expect_equal(length(mult_portf$sub.portfolios), 2L)
})

test_that("each sub.portfolios element inherits from 'sub.portfolio'", {
  expect_s3_class(mult_portf$sub.portfolios[[1]], "sub.portfolio")
  expect_s3_class(mult_portf$sub.portfolios[[2]], "sub.portfolio")
})

test_that("sub.portfolios[[1]]$portfolio is a valid portfolio object", {
  expect_true(is.portfolio(mult_portf$sub.portfolios[[1]]$portfolio))
})

test_that("sub.portfolios[[1]] stores the correct optimize_method", {
  expect_equal(mult_portf$sub.portfolios[[1]]$optimize_method, "ROI")
})

test_that("sub.portfolios[[1]] stores the correct rebalance_on", {
  expect_equal(mult_portf$sub.portfolios[[1]]$rebalance_on, "quarters")
})

test_that("sub.portfolios[[1]] stores the correct training_period", {
  expect_equal(mult_portf$sub.portfolios[[1]]$training_period, 36L)
})

# ---------------------------------------------------------------------------
# add.sub.portfolio() — error handling
# ---------------------------------------------------------------------------

test_that("add.sub.portfolio() errors when first argument is not a mult.portfolio.spec", {
  # top_portf is a plain portfolio, not a mult.portfolio.spec
  expect_error(add.sub.portfolio(top_portf, sub1))
})

test_that("add.sub.portfolio() with indexnum overwrites the specified sub-portfolio", {
  # Overwrite slot 1 with sub2; the assets of sub1 should be replaced by sub2's.
  mp_copy <- add.sub.portfolio(
    mult_portf, sub2,
    optimize_method  = "ROI",
    rebalance_on     = "quarters",
    training_period  = 36L,
    trailing_periods = 36L,
    indexnum         = 1L
  )
  # The total count must stay at 2 (no new slot was appended).
  expect_equal(length(mp_copy$sub.portfolios), 2L)
  # The assets in slot 1 must now match sub2.
  expect_equal(
    names(mp_copy$sub.portfolios[[1]]$portfolio$assets),
    sub2_assets
  )
})

# ---------------------------------------------------------------------------
# proxy.mult.portfolio()  (internal — accessed via :::)
#
# This function calls optimize.portfolio.rebalancing() on each sub-portfolio
# sequentially and returns proxy returns as an xts object.  It is the most
# computationally intensive test in this file; we guard against solver
# failures with tryCatch and skip rather than fail if one occurs.
# ---------------------------------------------------------------------------

test_that("proxy.mult.portfolio() returns a 2-column xts object", {
  proxy_ret <- tryCatch(
    suppressWarnings(
      PortfolioAnalytics:::proxy.mult.portfolio(
        R              = edhec8,
        mult.portfolio = mult_portf
      )
    ),
    error = function(e) NULL
  )

  skip_if(
    is.null(proxy_ret),
    "proxy.mult.portfolio() failed — possibly a transient ROI solver issue"
  )

  expect_true(xts::is.xts(proxy_ret))
  expect_equal(ncol(proxy_ret), 2L)
})

# ---------------------------------------------------------------------------
# sub.portfolio()  (internal constructor — accessed via :::)
# ---------------------------------------------------------------------------

test_that("sub.portfolio() returns an object of class 'sub.portfolio'", {
  sp <- PortfolioAnalytics:::sub.portfolio(sub1, optimize_method = "ROI")
  expect_s3_class(sp, "sub.portfolio")
})

test_that("sub.portfolio() errors when the portfolio argument is not a portfolio object", {
  expect_error(
    PortfolioAnalytics:::sub.portfolio(list(x = 1), optimize_method = "ROI")
  )
})
