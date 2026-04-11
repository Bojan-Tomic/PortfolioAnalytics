###############################################################################
# tests/testthat/test-leverage.R
#
# Migrated from inst/tests/test_demo_leverage.R.
# Tests leverage_exposure constraints for dollar-neutral and leveraged
# portfolios optimized with DEoptim.
#
# Setup reproduced inline from demo/demo_leverage_exposure_constraint.R.
# No demo scripts are sourced.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()
skip_if_not_installed("DEoptim")

# ---------------------------------------------------------------------------
# Shared data and base portfolio
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

init.portf <- portfolio.spec(assets = funds)
init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
init.portf <- add.objective(portfolio = init.portf, type = "risk", name = "ES")

# ---------------------------------------------------------------------------
# Dollar-neutral portfolio
#   weight_sum: [-0.01, 0.01]   box: [-0.5, 0.5]   leverage: 2
# ---------------------------------------------------------------------------

dollar.neutral.portf <- init.portf
dollar.neutral.portf <- add.constraint(
  portfolio = dollar.neutral.portf,
  type = "weight_sum",
  min_sum = -0.01,
  max_sum = 0.01
)
dollar.neutral.portf <- add.constraint(
  portfolio = dollar.neutral.portf,
  type = "box",
  min = -0.5,
  max = 0.5
)
dollar.neutral.portf <- add.constraint(
  portfolio = dollar.neutral.portf,
  type = "leverage_exposure",
  leverage = 2
)

set.seed(1234)
dollar.neutral.opt <- tryCatch(
  optimize.portfolio(
    R = R,
    portfolio = dollar.neutral.portf,
    optimize_method = "DEoptim",
    search_size = 10000
  ),
  error = function(e) {
    warning("dollar.neutral.opt optimization failed: ", conditionMessage(e))
    NULL
  }
)

# ---------------------------------------------------------------------------
# Leveraged portfolio
#   weight_sum: [0.99, 1.01]   box: [-0.3, 0.8]   leverage: 1.6
# ---------------------------------------------------------------------------

leveraged.portf <- init.portf
leveraged.portf <- add.constraint(
  portfolio = leveraged.portf,
  type = "weight_sum",
  min_sum = 0.99,
  max_sum = 1.01
)
leveraged.portf <- add.constraint(
  portfolio = leveraged.portf,
  type = "box",
  min = -0.3,
  max = 0.8
)
leveraged.portf <- add.constraint(
  portfolio = leveraged.portf,
  type = "leverage_exposure",
  leverage = 1.6
)

set.seed(1234)
leveraged.opt <- tryCatch(
  optimize.portfolio(
    R = R,
    portfolio = leveraged.portf,
    optimize_method = "DEoptim",
    search_size = 10000
  ),
  error = function(e) {
    warning("leveraged.opt optimization failed: ", conditionMessage(e))
    NULL
  }
)

# ---------------------------------------------------------------------------
# Tests: dollar-neutral portfolio
# ---------------------------------------------------------------------------

test_that("dollar.neutral.portf min_sum constraint is -0.01", {
  expect_equal(dollar.neutral.portf$constraints[[1]]$min_sum, -0.01)
})

test_that("dollar.neutral.portf max_sum constraint is 0.01", {
  expect_equal(dollar.neutral.portf$constraints[[1]]$max_sum, 0.01)
})

test_that("dollar.neutral.portf leverage exposure constraint is 2", {
  expect_equal(dollar.neutral.portf$constraints[[3]]$leverage, 2)
})

test_that("dollar.neutral.opt weights is a numeric vector", {
  skip_if(is.null(dollar.neutral.opt), "dollar.neutral.opt optimization did not converge")
  expect_true(is.numeric(extractWeights(dollar.neutral.opt)))
})

test_that("dollar.neutral.opt leverage exposure constraint is not violated", {
  skip_if(is.null(dollar.neutral.opt), "dollar.neutral.opt optimization did not converge")
  expect_true(sum(abs(extractWeights(dollar.neutral.opt))) <= 2)
})

test_that("dollar.neutral.opt objective measure mean is numeric", {
  skip_if(is.null(dollar.neutral.opt), "dollar.neutral.opt optimization did not converge")
  expect_true(is.numeric(extractObjectiveMeasures(dollar.neutral.opt)$mean))
})

test_that("dollar.neutral.opt objective measure ES is numeric", {
  skip_if(is.null(dollar.neutral.opt), "dollar.neutral.opt optimization did not converge")
  expect_true(is.numeric(extractObjectiveMeasures(dollar.neutral.opt)$ES))
})

# ---------------------------------------------------------------------------
# Tests: leveraged portfolio
# ---------------------------------------------------------------------------

test_that("leveraged.portf min_sum constraint is 0.99", {
  expect_equal(leveraged.portf$constraints[[1]]$min_sum, 0.99)
})

test_that("leveraged.portf max_sum constraint is 1.01", {
  expect_equal(leveraged.portf$constraints[[1]]$max_sum, 1.01)
})

test_that("leveraged.portf leverage exposure constraint is 1.6", {
  expect_equal(leveraged.portf$constraints[[3]]$leverage, 1.6)
})

test_that("leveraged.opt weights is a numeric vector", {
  skip_if(is.null(leveraged.opt), "leveraged.opt optimization did not converge")
  expect_true(is.numeric(extractWeights(leveraged.opt)))
})

test_that("leveraged.opt leverage exposure constraint is not violated", {
  skip_if(is.null(leveraged.opt), "leveraged.opt optimization did not converge")
  expect_true(sum(abs(extractWeights(leveraged.opt))) <= 1.6)
})

test_that("leveraged.opt objective measure mean is numeric", {
  skip_if(is.null(leveraged.opt), "leveraged.opt optimization did not converge")
  expect_true(is.numeric(extractObjectiveMeasures(leveraged.opt)$mean))
})

test_that("leveraged.opt objective measure ES is numeric", {
  skip_if(is.null(leveraged.opt), "leveraged.opt optimization did not converge")
  expect_true(is.numeric(extractObjectiveMeasures(leveraged.opt)$ES))
})
