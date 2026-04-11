###############################################################################
# tests/testthat/test-min-stddev.R
#
# Tests for minimum-StdDev portfolio optimisation via ROI (quadprog), random
# portfolios, and DEoptim.  Setup reproduces demo/demo_min_StdDev.R inline.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(DEoptim)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("DEoptim")

# ---------------------------------------------------------------------------
# Data and portfolio setup
# Reproduces demo/demo_min_StdDev.R inline — no sourcing.
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# Base portfolio: full-investment, long-only, minimize StdDev
init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(portfolio = init.portf, type = "risk", name = "StdDev")

# ROI long-only (deterministic)
minStdDev.lo.ROI <- optimize.portfolio(
  R                = R,
  portfolio        = init.portf,
  optimize_method  = "ROI",
  trace            = TRUE
)

# Replace constraint 2 with an explicit box constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type      = "box",
  min       = 0.05,
  max       = 0.3,
  indexnum  = 2
)

# ROI box-constrained (deterministic)
minStdDev.box.ROI <- optimize.portfolio(
  R                = R,
  portfolio        = init.portf,
  optimize_method  = "ROI",
  trace            = TRUE
)

# Relax weight-sum bounds for stochastic solvers
init.portf$constraints[[1]]$min_sum <- 0.99
init.portf$constraints[[1]]$max_sum <- 1.01

# Track mean return in objective output (multiplier = 0 means not optimised)
init.portf <- add.objective(
  portfolio  = init.portf,
  type       = "return",
  name       = "mean",
  multiplier = 0
)

# RP variant 1: wider box (allows shorting)
port1 <- add.constraint(
  portfolio = init.portf,
  type      = "box",
  min       = -0.3,
  max       =  0.8,
  indexnum  = 2
)

minStdDev.box1.RP <- optimize.portfolio(
  R                = R,
  portfolio        = port1,
  optimize_method  = "random",
  search_size      = 2000,
  trace            = TRUE
)

# RP variant 2: tighter box
port2 <- add.constraint(
  portfolio = init.portf,
  type      = "box",
  min       = 0.05,
  max       = 0.3,
  indexnum  = 2
)

minStdDev.box2.RP <- optimize.portfolio(
  R                = R,
  portfolio        = port2,
  optimize_method  = "random",
  search_size      = 2000,
  trace            = TRUE
)

# DEoptim (uses init.portf: box min=0.05, max=0.3)
minStdDev.box.DE <- optimize.portfolio(
  R                = R,
  portfolio        = init.portf,
  optimize_method  = "DEoptim",
  search_size      = 2000,
  trace            = TRUE
)


# ---------------------------------------------------------------------------
# Tests: ROI long-only
# ---------------------------------------------------------------------------

test_that("minStdDev.lo.ROI contains StdDev as an objective", {
  expect_equal(minStdDev.lo.ROI$portfolio$objectives[[1]]$name, "StdDev")
})

test_that("minStdDev.lo.ROI objective measure StdDev = 0.00764702", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(minStdDev.lo.ROI)$StdDev),
    0.00764702,
    tolerance = 1e-6
  )
})

test_that("minStdDev.lo.ROI min box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(minStdDev.lo.ROI)), 10) >=
      minStdDev.lo.ROI$portfolio$constraints[[2]]$min)
  )
})

test_that("minStdDev.lo.ROI max box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(minStdDev.lo.ROI)), 10) <=
      minStdDev.lo.ROI$portfolio$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: ROI box-constrained
# ---------------------------------------------------------------------------

test_that("minStdDev.box.ROI contains StdDev as an objective", {
  expect_equal(minStdDev.box.ROI$portfolio$objectives[[1]]$name, "StdDev")
})

test_that("minStdDev.box.ROI objective measure StdDev = 0.01033111", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(minStdDev.box.ROI)$StdDev),
    0.01033111,
    tolerance = 1e-6
  )
})

test_that("minStdDev.box.ROI min box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(minStdDev.box.ROI)), 10) >=
      minStdDev.box.ROI$portfolio$constraints[[2]]$min)
  )
})

test_that("minStdDev.box.ROI max box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(minStdDev.box.ROI)), 10) <=
      minStdDev.box.ROI$portfolio$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: Random portfolios — box variant 1 (wider box, allows shorting)
# ---------------------------------------------------------------------------

test_that("minStdDev.box1.RP contains StdDev as an objective", {
  expect_equal(minStdDev.box1.RP$portfolio$objectives[[1]]$name, "StdDev")
})

test_that("minStdDev.box1.RP contains mean as an objective", {
  expect_equal(minStdDev.box1.RP$portfolio$objectives[[2]]$name, "mean")
})

test_that("minStdDev.box1.RP objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minStdDev.box1.RP)$StdDev))
})

test_that("minStdDev.box1.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minStdDev.box1.RP)$mean))
})

test_that("minStdDev.box1.RP min box constraints are not violated", {
  expect_true(
    all(extractWeights(minStdDev.box1.RP) >= port1$constraints[[2]]$min)
  )
})

test_that("minStdDev.box1.RP max box constraints are not violated", {
  expect_true(
    all(extractWeights(minStdDev.box1.RP) <= port1$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: Random portfolios — box variant 2 (tighter box)
# ---------------------------------------------------------------------------

test_that("minStdDev.box2.RP contains StdDev as an objective", {
  expect_equal(minStdDev.box2.RP$portfolio$objectives[[1]]$name, "StdDev")
})

test_that("minStdDev.box2.RP contains mean as an objective", {
  expect_equal(minStdDev.box2.RP$portfolio$objectives[[2]]$name, "mean")
})

test_that("minStdDev.box2.RP objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minStdDev.box2.RP)$StdDev))
})

test_that("minStdDev.box2.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minStdDev.box2.RP)$mean))
})

test_that("minStdDev.box2.RP min box constraints are not violated", {
  expect_true(
    all(extractWeights(minStdDev.box2.RP) >= port2$constraints[[2]]$min)
  )
})

test_that("minStdDev.box2.RP max box constraints are not violated", {
  expect_true(
    all(extractWeights(minStdDev.box2.RP) <= port2$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: DEoptim box-constrained
# ---------------------------------------------------------------------------

test_that("minStdDev.box.DE contains StdDev as an objective", {
  expect_equal(minStdDev.box.DE$portfolio$objectives[[1]]$name, "StdDev")
})

test_that("minStdDev.box.DE contains mean as an objective", {
  expect_equal(minStdDev.box.DE$portfolio$objectives[[2]]$name, "mean")
})

test_that("minStdDev.box.DE objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minStdDev.box.DE)$StdDev))
})

test_that("minStdDev.box.DE objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minStdDev.box.DE)$mean))
})

test_that("minStdDev.box.DE min box constraints are not violated", {
  expect_true(
    all(extractWeights(minStdDev.box.DE) >= init.portf$constraints[[2]]$min)
  )
})

test_that("minStdDev.box.DE max box constraints are not violated", {
  expect_true(
    all(extractWeights(minStdDev.box.DE) <= init.portf$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: print and summary methods for optimize.portfolio objects
# These blocks exercise generics.R: print.optimize.portfolio.ROI,
# print.optimize.portfolio.random, print.optimize.portfolio.DEoptim,
# summary.optimize.portfolio, and print.summary.optimize.portfolio.
# ---------------------------------------------------------------------------

test_that("print.optimize.portfolio.ROI produces output without error", {
  expect_output(print(minStdDev.lo.ROI))
})

test_that("summary.optimize.portfolio returns a list with key elements", {
  s <- summary(minStdDev.lo.ROI)
  expect_true(is.list(s))
  expect_true(!is.null(s$weights))
  expect_true(!is.null(s$objective_values))
  expect_true(!is.null(s$leverage_constraint))
  expect_true(!is.null(s$box_constraint))
})

test_that("print(summary(optimize.portfolio.ROI)) produces output without error", {
  expect_output(print(summary(minStdDev.lo.ROI)))
})

test_that("print.optimize.portfolio.random produces output without error", {
  expect_output(print(minStdDev.box1.RP))
})

test_that("print.optimize.portfolio.DEoptim produces output without error", {
  expect_output(print(minStdDev.box.DE))
})
