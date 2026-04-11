###############################################################################
# tests/testthat/test-max-return.R
#
# Migrated from inst/tests/test_demo_max_return.R
#  - Removed require(testthat) / library(testthat)
#  - Removed all context() calls
#  - Replaced expect_that() with modern equivalents
#  - Reproduced demo/demo_max_return.R setup inline (no sourcing)
#  - Added skip guards
#  - Fixed description bug: "minES.box1.RP max box constraints are not violated"
#    corrected to "maxret.box1.RP max box constraints are not violated"
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(DEoptim)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")
skip_if_not_installed("DEoptim")

# ---------------------------------------------------------------------------
# Data and portfolio setup
# Reproduces demo/demo_max_return.R inline (no sourcing).
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

# Base portfolio spec: full_investment + long_only + max mean return
init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")

# ROI long-only: deterministic
maxret.lo.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# Replace constraint [[2]] with explicit box (min = 0.05, max = 0.30)
init.portf <- add.constraint(
  portfolio = init.portf, type = "box",
  min = 0.05, max = 0.3, indexnum = 2
)

# ROI box-constrained: deterministic
maxret.box.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# Relax weight_sum bounds for stochastic solvers
init.portf$constraints[[1]]$min_sum <- 0.99
init.portf$constraints[[1]]$max_sum <- 1.01

# Add StdDev with multiplier = 0 (tracked in output, not in objective)
init.portf <- add.objective(
  portfolio = init.portf, type = "risk",
  name = "StdDev", multiplier = 0
)

# RP variant 1: wider box (allows shorting)
port1 <- add.constraint(
  portfolio = init.portf, type = "box",
  min = -0.3, max = 0.8, indexnum = 2
)
maxret.box1.RP <- optimize.portfolio(
  R = R, portfolio = port1,
  optimize_method = "random",
  search_size = 2000, trace = TRUE
)

# RP variant 2: tighter box
port2 <- add.constraint(
  portfolio = init.portf, type = "box",
  min = 0.05, max = 0.3, indexnum = 2
)
maxret.box2.RP <- optimize.portfolio(
  R = R, portfolio = port2,
  optimize_method = "random",
  search_size = 2000, trace = TRUE
)

# DEoptim (uses init.portf: box min = 0.05, max = 0.30)
maxret.box.DE <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "DEoptim",
  search_size = 2000, trace = TRUE
)


# ---------------------------------------------------------------------------
# Tests: ROI long-only
# ---------------------------------------------------------------------------

test_that("maxret.lo.ROI contains mean as an objective", {
  expect_equal(maxret.lo.ROI$portfolio$objectives[[1]]$name, "mean")
})

test_that("maxret.lo.ROI objective measure mean = 0.006824915", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(maxret.lo.ROI)$mean),
    0.006824915,
    tolerance = 1e-6
  )
})

test_that("maxret.lo.ROI min box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(maxret.lo.ROI)), 10) >=
      maxret.lo.ROI$portfolio$constraints[[2]]$min)
  )
})

test_that("maxret.lo.ROI max box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(maxret.lo.ROI)), 10) <=
      maxret.lo.ROI$portfolio$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: ROI box-constrained
# ---------------------------------------------------------------------------

test_that("maxret.box.ROI contains mean as an objective", {
  expect_equal(maxret.box.ROI$portfolio$objectives[[1]]$name, "mean")
})

test_that("maxret.box.ROI objective measure mean = 0.006238891", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(maxret.box.ROI)$mean),
    0.006238891,
    tolerance = 1e-6
  )
})

test_that("maxret.box.ROI min box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(maxret.box.ROI)), 10) >=
      maxret.box.ROI$portfolio$constraints[[2]]$min)
  )
})

test_that("maxret.box.ROI max box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(maxret.box.ROI)), 10) <=
      maxret.box.ROI$portfolio$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: RP box1 — wider box (allows shorting)
# ---------------------------------------------------------------------------

test_that("maxret.box1.RP contains mean as an objective", {
  expect_equal(maxret.box1.RP$portfolio$objectives[[1]]$name, "mean")
})

test_that("maxret.box1.RP contains StdDev as an objective", {
  expect_equal(maxret.box1.RP$portfolio$objectives[[2]]$name, "StdDev")
})

test_that("maxret.box1.RP objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxret.box1.RP)$StdDev))
})

test_that("maxret.box1.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxret.box1.RP)$mean))
})

test_that("maxret.box1.RP min box constraints are not violated", {
  expect_true(
    all(extractWeights(maxret.box1.RP) >= port1$constraints[[2]]$min)
  )
})

test_that("maxret.box1.RP max box constraints are not violated", {
  expect_true(
    all(extractWeights(maxret.box1.RP) <= port1$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: RP box2 — tighter box
# ---------------------------------------------------------------------------

test_that("maxret.box2.RP contains mean as an objective", {
  expect_equal(maxret.box2.RP$portfolio$objectives[[1]]$name, "mean")
})

test_that("maxret.box2.RP contains StdDev as an objective", {
  expect_equal(maxret.box2.RP$portfolio$objectives[[2]]$name, "StdDev")
})

test_that("maxret.box2.RP objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxret.box2.RP)$StdDev))
})

test_that("maxret.box2.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxret.box2.RP)$mean))
})

test_that("maxret.box2.RP min box constraints are not violated", {
  expect_true(
    all(extractWeights(maxret.box2.RP) >= port2$constraints[[2]]$min)
  )
})

test_that("maxret.box2.RP max box constraints are not violated", {
  expect_true(
    all(extractWeights(maxret.box2.RP) <= port2$constraints[[2]]$max)
  )
})


# ---------------------------------------------------------------------------
# Tests: DEoptim box
# ---------------------------------------------------------------------------

test_that("maxret.box.DE contains mean as an objective", {
  expect_equal(maxret.box.DE$portfolio$objectives[[1]]$name, "mean")
})

test_that("maxret.box.DE contains StdDev as an objective", {
  expect_equal(maxret.box.DE$portfolio$objectives[[2]]$name, "StdDev")
})

test_that("maxret.box.DE objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxret.box.DE)$StdDev))
})

test_that("maxret.box.DE objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxret.box.DE)$mean))
})

test_that("maxret.box.DE min box constraints are not violated", {
  expect_true(
    all(extractWeights(maxret.box.DE) >= init.portf$constraints[[2]]$min)
  )
})

test_that("maxret.box.DE max box constraints are not violated", {
  expect_true(
    all(extractWeights(maxret.box.DE) <= init.portf$constraints[[2]]$max)
  )
})
