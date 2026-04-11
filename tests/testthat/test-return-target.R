###############################################################################
# tests/testthat/test-return-target.R
#
# Migration of inst/tests/test_demo_return_target.R
# Tests for portfolio optimization with return as an objective vs. a constraint.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("DEoptim")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Setup (inline reproduction of demo/demo_return_target.R)
# ---------------------------------------------------------------------------

utils::data(edhec)
ret <- edhec[, 1:4]

init.portf <- portfolio.spec(assets = colnames(ret))
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")

# Portfolio with return TARGET as objective
ret.obj.portf <- add.objective(
  portfolio = init.portf, type = "return",
  name = "mean", target = 0.006
)

# Portfolio with return target as CONSTRAINT (+ mean objective)
ret.constr.portf <- add.constraint(
  portfolio = init.portf, type = "return",
  return_target = 0.006
)
ret.constr.portf <- add.objective(
  portfolio = ret.constr.portf,
  type = "return", name = "mean"
)

# ROI optimization: return as objective
ret.obj.opt <- optimize.portfolio(
  R = ret, portfolio = ret.obj.portf,
  optimize_method = "ROI"
)

# ROI optimization: return as constraint
ret.constr.opt <- optimize.portfolio(
  R = ret, portfolio = ret.constr.portf,
  optimize_method = "ROI"
)

# Relax full-investment constraint for stochastic solvers
ret.obj.portf$constraints[[1]]$min_sum <- 0.99
ret.obj.portf$constraints[[1]]$max_sum <- 1.01
ret.constr.portf$constraints[[1]]$min_sum <- 0.99
ret.constr.portf$constraints[[1]]$max_sum <- 1.01

# DEoptim
set.seed(123)
opt.obj.de <- optimize.portfolio(
  R = ret, portfolio = ret.obj.portf,
  optimize_method = "DEoptim",
  search_size = 2000, traceDE = 5
)
set.seed(123)
opt.constr.de <- optimize.portfolio(
  R = ret, portfolio = ret.constr.portf,
  optimize_method = "DEoptim",
  search_size = 2000, traceDE = 5
)

# Random portfolios
opt.obj.rp <- optimize.portfolio(
  R = ret, portfolio = ret.obj.portf,
  optimize_method = "random",
  search_size = 2000
)
opt.constr.rp <- optimize.portfolio(
  R = ret, portfolio = ret.constr.portf,
  optimize_method = "random",
  search_size = 2000
)

# ---------------------------------------------------------------------------
# Portfolio structure: return target as objective
# ---------------------------------------------------------------------------

test_that("ret.obj.portf contains mean as an objective", {
  expect_true(ret.obj.portf$objectives[[1]]$name == "mean")
})

test_that("ret.obj.portf contains mean as an objective with target = 0.006", {
  expect_equal(ret.obj.portf$objectives[[1]]$target, 0.006)
})

# ---------------------------------------------------------------------------
# Portfolio structure: return target as constraint
# Bug fix: legacy description incorrectly read "ret.obj.portf contains target
# return as a constraint" -- corrected to ret.constr.portf throughout.
# ---------------------------------------------------------------------------

test_that("ret.constr.portf contains target return as a constraint", {
  expect_true(ret.constr.portf$constraints[[3]]$type == "return")
})

test_that("ret.constr.portf contains mean as a constraint with target = 0.006", {
  expect_equal(ret.constr.portf$constraints[[3]]$return_target, 0.006)
})

# ---------------------------------------------------------------------------
# ROI results (deterministic -- use tolerance = 1e-6)
# ---------------------------------------------------------------------------

test_that("ret.obj.opt is an optimize.portfolio.ROI object", {
  expect_s3_class(ret.obj.opt, "optimize.portfolio.ROI")
})

test_that("ret.obj.opt weights are numeric", {
  expect_true(is.numeric(extractWeights(ret.obj.opt)))
})

test_that("ret.obj.opt objective measure mean = 0.006", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(ret.obj.opt)$mean),
    0.006,
    tolerance = 1e-6
  )
})

test_that("ret.constr.opt is an optimize.portfolio.ROI object", {
  expect_s3_class(ret.constr.opt, "optimize.portfolio.ROI")
})

test_that("ret.constr.opt weights are numeric", {
  expect_true(is.numeric(extractWeights(ret.constr.opt)))
})

test_that("ret.constr.opt objective measure mean = 0.006", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(ret.constr.opt)$mean),
    0.006,
    tolerance = 1e-6
  )
})

# ---------------------------------------------------------------------------
# DEoptim results (stochastic -- check class, numeric, bounds; not exact values)
# ---------------------------------------------------------------------------

test_that("opt.obj.de is an optimize.portfolio.DEoptim object", {
  expect_s3_class(opt.obj.de, "optimize.portfolio.DEoptim")
})

test_that("opt.obj.de weights are numeric", {
  expect_true(is.numeric(extractWeights(opt.obj.de)))
})

test_that("opt.obj.de weights sum within [0.99, 1.01]", {
  w_sum <- sum(extractWeights(opt.obj.de))
  expect_true(w_sum >= 0.99 && w_sum <= 1.01)
})

test_that("opt.obj.de weights are non-negative (long only)", {
  expect_true(all(extractWeights(opt.obj.de) >= -1e-6))
})

test_that("opt.obj.de objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(opt.obj.de)$mean))
})

test_that("opt.constr.de is an optimize.portfolio.DEoptim object", {
  expect_s3_class(opt.constr.de, "optimize.portfolio.DEoptim")
})

test_that("opt.constr.de weights are numeric", {
  expect_true(is.numeric(extractWeights(opt.constr.de)))
})

test_that("opt.constr.de weights sum within [0.99, 1.01]", {
  w_sum <- sum(extractWeights(opt.constr.de))
  expect_true(w_sum >= 0.99 && w_sum <= 1.01)
})

test_that("opt.constr.de weights are non-negative (long only)", {
  expect_true(all(extractWeights(opt.constr.de) >= -1e-6))
})

test_that("opt.constr.de objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(opt.constr.de)$mean))
})

# ---------------------------------------------------------------------------
# Random portfolio results (stochastic -- check class, numeric, bounds; not exact values)
# ---------------------------------------------------------------------------

test_that("opt.obj.rp is an optimize.portfolio.random object", {
  expect_s3_class(opt.obj.rp, "optimize.portfolio.random")
})

test_that("opt.obj.rp weights are numeric", {
  expect_true(is.numeric(extractWeights(opt.obj.rp)))
})

test_that("opt.obj.rp weights sum within [0.99, 1.01]", {
  w_sum <- sum(extractWeights(opt.obj.rp))
  expect_true(w_sum >= 0.99 && w_sum <= 1.01)
})

test_that("opt.obj.rp weights are non-negative (long only)", {
  expect_true(all(extractWeights(opt.obj.rp) >= -1e-6))
})

test_that("opt.obj.rp objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(opt.obj.rp)$mean))
})

test_that("opt.constr.rp is an optimize.portfolio.random object", {
  expect_s3_class(opt.constr.rp, "optimize.portfolio.random")
})

test_that("opt.constr.rp weights are numeric", {
  expect_true(is.numeric(extractWeights(opt.constr.rp)))
})

test_that("opt.constr.rp weights sum within [0.99, 1.01]", {
  w_sum <- sum(extractWeights(opt.constr.rp))
  expect_true(w_sum >= 0.99 && w_sum <= 1.01)
})

test_that("opt.constr.rp weights are non-negative (long only)", {
  expect_true(all(extractWeights(opt.constr.rp) >= -1e-6))
})

test_that("opt.constr.rp objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(opt.constr.rp)$mean))
})
