###############################################################################
# tests/testthat/test-risk-budgets.R
#
# Migration of inst/tests/test_demo_risk_budgets.R
# Tests for risk budget portfolio optimizations using DEoptim and random
# portfolios with ES and StdDev risk budget objectives.
#
# Bugs fixed vs. legacy file:
#   1. Test description "eqES.portf min_concentration is false" was wrong;
#      corrected to "eqES.portf min_concentration is true" (the expect_true()
#      call was always correct — only the label was wrong).
#   2. rbStdDev.DE constraint-violation test wrongly accessed $ES$pct_contrib_MES;
#      corrected to $StdDev$pct_contrib_StdDev.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("DEoptim")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Setup — inline reproduction of demo/demo_risk_budgets.R
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:8]
funds <- colnames(R)

init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(
  portfolio = init.portf, type = "leverage",
  min_sum = 0.99, max_sum = 1.01
)
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")

# --- ES risk budget: max percentage risk contribution (max_prisk = 0.4) ---
rbES.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
rbES.portf <- add.objective(
  portfolio = rbES.portf, type = "risk_budget", name = "ES",
  max_prisk = 0.4, arguments = list(p = 0.92)
)

set.seed(123)
rbES.DE <- optimize.portfolio(
  R = R, portfolio = rbES.portf,
  optimize_method = "DEoptim",
  search_size = 2000, trace = TRUE
)

# --- ES risk budget: equal risk contribution (min_concentration) ---
eqES.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
eqES.portf <- add.objective(
  portfolio = eqES.portf, type = "risk_budget", name = "ES",
  min_concentration = TRUE,
  arguments = list(p = 0.9, clean = "boudt"),
  multiplier = 10
)

R.clean <- PerformanceAnalytics::Return.clean(R = R, method = "boudt")

set.seed(123)
eqES.RP <- optimize.portfolio(
  R = R.clean, portfolio = eqES.portf,
  optimize_method = "random",
  search_size = 2000, trace = TRUE
)

# --- StdDev risk budget: max percentage risk contribution (max_prisk = 0.25) ---
rbStdDev.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
rbStdDev.portf <- add.objective(
  portfolio = rbStdDev.portf, type = "risk_budget", name = "StdDev",
  max_prisk = 0.25
)

set.seed(123)
rbStdDev.DE <- optimize.portfolio(
  R = R.clean, portfolio = rbStdDev.portf,
  optimize_method = "DEoptim",
  search_size = 2000, trace = TRUE
)

# ---------------------------------------------------------------------------
# Portfolio structure: ES risk budget with max_prisk
# ---------------------------------------------------------------------------

test_that("rbES.portf objectives[[2]] inherits risk_budget_objective", {
  expect_true(inherits(rbES.portf$objectives[[2]], "risk_budget_objective"))
})

test_that("rbES.portf risk budget objective name is ES", {
  expect_true(rbES.portf$objectives[[2]]$name == "ES")
})

test_that("rbES.portf max_prisk is 0.4 for all assets", {
  expect_equal(as.numeric(rbES.portf$objectives[[2]]$max_prisk), rep(0.4, 8))
})

test_that("rbES.portf min_concentration is false", {
  expect_false(rbES.portf$objectives[[2]]$min_concentration)
})

test_that("rbES.portf min_difference is false", {
  expect_false(rbES.portf$objectives[[2]]$min_difference)
})

# ---------------------------------------------------------------------------
# DEoptim results: ES risk budget (stochastic — numeric/class/bounds checks)
# ---------------------------------------------------------------------------

test_that("rbES.DE is an optimize.portfolio.DEoptim object", {
  expect_s3_class(rbES.DE, "optimize.portfolio.DEoptim")
})

test_that("rbES.DE optimal weights are numeric", {
  expect_true(is.numeric(extractWeights(rbES.DE)))
})

test_that("rbES.DE objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(rbES.DE)$mean))
})

test_that("rbES.DE optimization does not violate max_prisk", {
  expect_true(all(extractObjectiveMeasures(rbES.DE)$ES$pct_contrib_MES <= 0.4))
})

# ---------------------------------------------------------------------------
# Portfolio structure: ES risk budget with min_concentration
# ---------------------------------------------------------------------------

test_that("eqES.portf objectives[[2]] inherits risk_budget_objective", {
  expect_true(inherits(eqES.portf$objectives[[2]], "risk_budget_objective"))
})

test_that("eqES.portf risk budget objective name is ES", {
  expect_true(eqES.portf$objectives[[2]]$name == "ES")
})

# Bug fix: description was "eqES.portf min_concentration is false" in legacy;
# corrected to "true" — the expect_true() logic was always correct.
test_that("eqES.portf min_concentration is true", {
  expect_true(eqES.portf$objectives[[2]]$min_concentration)
})

test_that("eqES.portf min_difference is false", {
  expect_false(eqES.portf$objectives[[2]]$min_difference)
})

# ---------------------------------------------------------------------------
# Random portfolio results: ES equal risk (stochastic — numeric/class checks)
# ---------------------------------------------------------------------------

test_that("eqES.RP is an optimize.portfolio.random object", {
  expect_s3_class(eqES.RP, "optimize.portfolio.random")
})

test_that("eqES.RP optimal weights are numeric", {
  expect_true(is.numeric(extractWeights(eqES.RP)))
})

test_that("eqES.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(eqES.RP)$mean))
})

test_that("eqES.RP optimization pct_contrib_MES is a numeric vector", {
  expect_true(is.numeric(extractObjectiveMeasures(eqES.RP)$ES$pct_contrib_MES))
})

# ---------------------------------------------------------------------------
# Portfolio structure: StdDev risk budget with max_prisk
# ---------------------------------------------------------------------------

test_that("rbStdDev.portf objectives[[2]] inherits risk_budget_objective", {
  expect_true(inherits(rbStdDev.portf$objectives[[2]], "risk_budget_objective"))
})

test_that("rbStdDev.portf risk budget objective name is StdDev", {
  expect_true(rbStdDev.portf$objectives[[2]]$name == "StdDev")
})

test_that("rbStdDev.portf max_prisk is 0.25 for all assets", {
  expect_equal(as.numeric(rbStdDev.portf$objectives[[2]]$max_prisk), rep(0.25, 8))
})

test_that("rbStdDev.portf min_concentration is false", {
  expect_false(rbStdDev.portf$objectives[[2]]$min_concentration)
})

test_that("rbStdDev.portf min_difference is false", {
  expect_false(rbStdDev.portf$objectives[[2]]$min_difference)
})

# ---------------------------------------------------------------------------
# DEoptim results: StdDev risk budget (stochastic — numeric/class/bounds checks)
# Bug fix: legacy test wrongly accessed $ES$pct_contrib_MES; corrected to
# $StdDev$pct_contrib_StdDev throughout.
# ---------------------------------------------------------------------------

test_that("rbStdDev.DE is an optimize.portfolio.DEoptim object", {
  expect_s3_class(rbStdDev.DE, "optimize.portfolio.DEoptim")
})

test_that("rbStdDev.DE optimal weights are numeric", {
  expect_true(is.numeric(extractWeights(rbStdDev.DE)))
})

test_that("rbStdDev.DE objective measure pct_contrib_StdDev is a numeric vector", {
  expect_true(is.numeric(extractObjectiveMeasures(rbStdDev.DE)$StdDev$pct_contrib_StdDev))
})

test_that("rbStdDev.DE optimization does not violate max_prisk", {
  expect_true(all(extractObjectiveMeasures(rbStdDev.DE)$StdDev$pct_contrib_StdDev <= 0.25))
})
