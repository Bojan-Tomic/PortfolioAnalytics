###############################################################################
# tests/testthat/test-min-expected-shortfall.R
#
# Migration of inst/tests/test_demo_min_expected_shortfall.R
# Tests for minimum ES portfolio optimization via ROI (deterministic),
# random portfolios, and DEoptim (stochastic).
#
# Bug fixed: legacy context label "minES.box1.DE" corrected to "minES.box.DE"
# throughout (object is always `minES.box.DE`).
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")
skip_if_not_installed("DEoptim")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Setup — inline reproduction of demo/demo_min_expected_shortfall.R
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(
  portfolio = init.portf, type = "risk", name = "ES",
  arguments = list(p = 0.9)
)

# --- ROI: long-only, full-investment ---
minES.lo.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# --- ROI: box-constrained (replaces long_only at indexnum = 2) ---
init.portf <- add.constraint(
  portfolio = init.portf, type = "box",
  min = 0.05, max = 0.3, indexnum = 2
)
minES.box.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# Relax full-investment constraint and add tracked mean objective for stochastic
init.portf$constraints[[1]]$min_sum <- 0.99
init.portf$constraints[[1]]$max_sum <- 1.01
init.portf <- add.objective(
  portfolio = init.portf, type = "return",
  name = "mean", multiplier = 0
)

# --- RP: wide box (min = -0.3, max = 0.8) ---
port1 <- add.constraint(
  portfolio = init.portf, type = "box",
  min = -0.3, max = 0.8, indexnum = 2
)
minES.box1.RP <- optimize.portfolio(
  R = R, portfolio = port1,
  optimize_method = "random",
  search_size = 2000, trace = TRUE
)

# --- RP: tight box (min = 0.05, max = 0.3) ---
port2 <- add.constraint(
  portfolio = init.portf, type = "box",
  min = 0.05, max = 0.3, indexnum = 2
)
minES.box2.RP <- optimize.portfolio(
  R = R, portfolio = port2,
  optimize_method = "random",
  search_size = 2000, trace = TRUE
)

# --- DEoptim: box-constrained (uses init.portf with min=0.05, max=0.3) ---
minES.box.DE <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "DEoptim",
  search_size = 2000, trace = TRUE
)


# ===========================================================================
# ROI — long-only, full-investment (deterministic)
# ===========================================================================

test_that("minES.lo.ROI contains ES as an objective", {
  expect_true(minES.lo.ROI$portfolio$objectives[[1]]$name == "ES")
})

test_that("minES.lo.ROI ES objective p = 0.9", {
  expect_equal(minES.lo.ROI$portfolio$objectives[[1]]$arguments$p, 0.9)
})

test_that("minES.lo.ROI is an optimize.portfolio.ROI object", {
  expect_s3_class(minES.lo.ROI, "optimize.portfolio.ROI")
})

test_that("minES.lo.ROI objective measure ES = 0.01024673", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(minES.lo.ROI)$ES),
    0.01024673,
    tolerance = 1e-6
  )
})

test_that("minES.lo.ROI min box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(minES.lo.ROI)), 10) >=
      minES.lo.ROI$portfolio$constraints[[2]]$min - 1e-6)
  )
})

test_that("minES.lo.ROI max box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(minES.lo.ROI)), 10) <=
      minES.lo.ROI$portfolio$constraints[[2]]$max + 1e-6)
  )
})


# ===========================================================================
# ROI — box-constrained (deterministic)
# ===========================================================================

test_that("minES.box.ROI contains ES as an objective", {
  expect_true(minES.box.ROI$portfolio$objectives[[1]]$name == "ES")
})

test_that("minES.box.ROI ES objective p = 0.9", {
  expect_equal(minES.box.ROI$portfolio$objectives[[1]]$arguments$p, 0.9)
})

test_that("minES.box.ROI is an optimize.portfolio.ROI object", {
  expect_s3_class(minES.box.ROI, "optimize.portfolio.ROI")
})

test_that("minES.box.ROI objective measure ES = 0.01482286", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(minES.box.ROI)$ES),
    0.01482286,
    tolerance = 1e-6
  )
})

test_that("minES.box.ROI min box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(minES.box.ROI)), 10) >=
      minES.box.ROI$portfolio$constraints[[2]]$min - 1e-6)
  )
})

test_that("minES.box.ROI max box constraints are not violated", {
  expect_true(
    all(round(as.numeric(extractWeights(minES.box.ROI)), 10) <=
      minES.box.ROI$portfolio$constraints[[2]]$max + 1e-6)
  )
})


# ===========================================================================
# Random portfolios — wide box (stochastic: class/numeric/bounds only)
# ===========================================================================

test_that("minES.box1.RP contains ES as an objective", {
  expect_true(minES.box1.RP$portfolio$objectives[[1]]$name == "ES")
})

test_that("minES.box1.RP ES objective p = 0.9", {
  expect_equal(minES.box1.RP$portfolio$objectives[[1]]$arguments$p, 0.9)
})

test_that("minES.box1.RP contains mean as an objective", {
  expect_true(minES.box1.RP$portfolio$objectives[[2]]$name == "mean")
})

test_that("minES.box1.RP is an optimize.portfolio.random object", {
  expect_s3_class(minES.box1.RP, "optimize.portfolio.random")
})

test_that("minES.box1.RP weights are numeric", {
  expect_true(is.numeric(extractWeights(minES.box1.RP)))
})

test_that("minES.box1.RP objective measure ES is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minES.box1.RP)$ES))
})

test_that("minES.box1.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minES.box1.RP)$mean))
})

test_that("minES.box1.RP min box constraints are not violated", {
  expect_true(
    all(extractWeights(minES.box1.RP) >=
      minES.box1.RP$portfolio$constraints[[2]]$min - 1e-6)
  )
})

test_that("minES.box1.RP max box constraints are not violated", {
  expect_true(
    all(extractWeights(minES.box1.RP) <=
      minES.box1.RP$portfolio$constraints[[2]]$max + 1e-6)
  )
})


# ===========================================================================
# Random portfolios — tight box (stochastic: class/numeric/bounds only)
# ===========================================================================

test_that("minES.box2.RP contains ES as an objective", {
  expect_true(minES.box2.RP$portfolio$objectives[[1]]$name == "ES")
})

test_that("minES.box2.RP ES objective p = 0.9", {
  expect_equal(minES.box2.RP$portfolio$objectives[[1]]$arguments$p, 0.9)
})

test_that("minES.box2.RP contains mean as an objective", {
  expect_true(minES.box2.RP$portfolio$objectives[[2]]$name == "mean")
})

test_that("minES.box2.RP is an optimize.portfolio.random object", {
  expect_s3_class(minES.box2.RP, "optimize.portfolio.random")
})

test_that("minES.box2.RP weights are numeric", {
  expect_true(is.numeric(extractWeights(minES.box2.RP)))
})

test_that("minES.box2.RP objective measure ES is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minES.box2.RP)$ES))
})

test_that("minES.box2.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minES.box2.RP)$mean))
})

test_that("minES.box2.RP min box constraints are not violated", {
  expect_true(
    all(extractWeights(minES.box2.RP) >=
      minES.box2.RP$portfolio$constraints[[2]]$min - 1e-6)
  )
})

test_that("minES.box2.RP max box constraints are not violated", {
  expect_true(
    all(extractWeights(minES.box2.RP) <=
      minES.box2.RP$portfolio$constraints[[2]]$max + 1e-6)
  )
})


# ===========================================================================
# DEoptim — box-constrained (stochastic: class/numeric/bounds only)
# Bug fix: legacy context was "minES.box1.DE" — correct object is minES.box.DE
# ===========================================================================

test_that("minES.box.DE contains ES as an objective", {
  expect_true(minES.box.DE$portfolio$objectives[[1]]$name == "ES")
})

test_that("minES.box.DE ES objective p = 0.9", {
  expect_equal(minES.box.DE$portfolio$objectives[[1]]$arguments$p, 0.9)
})

test_that("minES.box.DE contains mean as an objective", {
  expect_true(minES.box.DE$portfolio$objectives[[2]]$name == "mean")
})

test_that("minES.box.DE is an optimize.portfolio.DEoptim object", {
  expect_s3_class(minES.box.DE, "optimize.portfolio.DEoptim")
})

test_that("minES.box.DE weights are numeric", {
  expect_true(is.numeric(extractWeights(minES.box.DE)))
})

test_that("minES.box.DE objective measure ES is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minES.box.DE)$ES))
})

test_that("minES.box.DE objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minES.box.DE)$mean))
})

test_that("minES.box.DE min box constraints are not violated", {
  expect_true(
    all(extractWeights(minES.box.DE) >=
      minES.box.DE$portfolio$constraints[[2]]$min - 1e-6)
  )
})

test_that("minES.box.DE max box constraints are not violated", {
  expect_true(
    all(extractWeights(minES.box.DE) <=
      minES.box.DE$portfolio$constraints[[2]]$max + 1e-6)
  )
})
