###############################################################################
# tests/testthat/test-group-constraints.R
#
# Migrated from inst/tests/test_demo_group_constraints.R.
# Reproduces demo/demo_group_constraints.R setup inline (no source()).
#
# Bugs fixed vs. the original:
#   - DE block used extractObjectiveMeasures(minStdDev.ROI)$StdDev;
#     corrected to extractObjectiveMeasures(minStdDev.DE)$StdDev.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("DEoptim")

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:5]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQM")
funds <- colnames(R)

init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.constraint(
  portfolio = init.portf, type = "group",
  groups = list(
    groupA = c(1, 3, 5),
    groupB = c(2, 4)
  ),
  group_min = c(0.05, 0.15),
  group_max = c(0.7, 0.5)
)
init.portf <- add.objective(portfolio = init.portf, type = "risk", name = "StdDev")

# ROI optimization (deterministic — run before relaxing constraints)
minStdDev.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI"
)

# Relax weight_sum tolerance for stochastic methods
init.portf$constraints[[1]]$min_sum <- 0.99
init.portf$constraints[[1]]$max_sum <- 1.01

# Stochastic optimizations
set.seed(1234)
minStdDev.RP <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "random",
  search_size = 2000
)

set.seed(1234)
minStdDev.DE <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "DEoptim",
  search_size = 2000
)

# Convenience references used across multiple tests
group_constr <- init.portf$constraints[[3]]
cLO <- group_constr$cLO # c(0.05, 0.15)
cUP <- group_constr$cUP # c(0.70, 0.50)

# ---------------------------------------------------------------------------
# Group constraint structure
# ---------------------------------------------------------------------------

test_that("init.portf constraint[[3]] is a group_constraint", {
  expect_s3_class(group_constr, "group_constraint")
})

test_that("group constraint for groupA is c(1, 3, 5)", {
  expect_equal(group_constr$groups$groupA, c(1, 3, 5))
})

test_that("group constraint for groupB is c(2, 4)", {
  expect_equal(group_constr$groups$groupB, c(2, 4))
})

test_that("group constraint cLO is c(0.05, 0.15)", {
  expect_equal(group_constr$cLO, c(0.05, 0.15))
})

test_that("group constraint cUP is c(0.7, 0.5)", {
  expect_equal(group_constr$cUP, c(0.7, 0.5))
})

# ---------------------------------------------------------------------------
# ROI optimization — deterministic exact checks
# ---------------------------------------------------------------------------

test_that("minStdDev.ROI weights equal expected values", {
  # Round to 10 d.p. to handle near-zero floating-point artifacts from quadprog
  expect_equal(
    round(as.numeric(extractWeights(minStdDev.ROI)), 10),
    c(0, 0.2535171, 0, 0.04648292, 0.7),
    tolerance = 1e-6
  )
})

test_that("minStdDev.ROI objective measure StdDev equals 0.00967887", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(minStdDev.ROI)$StdDev),
    0.00967887,
    tolerance = 1e-6
  )
})

test_that("minStdDev.ROI group weights are calculated correctly", {
  weights.ROI <- extractWeights(minStdDev.ROI)
  expect_equal(
    as.numeric(extractGroups(minStdDev.ROI)$group_weights),
    c(sum(weights.ROI[c(1, 3, 5)]), sum(weights.ROI[c(2, 4)]))
  )
})

test_that("minStdDev.ROI group weights satisfy lower bound (cLO)", {
  expect_true(all(extractGroups(minStdDev.ROI)$group_weights >= cLO))
})

test_that("minStdDev.ROI group weights satisfy upper bound (cUP)", {
  # Allow 1e-10 tolerance for floating-point artifacts at the group upper bound
  expect_true(all(extractGroups(minStdDev.ROI)$group_weights <= cUP + 1e-10))
})

# ---------------------------------------------------------------------------
# Random portfolio optimization — structural / bounds checks only
# ---------------------------------------------------------------------------

test_that("minStdDev.RP weights is a numeric vector", {
  expect_true(is.numeric(extractWeights(minStdDev.RP)))
})

test_that("minStdDev.RP objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minStdDev.RP)$StdDev))
})

test_that("minStdDev.RP group weights are calculated correctly", {
  weights.RP <- extractWeights(minStdDev.RP)
  expect_equal(
    as.numeric(extractGroups(minStdDev.RP)$group_weights),
    c(sum(weights.RP[c(1, 3, 5)]), sum(weights.RP[c(2, 4)]))
  )
})

test_that("minStdDev.RP group weights satisfy lower bound (cLO)", {
  expect_true(all(extractGroups(minStdDev.RP)$group_weights >= cLO))
})

test_that("minStdDev.RP group weights satisfy upper bound (cUP)", {
  expect_true(all(extractGroups(minStdDev.RP)$group_weights <= cUP))
})

# ---------------------------------------------------------------------------
# DEoptim optimization — structural / bounds checks only
# (Bug fix: legacy test referenced minStdDev.ROI in the StdDev check below)
# ---------------------------------------------------------------------------

test_that("minStdDev.DE weights is a numeric vector", {
  expect_true(is.numeric(extractWeights(minStdDev.DE)))
})

test_that("minStdDev.DE objective measure StdDev is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(minStdDev.DE)$StdDev))
})

test_that("minStdDev.DE group weights are calculated correctly", {
  weights.DE <- extractWeights(minStdDev.DE)
  expect_equal(
    as.numeric(extractGroups(minStdDev.DE)$group_weights),
    c(sum(weights.DE[c(1, 3, 5)]), sum(weights.DE[c(2, 4)]))
  )
})

test_that("minStdDev.DE group weights satisfy lower bound (cLO)", {
  expect_true(all(extractGroups(minStdDev.DE)$group_weights >= cLO))
})

test_that("minStdDev.DE group weights satisfy upper bound (cUP)", {
  expect_true(all(extractGroups(minStdDev.DE)$group_weights <= cUP))
})
