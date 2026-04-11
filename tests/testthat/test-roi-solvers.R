###############################################################################
# tests/testthat/test-roi-solvers.R
#
# Source files covered:
#   R/optFUN.R           — gmv_opt(), maxret_opt(), mines_opt(), gmv_opt_qu()
#   R/optimize.portfolio.R — ROI dispatch: optimize_method = "ROI" / "quadprog"
#                            / "glpk" routing logic
#
# Tests that the generic ROI dispatcher and the explicit solver aliases
# (quadprog, glpk) reach the same solution when given identical inputs.
# Cross-checks:
#   - max return  : ROI (auto → glpk)  vs explicit glpk
#   - min ES      : ROI (auto → glpk)  vs explicit glpk
#   - min StdDev  : ROI (auto → quadprog) vs explicit quadprog
#   - quad utility: ROI (auto → quadprog) vs explicit quadprog
#   - symphony    : optional, guarded with skip_if_not_installed
#   - weight validity for all ROI results
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
# Shared data and portfolio specs (file scope)
# ---------------------------------------------------------------------------

R   <- edhec4
nms <- funds4

portf_init <- portfolio.spec(assets = nms)
portf_init <- add.constraint(portf_init, type = "full_investment")
portf_init <- add.constraint(portf_init, type = "long_only")

# Max mean return (LP)
portf_maxret <- add.objective(portf_init, type = "return", name = "mean")

# Min expected shortfall (LP)
portf_mines <- add.objective(portf_init, type = "risk", name = "ES")

# Min standard deviation (QP)
portf_minsd <- add.objective(portf_init, type = "risk", name = "StdDev")

# Quadratic utility: mean - lambda * StdDev  (QP)
portf_qu <- add.objective(portf_init, type = "risk",   name = "StdDev",
                          risk_aversion = 0.25)
portf_qu <- add.objective(portf_qu,    type = "return", name = "mean")

# ---------------------------------------------------------------------------
# Run all deterministic optimisations at file scope so test blocks are short
# ---------------------------------------------------------------------------

opt_maxret_roi  <- optimize.portfolio(R, portf_maxret, optimize_method = "ROI")
opt_maxret_glpk <- optimize.portfolio(R, portf_maxret, optimize_method = "glpk")

opt_mines_roi   <- optimize.portfolio(R, portf_mines,  optimize_method = "ROI")
opt_mines_glpk  <- optimize.portfolio(R, portf_mines,  optimize_method = "glpk")

opt_minsd_roi   <- optimize.portfolio(R, portf_minsd,  optimize_method = "ROI")
opt_minsd_qp    <- optimize.portfolio(R, portf_minsd,  optimize_method = "quadprog")

opt_qu_roi      <- optimize.portfolio(R, portf_qu,     optimize_method = "ROI")
opt_qu_qp       <- optimize.portfolio(R, portf_qu,     optimize_method = "quadprog")

# ---------------------------------------------------------------------------
# 1–2  Max return: ROI vs glpk
# ---------------------------------------------------------------------------

test_that("max return — ROI and glpk both return class optimize.portfolio.ROI", {
  expect_s3_class(opt_maxret_roi,  "optimize.portfolio.ROI")
  expect_s3_class(opt_maxret_glpk, "optimize.portfolio.ROI")
})

test_that("max return — ROI and glpk extractStats are equal within 1e-4", {
  stats_roi  <- extractStats(opt_maxret_roi)
  stats_glpk <- extractStats(opt_maxret_glpk)
  expect_equal(stats_roi, stats_glpk, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 3–4  Min ES: ROI vs glpk
# ---------------------------------------------------------------------------

test_that("min ES — ROI and glpk both return class optimize.portfolio.ROI", {
  expect_s3_class(opt_mines_roi,  "optimize.portfolio.ROI")
  expect_s3_class(opt_mines_glpk, "optimize.portfolio.ROI")
})

test_that("min ES — ROI and glpk extractStats are equal within 1e-4", {
  stats_roi  <- extractStats(opt_mines_roi)
  stats_glpk <- extractStats(opt_mines_glpk)
  expect_equal(stats_roi, stats_glpk, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 5–6  Min StdDev: ROI vs quadprog
# ---------------------------------------------------------------------------

test_that("min StdDev — ROI and quadprog both return class optimize.portfolio.ROI", {
  expect_s3_class(opt_minsd_roi, "optimize.portfolio.ROI")
  expect_s3_class(opt_minsd_qp,  "optimize.portfolio.ROI")
})

test_that("min StdDev — ROI and quadprog extractStats are equal within 1e-4", {
  stats_roi <- extractStats(opt_minsd_roi)
  stats_qp  <- extractStats(opt_minsd_qp)
  expect_equal(stats_roi, stats_qp, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 7–8  Quadratic utility: ROI vs quadprog
# ---------------------------------------------------------------------------

test_that("quadratic utility — ROI and quadprog both return class optimize.portfolio.ROI", {
  expect_s3_class(opt_qu_roi, "optimize.portfolio.ROI")
  expect_s3_class(opt_qu_qp,  "optimize.portfolio.ROI")
})

test_that("quadratic utility — ROI and quadprog extractStats are equal within 1e-4", {
  stats_roi <- extractStats(opt_qu_roi)
  stats_qp  <- extractStats(opt_qu_qp)
  expect_equal(stats_roi, stats_qp, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 9  Symphony solver (optional — skip when plugin not installed)
# ---------------------------------------------------------------------------

test_that("symphony solver produces valid optimize.portfolio.ROI result", {
  skip_if_not_installed("ROI.plugin.symphony")
  library(ROI.plugin.symphony)
  opt_sym <- optimize.portfolio(R, portf_maxret, optimize_method = "symphony")
  expect_s3_class(opt_sym, "optimize.portfolio.ROI")
  expect_valid_weights(extractWeights(opt_sym))
})

# ---------------------------------------------------------------------------
# 10  Weight validity for all ROI optimisations
# ---------------------------------------------------------------------------

test_that("max return ROI weights are valid (full investment, long only)", {
  expect_valid_weights(extractWeights(opt_maxret_roi))
})

test_that("max return glpk weights are valid (full investment, long only)", {
  expect_valid_weights(extractWeights(opt_maxret_glpk))
})

test_that("min ES ROI weights are valid (full investment, long only)", {
  expect_valid_weights(extractWeights(opt_mines_roi))
})

test_that("min ES glpk weights are valid (full investment, long only)", {
  expect_valid_weights(extractWeights(opt_mines_glpk))
})

test_that("min StdDev ROI weights are valid (full investment, long only)", {
  expect_valid_weights(extractWeights(opt_minsd_roi))
})

test_that("min StdDev quadprog weights are valid (full investment, long only)", {
  expect_valid_weights(extractWeights(opt_minsd_qp))
})

test_that("quadratic utility ROI weights are valid (full investment, long only)", {
  expect_valid_weights(extractWeights(opt_qu_roi))
})

test_that("quadratic utility quadprog weights are valid (full investment, long only)", {
  expect_valid_weights(extractWeights(opt_qu_qp))
})

# ---------------------------------------------------------------------------
# Additional structural checks
# ---------------------------------------------------------------------------

test_that("max return ROI result has no NA weights", {
  expect_false(any(is.na(extractWeights(opt_maxret_roi))))
})

test_that("min StdDev ROI result has no NA weights", {
  expect_false(any(is.na(extractWeights(opt_minsd_roi))))
})

test_that("quadratic utility ROI result has no NA weights", {
  expect_false(any(is.na(extractWeights(opt_qu_roi))))
})

test_that("extractStats returns a named numeric vector for each ROI result", {
  expect_true(is.numeric(extractStats(opt_maxret_roi)))
  expect_true(!is.null(names(extractStats(opt_maxret_roi))))
  expect_true(is.numeric(extractStats(opt_minsd_roi)))
  expect_true(is.numeric(extractStats(opt_qu_roi)))
})

test_that("extractObjectiveMeasures returns a named list for each ROI result", {
  om_maxret <- extractObjectiveMeasures(opt_maxret_roi)
  om_minsd  <- extractObjectiveMeasures(opt_minsd_roi)
  om_qu     <- extractObjectiveMeasures(opt_qu_roi)
  expect_true(is.list(om_maxret))
  expect_true(is.list(om_minsd))
  expect_true(is.list(om_qu))
  expect_false(is.null(om_maxret$mean))
  expect_false(is.null(om_minsd$StdDev))
})
