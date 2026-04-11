###############################################################################
# tests/testthat/test-optFUN-milp-toc-leverage.R
#
# Coverage targets (R/optFUN.R — 77.08% baseline, ~157 uncovered exprs):
#
#   maxret_milp_opt  L296-L406  (entirely uncovered)
#   etl_milp_opt     L520-L671  (entirely uncovered)
#   gmv_opt_toc      L688-L834  (entirely uncovered)
#   gmv_opt_leverage L1012-L1162 (entirely uncovered)
#
# These functions are dispatched from optimize.portfolio_v2 (ROI solver path)
# when the matching constraint is present in the portfolio spec.
#
# Dispatch conditions (from optimize.portfolio.R):
#   maxret_milp_opt  : only "mean" objective + max_pos constraint
#   etl_milp_opt     : ETL/ES/CVaR objective + max_pos constraint
#   gmv_opt_toc      : variance/StdDev objective + turnover constraint
#   gmv_opt_leverage : variance/StdDev objective + leverage_exposure constraint
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("PerformanceAnalytics")
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")

utils::data(edhec)
R4 <- edhec[, 1:4]
nms <- colnames(R4)

# ===========================================================================
# Section 1: maxret_milp_opt (L296-L406)
#
# Dispatch: only "mean" return objective + position_limit (max_pos) constraint
# Solver: glpk (MILP — mixed-integer linear programming)
# ===========================================================================

portf_maxret_milp <- portfolio.spec(nms)
portf_maxret_milp <- add.constraint(portf_maxret_milp, type = "full_investment")
portf_maxret_milp <- add.constraint(portf_maxret_milp, type = "long_only")
portf_maxret_milp <- add.constraint(portf_maxret_milp, type = "position_limit",
                                    max_pos = 3L)
portf_maxret_milp <- add.objective(portf_maxret_milp, type = "return", name = "mean")

opt_maxret_milp <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_maxret_milp, optimize_method = "ROI")
))

test_that("maxret_milp_opt: returns optimize.portfolio.ROI object", {
  expect_s3_class(opt_maxret_milp, "optimize.portfolio.ROI")
})

test_that("maxret_milp_opt: weights sum to 1", {
  w <- opt_maxret_milp$weights
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

test_that("maxret_milp_opt: at most max_pos non-zero weights", {
  w <- opt_maxret_milp$weights
  expect_lte(sum(w > 1e-6), 3L)
})

test_that("maxret_milp_opt: all weights non-negative (long_only)", {
  w <- opt_maxret_milp$weights
  expect_true(all(w >= -1e-8))
})

test_that("maxret_milp_opt: objective_measures contains 'mean'", {
  expect_false(is.null(opt_maxret_milp$objective_measures$mean))
})

test_that("maxret_milp_opt: with target return NA still produces result", {
  # NA target = just maximise; already exercised above — confirm explicitly
  portf_t <- portf_maxret_milp  # no return constraint object added
  opt_t <- suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_t, optimize_method = "ROI")
  ))
  expect_s3_class(opt_t, "optimize.portfolio.ROI")
})

# ===========================================================================
# Section 2: etl_milp_opt (L520-L671)
#
# Dispatch: CVaR/ETL/ES objective + position_limit (max_pos) constraint
# Solver: glpk (MILP)
# ===========================================================================

portf_etl_milp <- portfolio.spec(nms)
portf_etl_milp <- add.constraint(portf_etl_milp, type = "full_investment")
portf_etl_milp <- add.constraint(portf_etl_milp, type = "long_only")
portf_etl_milp <- add.constraint(portf_etl_milp, type = "position_limit",
                                  max_pos = 3L)
portf_etl_milp <- add.objective(portf_etl_milp, type = "risk", name = "CVaR",
                                 arguments = list(p = 0.95, clean = "none"))

opt_etl_milp <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_etl_milp, optimize_method = "ROI")
))

test_that("etl_milp_opt: returns optimize.portfolio.ROI object", {
  expect_s3_class(opt_etl_milp, "optimize.portfolio.ROI")
})

test_that("etl_milp_opt: weights sum to 1", {
  w <- opt_etl_milp$weights
  expect_equal(sum(w), 1, tolerance = 1e-4)
})

test_that("etl_milp_opt: at most max_pos non-zero weights", {
  w <- opt_etl_milp$weights
  expect_lte(sum(w > 1e-4), 3L)
})

test_that("etl_milp_opt: all weights non-negative (long_only)", {
  w <- opt_etl_milp$weights
  expect_true(all(w >= -1e-6))
})

# ETL/CVaR objective with both mean+CVaR (maxSTARR path exercises etl_milp_opt too)
portf_etl_milp2 <- portfolio.spec(nms)
portf_etl_milp2 <- add.constraint(portf_etl_milp2, type = "full_investment")
portf_etl_milp2 <- add.constraint(portf_etl_milp2, type = "long_only")
portf_etl_milp2 <- add.constraint(portf_etl_milp2, type = "position_limit",
                                   max_pos = 3L)
portf_etl_milp2 <- add.objective(portf_etl_milp2, type = "risk",   name = "CVaR",
                                  arguments = list(p = 0.95, clean = "none"))
portf_etl_milp2 <- add.objective(portf_etl_milp2, type = "return", name = "mean")

opt_etl_milp2 <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_etl_milp2, optimize_method = "ROI",
                     maxSTARR = TRUE)
))

test_that("etl_milp_opt: maxSTARR=TRUE path returns valid result", {
  expect_s3_class(opt_etl_milp2, "optimize.portfolio.ROI")
  w <- opt_etl_milp2$weights
  expect_equal(sum(w), 1, tolerance = 1e-4)
})

# ===========================================================================
# Section 3: gmv_opt_toc (L688-L834)
#
# Dispatch: variance/StdDev objective + turnover constraint
# Solver: quadprog (QP with turnover via extra variables)
# ===========================================================================

skip_if_not_installed("ROI.plugin.quadprog")

portf_toc <- portfolio.spec(nms)
portf_toc <- add.constraint(portf_toc, type = "full_investment")
portf_toc <- add.constraint(portf_toc, type = "long_only")
portf_toc <- add.constraint(portf_toc, type = "turnover", turnover_target = 0.5)
portf_toc <- add.objective(portf_toc, type = "risk", name = "StdDev")

opt_toc <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_toc, optimize_method = "ROI")
))

test_that("gmv_opt_toc: returns optimize.portfolio.ROI object", {
  expect_s3_class(opt_toc, "optimize.portfolio.ROI")
})

test_that("gmv_opt_toc: weights sum to 1", {
  w <- opt_toc$weights
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

test_that("gmv_opt_toc: all weights non-negative (long_only)", {
  w <- opt_toc$weights
  expect_true(all(w >= -1e-8))
})

test_that("gmv_opt_toc: objective_measures contains StdDev", {
  expect_false(is.null(opt_toc$objective_measures$StdDev))
})

# Also with both mean + StdDev objectives (quadratic utility path)
portf_toc2 <- portfolio.spec(nms)
portf_toc2 <- add.constraint(portf_toc2, type = "full_investment")
portf_toc2 <- add.constraint(portf_toc2, type = "long_only")
portf_toc2 <- add.constraint(portf_toc2, type = "turnover", turnover_target = 0.5)
portf_toc2 <- add.objective(portf_toc2, type = "risk",   name = "StdDev")
portf_toc2 <- add.objective(portf_toc2, type = "return", name = "mean")

opt_toc2 <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_toc2, optimize_method = "ROI")
))

test_that("gmv_opt_toc: mean+StdDev path returns valid result", {
  expect_s3_class(opt_toc2, "optimize.portfolio.ROI")
  w <- opt_toc2$weights
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

test_that("gmv_opt_toc: mean+StdDev path includes mean in objective_measures", {
  expect_false(is.null(opt_toc2$objective_measures$mean))
})

# ===========================================================================
# Section 4: gmv_opt_leverage (L1012-L1162)
#
# Dispatch: variance/StdDev objective + leverage_exposure constraint
# Solver: quadprog (QP)
# ===========================================================================

portf_lev <- portfolio.spec(nms)
portf_lev <- add.constraint(portf_lev, type = "full_investment")
portf_lev <- add.constraint(portf_lev, type = "box", min = -0.1, max = 0.6)
portf_lev <- add.constraint(portf_lev, type = "leverage_exposure", leverage = 1.5)
portf_lev <- add.objective(portf_lev, type = "risk", name = "StdDev")

opt_lev <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_lev, optimize_method = "ROI")
))

test_that("gmv_opt_leverage: returns optimize.portfolio.ROI object", {
  expect_s3_class(opt_lev, "optimize.portfolio.ROI")
})

test_that("gmv_opt_leverage: weights sum to 1", {
  w <- opt_lev$weights
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

test_that("gmv_opt_leverage: leverage (sum |w|) is at most leverage limit", {
  w   <- opt_lev$weights
  lev <- sum(abs(w))
  expect_lte(lev, 1.5 + 1e-6)
})

test_that("gmv_opt_leverage: objective_measures contains StdDev", {
  expect_false(is.null(opt_lev$objective_measures$StdDev))
})

# Also with mean + StdDev (quadratic utility path)
portf_lev2 <- portfolio.spec(nms)
portf_lev2 <- add.constraint(portf_lev2, type = "full_investment")
portf_lev2 <- add.constraint(portf_lev2, type = "box", min = -0.1, max = 0.6)
portf_lev2 <- add.constraint(portf_lev2, type = "leverage_exposure", leverage = 1.5)
portf_lev2 <- add.objective(portf_lev2, type = "risk",   name = "StdDev")
portf_lev2 <- add.objective(portf_lev2, type = "return", name = "mean")

opt_lev2 <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_lev2, optimize_method = "ROI")
))

test_that("gmv_opt_leverage: mean+StdDev path returns valid result", {
  expect_s3_class(opt_lev2, "optimize.portfolio.ROI")
  w <- opt_lev2$weights
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

test_that("gmv_opt_leverage: mean+StdDev path includes mean in objective_measures", {
  expect_false(is.null(opt_lev2$objective_measures$mean))
})
