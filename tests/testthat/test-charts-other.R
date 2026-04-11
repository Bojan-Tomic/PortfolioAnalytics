###############################################################################
# tests/testthat/test-charts-other.R
#
# Source files covered:
#   R/charts.PSO.R         — chart.Weight.pso, chart.Scatter.pso,
#                             charts.pso, plot.optimize.portfolio.pso
#   R/charts.GenSA.R       — chart.Weight.GenSA, chart.Scatter.GenSA,
#                             charts.GenSA, plot.optimize.portfolio.GenSA
#   R/chart.Weights.R      — chart.Weights generic, barplotWeights,
#                             chart.Weights.optimize.portfolio.rebalancing
#   R/chart.concentration.R — chart.Concentration
#   R/chart.RiskReward.R   — chart.RiskReward generic
#
# All chart tests redirect graphics to a null PDF device.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Local fixtures
# ---------------------------------------------------------------------------

# PSO result WITH trace=TRUE (for chart.Scatter.pso which accesses object$R
# and calls extractStats, which requires object$PSOoutput).
# Uses a mean+StdDev spec so extractStats has a "mean" column.
opt_pso_trace <- NULL
if (requireNamespace("pso", quietly = TRUE)) {
  p_pso <- portfolio.spec(assets = colnames(edhec4))
  p_pso <- add.constraint(p_pso, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_pso <- add.constraint(p_pso, type = "box",        min = 0.05,     max = 0.60)
  p_pso <- add.objective(p_pso,  type = "return",     name = "mean")
  p_pso <- add.objective(p_pso,  type = "risk",       name = "StdDev")
  opt_pso_trace <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, p_pso,
                       optimize_method = "pso",
                       trace           = TRUE,
                       maxit           = 50)
  }, error = function(e) NULL)
}

# GenSA result WITH trace=TRUE (for chart.Scatter.GenSA which accesses
# object$R and computes risk/return via applyFUN).
# Uses a mean+StdDev spec so return.col="mean" and risk.col="StdDev" resolve.
opt_gensa_trace <- NULL
if (requireNamespace("GenSA", quietly = TRUE)) {
  p_gensa <- portfolio.spec(assets = colnames(edhec4))
  p_gensa <- add.constraint(p_gensa, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_gensa <- add.constraint(p_gensa, type = "box",        min = 0.05,     max = 0.60)
  p_gensa <- add.objective(p_gensa,  type = "return",     name = "mean")
  p_gensa <- add.objective(p_gensa,  type = "risk",       name = "StdDev")
  opt_gensa_trace <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, p_gensa,
                       optimize_method = "GenSA",
                       trace           = TRUE,
                       maxit           = 30)
  }, error = function(e) NULL)
}

# Rebalancing result for chart.Weights.optimize.portfolio.rebalancing.
# Uses ROI for speed; short data window (2019–2021) with annual rebalancing
# and a 12-month training window gives a handful of rebalance periods.
opt_rebal <- tryCatch({
  R_short <- edhec4["2019/2021"]
  p_r <- portfolio.spec(assets = colnames(R_short))
  p_r <- add.constraint(p_r, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_r <- add.constraint(p_r, type = "box",        min = 0,         max = 1)
  p_r <- add.objective(p_r,  type = "return",     name = "mean")
  p_r <- add.objective(p_r,  type = "risk",       name = "StdDev")
  optimize.portfolio.rebalancing(R_short, p_r,
                                 optimize_method = "ROI",
                                 rebalance_on    = "years",
                                 training_period = 12)
}, error = function(e) NULL)

# ===========================================================================
# Section 1: charts.PSO.R
# ===========================================================================

# ---------------------------------------------------------------------------
# 1.1  chart.Weight.pso — line plot
# ---------------------------------------------------------------------------

test_that("chart.Weight.pso renders a line plot without error", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.pso(opt_pso, plot.type = "line")
  )
})

# ---------------------------------------------------------------------------
# 1.2  chart.Weight.pso — barplot (also exercises barplotWeights)
# ---------------------------------------------------------------------------

test_that("chart.Weight.pso renders a barplot without error", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.pso(opt_pso, plot.type = "barplot")
  )
})

# ---------------------------------------------------------------------------
# 1.3  chart.Weights.optimize.portfolio.pso — S3 dispatch alias
# ---------------------------------------------------------------------------

test_that("chart.Weights dispatches to the pso method without error", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_pso, plot.type = "line")
  )
})

# ---------------------------------------------------------------------------
# 1.4  chart.Scatter.pso — requires trace=TRUE (object$R + PSOoutput)
#      return.col="mean" and risk.col="StdDev" match the p_pso spec above.
# ---------------------------------------------------------------------------

test_that("chart.Scatter.pso renders without error using opt_pso_trace", {
  skip_if(is.null(opt_pso_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.pso(opt_pso_trace,
                      return.col = "mean",
                      risk.col   = "StdDev")
  )
})

# ---------------------------------------------------------------------------
# 1.5  charts.pso — combined scatter + weight layout wrapper
# ---------------------------------------------------------------------------

test_that("charts.pso renders combined scatter and weight charts without error", {
  skip_if(is.null(opt_pso_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.pso(opt_pso_trace,
               return.col = "mean",
               risk.col   = "StdDev")
  )
})

# ---------------------------------------------------------------------------
# 1.6  plot.optimize.portfolio.pso — S3 plot() wrapper for charts.pso
# ---------------------------------------------------------------------------

test_that("plot.optimize.portfolio.pso renders without error", {
  skip_if(is.null(opt_pso_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plot(opt_pso_trace,
         return.col = "mean",
         risk.col   = "StdDev")
  )
})

# ===========================================================================
# Section 2: charts.GenSA.R
# ===========================================================================

# ---------------------------------------------------------------------------
# 2.1  chart.Weight.GenSA — line plot
# ---------------------------------------------------------------------------

test_that("chart.Weight.GenSA renders a line plot without error", {
  skip_if_not_installed("GenSA")
  skip_if(is.null(opt_gensa))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.GenSA(opt_gensa, plot.type = "line")
  )
})

# ---------------------------------------------------------------------------
# 2.2  chart.Weight.GenSA — barplot (also exercises barplotWeights)
# ---------------------------------------------------------------------------

test_that("chart.Weight.GenSA renders a barplot without error", {
  skip_if_not_installed("GenSA")
  skip_if(is.null(opt_gensa))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.GenSA(opt_gensa, plot.type = "barplot")
  )
})

# ---------------------------------------------------------------------------
# 2.3  chart.Weights.optimize.portfolio.GenSA — S3 dispatch alias
# ---------------------------------------------------------------------------

test_that("chart.Weights dispatches to the GenSA method without error", {
  skip_if_not_installed("GenSA")
  skip_if(is.null(opt_gensa))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_gensa, plot.type = "line")
  )
})

# ---------------------------------------------------------------------------
# 2.4  chart.Scatter.GenSA — requires trace=TRUE (object$R)
#      return.col="mean" and risk.col="StdDev" match the p_gensa spec above.
# ---------------------------------------------------------------------------

test_that("chart.Scatter.GenSA renders without error using opt_gensa_trace", {
  skip_if(is.null(opt_gensa_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.GenSA(opt_gensa_trace,
                        return.col = "mean",
                        risk.col   = "StdDev")
  )
})

# ---------------------------------------------------------------------------
# 2.5  charts.GenSA — combined scatter + weight layout wrapper
# ---------------------------------------------------------------------------

test_that("charts.GenSA renders combined scatter and weight charts without error", {
  skip_if(is.null(opt_gensa_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.GenSA(opt_gensa_trace,
                 return.col = "mean",
                 risk.col   = "StdDev")
  )
})

# ---------------------------------------------------------------------------
# 2.6  plot.optimize.portfolio.GenSA — S3 plot() wrapper for charts.GenSA
# ---------------------------------------------------------------------------

test_that("plot.optimize.portfolio.GenSA renders without error", {
  skip_if(is.null(opt_gensa_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plot(opt_gensa_trace,
         return.col = "mean",
         risk.col   = "StdDev")
  )
})

# ===========================================================================
# Section 3: chart.Weights.R
# ===========================================================================

# ---------------------------------------------------------------------------
# 3.1  chart.Weights generic — dispatches to chart.Weights.optimize.portfolio.ROI
#      line plot type
# ---------------------------------------------------------------------------

test_that("chart.Weights generic dispatches to the ROI method for a line chart without error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "line")
  )
})

# ---------------------------------------------------------------------------
# 3.2  chart.Weights generic — barplot type (also exercises barplotWeights)
# ---------------------------------------------------------------------------

test_that("chart.Weights generic dispatches to barplotWeights for a barplot without error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "barplot")
  )
})

# ---------------------------------------------------------------------------
# 3.3  chart.Weights.optimize.portfolio.rebalancing
#      Uses chart.StackedBar from PerformanceAnalytics internally.
# ---------------------------------------------------------------------------

test_that("chart.Weights.optimize.portfolio.rebalancing renders a stacked bar chart without error", {
  skip_if(is.null(opt_rebal))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_rebal)
  )
})

# ===========================================================================
# Section 4: chart.concentration.R
# ===========================================================================

# ---------------------------------------------------------------------------
# 4.1  chart.Concentration — conc.type = "weights" (default)
#      opt_rp_trace has mean+ES objectives with trace=TRUE so extractStats
#      returns "mean", "ES", and "w.*" columns — all required here.
# ---------------------------------------------------------------------------

test_that("chart.Concentration renders with conc.type weights without error", {
  skip_if(is.null(opt_rp_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Concentration(opt_rp_trace,
                        return.col = "mean",
                        risk.col   = "ES",
                        conc.type  = "weights")
  )
})

# ---------------------------------------------------------------------------
# 4.2  chart.Concentration — conc.type = "pct_contrib"
#      pct_contrib columns are only produced by risk_budget optimisations.
#      opt_rp_trace uses a plain risk objective, so no pct_contrib columns
#      are expected; this test is skipped if they are absent.
# ---------------------------------------------------------------------------

test_that("chart.Concentration with conc.type pct_contrib is skipped when no pct_contrib columns exist", {
  skip_if(is.null(opt_rp_trace))
  xtract <- extractStats(opt_rp_trace)
  pct_idx <- grep("pct_contrib", colnames(xtract))
  skip_if(
    length(pct_idx) == 0,
    "no pct_contrib columns in extractStats output — not a risk_budget optimisation"
  )
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Concentration(opt_rp_trace,
                        return.col = "mean",
                        risk.col   = "ES",
                        conc.type  = "pct_contrib")
  )
})

# ===========================================================================
# Section 5: chart.RiskReward.R
# ===========================================================================

# ---------------------------------------------------------------------------
# 5.1  chart.RiskReward generic — UseMethod dispatcher
#      Calling chart.RiskReward() on an optimize.portfolio.ROI object
#      dispatches to chart.RiskReward.optimize.portfolio.ROI (= chart.Scatter.ROI).
#      opt_roi_trace has trace=TRUE so object$R is available.
#      risk.col="StdDev" matches the .portf_meansd4 spec used to build it.
# ---------------------------------------------------------------------------

test_that("chart.RiskReward generic dispatches to the ROI method without error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskReward(opt_roi_trace,
                     return.col = "mean",
                     risk.col   = "StdDev")
  )
})

# ---------------------------------------------------------------------------
# 5.2  chart.RiskReward — dispatches to the pso method via chart.Scatter.pso
#      Verifies that chart.RiskReward.optimize.portfolio.pso is registered.
# ---------------------------------------------------------------------------

test_that("chart.RiskReward dispatches to the pso method without error", {
  skip_if(is.null(opt_pso_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskReward(opt_pso_trace,
                     return.col = "mean",
                     risk.col   = "StdDev")
  )
})

# ---------------------------------------------------------------------------
# 5.3  chart.RiskReward — dispatches to the GenSA method via chart.Scatter.GenSA
#      Verifies that chart.RiskReward.optimize.portfolio.GenSA is registered.
# ---------------------------------------------------------------------------

test_that("chart.RiskReward dispatches to the GenSA method without error", {
  skip_if(is.null(opt_gensa_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskReward(opt_gensa_trace,
                     return.col = "mean",
                     risk.col   = "StdDev")
  )
})

# ===========================================================================
# Section 6: chart.Scatter.pso with neighbors argument
# ===========================================================================

test_that("chart.Scatter.pso: neighbors=3 (integer scalar) produces no error", {
  skip_if(is.null(opt_pso_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.pso(opt_pso_trace,
                      return.col = "mean",
                      risk.col   = "StdDev",
                      neighbors  = 3)
  )
})

test_that("chart.Scatter.pso: neighbors=c(1,2,3) (index vector) produces no error", {
  skip_if(is.null(opt_pso_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.pso(opt_pso_trace,
                      return.col = "mean",
                      risk.col   = "StdDev",
                      neighbors  = c(1, 2, 3))
  )
})

test_that("chart.Scatter.pso: chart.assets=TRUE produces no error", {
  skip_if(is.null(opt_pso_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.pso(opt_pso_trace,
                      return.col   = "mean",
                      risk.col     = "StdDev",
                      chart.assets = TRUE)
  )
})

test_that("charts.pso: neighbors=3 propagates through wrapper without error", {
  skip_if(is.null(opt_pso_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.pso(opt_pso_trace,
               return.col = "mean",
               risk.col   = "StdDev",
               neighbors  = 3)
  )
})

# ===========================================================================
# Section 7: chart.Concentration with conc.type = "pct_contrib"
#
# pct_contrib columns are only produced by risk_budget objectives.
# Build a dedicated DEoptim result with a risk_budget objective here at
# file scope so it is available to the test below.
# ===========================================================================

# Build a risk_budget optimization result with trace=TRUE.
# The pct_contrib columns are only produced by risk_budget objectives.
opt_rb_conc <- NULL
{
  p_rb <- portfolio.spec(assets = colnames(edhec4))
  p_rb <- add.constraint(p_rb, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_rb <- add.constraint(p_rb, type = "box", min = 0.05, max = 0.60)
  p_rb <- add.objective(p_rb, type = "return", name = "mean")
  p_rb <- add.objective(p_rb, type = "risk_budget", name = "ES",
                        min_concentration = TRUE)
  opt_rb_conc <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, p_rb,
                       optimize_method = "DEoptim",
                       trace       = TRUE,
                       itermax     = 30,
                       search_size = 200)
  }, error = function(e) NULL)
}

test_that("chart.Concentration: conc.type='pct_contrib' with risk_budget result produces no error", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_rb_conc))
  # Verify pct_contrib columns exist
  xtract <- extractStats(opt_rb_conc)
  pct_idx <- grep("pct_contrib", colnames(xtract))
  skip_if(length(pct_idx) == 0, "no pct_contrib columns — risk_budget objective did not store them")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Concentration(opt_rb_conc,
                        return.col = "mean",
                        risk.col   = "ES",
                        conc.type  = "pct_contrib")
  )
})
