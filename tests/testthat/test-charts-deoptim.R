###############################################################################
# tests/testthat/test-charts-deoptim.R
#
# Source files covered:
#   R/charts.DE.R     — chart.Weight.DE, chart.Scatter.DE,
#                        charts.DE, plot.optimize.portfolio.DEoptim
#   R/charts.risk.R   — chart.RiskBudget (single-period, rebalancing, opt.list)
#
# All chart tests redirect graphics to a null PDF device.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

library(PortfolioAnalytics)

skip_if_not_installed("DEoptim")

# =============================================================================
# File-scope fixtures — built once, reused across all test_that blocks.
# =============================================================================

# DEoptim result with trace=TRUE  (for chart.Scatter.DE and charts.DE wrapper)
# Keep itermax small (50) for speed. Portfolio needs mean + ES objectives so
# extractStats() output has "mean" and "ES" columns.
opt_de_trace <- tryCatch({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "ES")
  set.seed(42)
  optimize.portfolio(edhec4, p,
                     optimize_method = "DEoptim",
                     trace  = TRUE,
                     search_size = 200,
                     itermax = 50)
}, error = function(e) NULL)

# Risk budget optimization result (for chart.RiskBudget.optimize.portfolio)
# Uses DEoptim (fast, small itermax). Needs a risk_budget_objective.
opt_rb <- tryCatch({
  p_rb <- portfolio.spec(assets = colnames(edhec4))
  p_rb <- add.constraint(p_rb, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_rb <- add.constraint(p_rb, type = "box", min = 0.05, max = 0.40)
  p_rb <- add.objective(p_rb, type = "risk_budget", name = "ES",
                        min_concentration = TRUE)
  set.seed(42)
  optimize.portfolio(edhec4, p_rb,
                     optimize_method = "DEoptim",
                     search_size = 200,
                     itermax = 50)
}, error = function(e) NULL)

# opt.list with risk budget (for chart.RiskBudget.opt.list)
# Two separate risk-budget optimizations bundled into an opt.list
opt_rb_list <- tryCatch({
  p_rb2 <- portfolio.spec(assets = colnames(edhec4))
  p_rb2 <- add.constraint(p_rb2, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_rb2 <- add.constraint(p_rb2, type = "box", min = 0.05, max = 0.60)
  p_rb2 <- add.objective(p_rb2, type = "risk_budget", name = "ES",
                         min_concentration = TRUE)
  set.seed(99)
  opt2 <- optimize.portfolio(edhec4, p_rb2,
                              optimize_method = "DEoptim",
                              search_size = 200,
                              itermax = 50)
  combine.optimizations(list(opt1 = opt_rb, opt2 = opt2))
}, error = function(e) NULL)

# Rebalancing result with risk budget (for chart.RiskBudget.optimize.portfolio.rebalancing)
# Use a short sub-period of edhec to keep it fast (2 rebalance periods)
opt_rb_rebal <- tryCatch({
  R_short <- edhec4["2019/2021"]   # ~28 months
  p_rbr <- portfolio.spec(assets = colnames(R_short))
  p_rbr <- add.constraint(p_rbr, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_rbr <- add.constraint(p_rbr, type = "box", min = 0.05, max = 0.60)
  p_rbr <- add.objective(p_rbr, type = "risk_budget", name = "ES",
                          min_concentration = TRUE)
  set.seed(42)
  optimize.portfolio.rebalancing(R_short, p_rbr,
                                  optimize_method = "DEoptim",
                                  rebalance_on   = "years",
                                  training_period = 12,
                                  search_size = 100,
                                  itermax = 30)
}, error = function(e) NULL)

# =============================================================================
# Fixture integrity checks — no graphics; verify objects built correctly.
# =============================================================================

test_that("opt_de_trace is an optimize.portfolio.DEoptim object", {
  skip_if(is.null(opt_de_trace))
  expect_s3_class(opt_de_trace, "optimize.portfolio.DEoptim")
})

test_that("opt_de_trace$R is available because trace=TRUE", {
  skip_if(is.null(opt_de_trace))
  expect_false(is.null(opt_de_trace$R))
  expect_true(is.numeric(opt_de_trace$R))
})

test_that("opt_de_trace extractStats has mean and ES columns", {
  skip_if(is.null(opt_de_trace))
  stats_cols <- colnames(extractStats(opt_de_trace))
  expect_true(any(grepl("mean", stats_cols)),
              label = "extractStats has 'mean' column")
  expect_true(any(grepl("ES",   stats_cols)),
              label = "extractStats has 'ES' column")
})

test_that("opt_rb is an optimize.portfolio.DEoptim object", {
  skip_if(is.null(opt_rb))
  expect_s3_class(opt_rb, "optimize.portfolio.DEoptim")
})

test_that("opt_rb objective_measures contains ES with contribution and pct_contrib", {
  skip_if(is.null(opt_rb))
  om <- opt_rb$objective_measures
  expect_false(is.null(om$ES),
               label = "opt_rb$objective_measures$ES exists")
  # min_concentration = TRUE populates at least contribution and pct_contrib
  expect_true(length(om$ES) > 1,
              label = "ES objective_measure has contribution and pct_contrib slots")
})

test_that("opt_rb_list is an opt.list object", {
  skip_if(is.null(opt_rb_list))
  expect_s3_class(opt_rb_list, "opt.list")
  expect_length(opt_rb_list, 2L)
})

test_that("opt_rb_rebal is an optimize.portfolio.rebalancing object", {
  skip_if(is.null(opt_rb_rebal))
  expect_s3_class(opt_rb_rebal, "optimize.portfolio.rebalancing")
})

# =============================================================================
# charts.DE.R — chart.Weight.DE
# =============================================================================

test_that("chart.Weight.DE plot.type='line' (default) does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.DE(opt_de_trace, plot.type = "line")
  )
})

test_that("chart.Weight.DE plot.type='barplot' does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.DE(opt_de_trace, plot.type = "barplot")
  )
})

test_that("chart.Weights dispatch to chart.Weights.optimize.portfolio.DEoptim does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  # chart.Weights.optimize.portfolio.DEoptim is an alias for chart.Weight.DE
  expect_no_error(
    chart.Weights(opt_de_trace, plot.type = "line")
  )
})

# =============================================================================
# charts.DE.R — chart.Scatter.DE / chart.RiskReward.optimize.portfolio.DEoptim
# =============================================================================

test_that("chart.Scatter.DE with return.col='mean' risk.col='ES' does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.DE(opt_de_trace, return.col = "mean", risk.col = "ES")
  )
})

test_that("chart.Scatter.DE with chart.assets=FALSE does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.DE(opt_de_trace,
                     return.col   = "mean",
                     risk.col     = "ES",
                     chart.assets = FALSE)
  )
})

test_that("chart.RiskReward dispatch to chart.RiskReward.optimize.portfolio.DEoptim does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  # chart.RiskReward.optimize.portfolio.DEoptim is an alias for chart.Scatter.DE
  expect_no_error(
    chart.RiskReward(opt_de_trace, return.col = "mean", risk.col = "ES")
  )
})

# =============================================================================
# charts.DE.R — charts.DE (composite wrapper)
# =============================================================================

test_that("charts.DE does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.DE(opt_de_trace,
              risk.col     = "ES",
              return.col   = "mean",
              chart.assets = FALSE)
  )
})

test_that("charts.DE passes main argument without error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.DE(opt_de_trace,
              risk.col     = "ES",
              return.col   = "mean",
              chart.assets = FALSE,
              main         = "Test DE Portfolio")
  )
})

# =============================================================================
# charts.DE.R — plot.optimize.portfolio.DEoptim (S3 plot method)
# =============================================================================

test_that("plot.optimize.portfolio.DEoptim does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plot(opt_de_trace,
         return.col   = "mean",
         risk.col     = "ES",
         chart.assets = FALSE)
  )
})

test_that("plot.optimize.portfolio.DEoptim with default return.col and risk.col does not error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  # Default args: return.col='mean', risk.col='ES' — matches our objectives
  expect_no_error(plot(opt_de_trace))
})

# =============================================================================
# charts.risk.R — chart.RiskBudget.optimize.portfolio (single-period)
# =============================================================================

test_that("chart.RiskBudget.optimize.portfolio absolute risk does not error", {
  skip_if(is.null(opt_rb))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb, risk.type = "absolute")
  )
})

test_that("chart.RiskBudget.optimize.portfolio pct_contrib risk does not error", {
  skip_if(is.null(opt_rb))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb, risk.type = "pct_contrib")
  )
})

test_that("chart.RiskBudget.optimize.portfolio absolute with custom main does not error", {
  skip_if(is.null(opt_rb))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb, risk.type = "absolute", main = "ES Risk Contribution")
  )
})

# =============================================================================
# charts.risk.R — chart.RiskBudget.optimize.portfolio.rebalancing
# =============================================================================

test_that("chart.RiskBudget rebalancing absolute risk does not error", {
  skip_if(is.null(opt_rb_rebal))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb_rebal,
                     match.col  = "ES",
                     risk.type  = "absolute")
  )
})

test_that("chart.RiskBudget rebalancing pct_contrib risk does not error", {
  skip_if(is.null(opt_rb_rebal))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb_rebal,
                     match.col  = "ES",
                     risk.type  = "pct_contrib")
  )
})

test_that("chart.RiskBudget rebalancing absolute with custom main does not error", {
  skip_if(is.null(opt_rb_rebal))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb_rebal,
                     match.col  = "ES",
                     risk.type  = "absolute",
                     main       = "ES Contribution Over Time")
  )
})

# =============================================================================
# charts.risk.R — chart.RiskBudget.opt.list / barplotRiskBudget
# =============================================================================

test_that("chart.RiskBudget.opt.list plot.type='line' absolute does not error", {
  skip_if(is.null(opt_rb_list))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb_list,
                     match.col  = "ES",
                     risk.type  = "absolute",
                     plot.type  = "line")
  )
})

test_that("chart.RiskBudget.opt.list plot.type='line' pct_contrib does not error", {
  skip_if(is.null(opt_rb_list))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb_list,
                     match.col  = "ES",
                     risk.type  = "pct_contrib",
                     plot.type  = "line")
  )
})

test_that("chart.RiskBudget.opt.list plot.type='barplot' absolute (barplotRiskBudget) does not error", {
  skip_if(is.null(opt_rb_list))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb_list,
                     match.col  = "ES",
                     risk.type  = "absolute",
                     plot.type  = "barplot")
  )
})

test_that("chart.RiskBudget.opt.list plot.type='barplot' pct_contrib (barplotRiskBudget) does not error", {
  skip_if(is.null(opt_rb_list))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb_list,
                     match.col  = "ES",
                     risk.type  = "pct_contrib",
                     plot.type  = "barplot")
  )
})

test_that("chart.RiskBudget.opt.list with legend does not error", {
  skip_if(is.null(opt_rb_list))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskBudget(opt_rb_list,
                     match.col   = "ES",
                     risk.type   = "absolute",
                     plot.type   = "line",
                     legend.loc  = "topright")
  )
})

# ===========================================================================
# New Section: chart.Scatter.DE with neighbors argument
# ===========================================================================

test_that("chart.Scatter.DE: neighbors=3 (integer scalar) produces no error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.DE(opt_de_trace,
                     return.col = "mean",
                     risk.col   = "ES",
                     neighbors  = 3)
  )
})

test_that("chart.Scatter.DE: neighbors=c(1,2,3) (index vector) produces no error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.DE(opt_de_trace,
                     return.col = "mean",
                     risk.col   = "ES",
                     neighbors  = c(1, 2, 3))
  )
})

test_that("chart.Scatter.DE: chart.assets=TRUE produces no error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.DE(opt_de_trace,
                     return.col   = "mean",
                     risk.col     = "ES",
                     chart.assets = TRUE)
  )
})

test_that("charts.DE: neighbors=3 propagates through wrapper without error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.DE(opt_de_trace,
              risk.col     = "ES",
              return.col   = "mean",
              neighbors    = 3,
              chart.assets = FALSE)
  )
})

test_that("chart.Weight.DE: neighbors=3 in weight chart produces no error", {
  skip_if(is.null(opt_de_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.DE(opt_de_trace,
                    neighbors = 3)
  )
})
