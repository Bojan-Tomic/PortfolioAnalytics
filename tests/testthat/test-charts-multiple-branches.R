###############################################################################
# tests/testthat/test-charts-multiple-branches.R
#
# Source file: R/charts.multiple.R
#
# Targeted branches:
#   chart.RiskReward.opt.list():
#     - chart.assets=TRUE  (lines 80-111, 141-143)
#     - labels.assets=FALSE (line 138 skipped)
#     - explicit xlim / ylim (lines 124-134 else branches bypassed)
#
#   chart.Weights.opt.list():
#     - plot.type="bar" / "barplot"  (barplotOptWeights path, line 10)
#     - labels.assets not applicable; legend.loc=NULL (line 44 skipped)
#
# All chart tests use dev.off(pdf(NULL)) to suppress graphical output.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Fixtures: two optimize.portfolio (ROI) results to form an opt.list
# ---------------------------------------------------------------------------

utils::data(edhec)
R4 <- edhec[, 1:4]

p_minsd <- portfolio.spec(assets = colnames(R4))
p_minsd <- add.constraint(p_minsd, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
p_minsd <- add.constraint(p_minsd, type = "box", min = 0, max = 1)
p_minsd <- add.objective(p_minsd, type = "return", name = "mean")
p_minsd <- add.objective(p_minsd, type = "risk",   name = "ES")

p_maxret <- portfolio.spec(assets = colnames(R4))
p_maxret <- add.constraint(p_maxret, type = "weight_sum",
                            min_sum = 0.99, max_sum = 1.01)
p_maxret <- add.constraint(p_maxret, type = "box", min = 0, max = 1)
p_maxret <- add.objective(p_maxret, type = "return", name = "mean")
p_maxret <- add.objective(p_maxret, type = "risk",   name = "ES")

opt1 <- tryCatch(
  optimize.portfolio(R4, p_minsd,  optimize_method = "ROI", trace = TRUE),
  error = function(e) NULL
)
opt2 <- tryCatch(
  optimize.portfolio(R4, p_maxret, optimize_method = "ROI", trace = TRUE),
  error = function(e) NULL
)

skip_if(is.null(opt1) || is.null(opt2), "ROI optimization failed; skipping chart tests")

optlist <- list(minES = opt1, maxRet = opt2)
class(optlist) <- "opt.list"

# Helper: run a plot expression capturing any errors; suppress device output
plot_ok <- function(expr) {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  tryCatch({ force(expr); TRUE }, error = function(e) { message(e); FALSE })
}


# ===========================================================================
# 1. chart.RiskReward.opt.list — baseline (labels.assets=TRUE, no custom axes)
# ===========================================================================

test_that("chart.RiskReward.opt.list: default call runs without error", {
  expect_true(plot_ok(
    chart.RiskReward(optlist, risk.col = "ES", return.col = "mean")
  ))
})

test_that("chart.RiskReward.opt.list: labels.assets=FALSE runs without error", {
  expect_true(plot_ok(
    chart.RiskReward(optlist,
                     risk.col      = "ES",
                     return.col    = "mean",
                     labels.assets = FALSE)
  ))
})

test_that("chart.RiskReward.opt.list: explicit xlim and ylim run without error", {
  expect_true(plot_ok(
    chart.RiskReward(optlist,
                     risk.col   = "ES",
                     return.col = "mean",
                     xlim       = c(0, 0.05),
                     ylim       = c(0, 0.02))
  ))
})

test_that("chart.RiskReward.opt.list: chart.assets=TRUE runs without error", {
  expect_true(plot_ok(
    chart.RiskReward(optlist,
                     risk.col     = "ES",
                     return.col   = "mean",
                     chart.assets = TRUE)
  ))
})

test_that("chart.RiskReward.opt.list: chart.assets=TRUE with custom axes runs without error", {
  expect_true(plot_ok(
    chart.RiskReward(optlist,
                     risk.col     = "ES",
                     return.col   = "mean",
                     chart.assets = TRUE,
                     xlim         = c(0, 0.05),
                     ylim         = c(0, 0.02))
  ))
})

test_that("chart.RiskReward.opt.list: errors on non-opt.list input", {
  # class "list" has no chart.RiskReward method → UseMethod error
  expect_error(chart.RiskReward(list()))
})

test_that("chart.RiskReward.opt.list: errors on invalid risk.col", {
  pdf(NULL); on.exit(dev.off())
  expect_error(
    chart.RiskReward(optlist, risk.col = "FAKE", return.col = "mean"),
    regexp = "not in column names"
  )
})

test_that("chart.RiskReward.opt.list: errors on invalid return.col", {
  pdf(NULL); on.exit(dev.off())
  expect_error(
    chart.RiskReward(optlist, risk.col = "ES", return.col = "FAKE"),
    regexp = "not in column names"
  )
})


# ===========================================================================
# 2. chart.Weights.opt.list — line (default) and bar plot types
# ===========================================================================

test_that("chart.Weights.opt.list: default (line) plot runs without error", {
  expect_true(plot_ok(
    chart.Weights(optlist)
  ))
})

test_that("chart.Weights.opt.list: plot.type='bar' runs without error", {
  expect_true(plot_ok(
    chart.Weights(optlist, plot.type = "bar")
  ))
})

test_that("chart.Weights.opt.list: plot.type='barplot' alias runs without error", {
  expect_true(plot_ok(
    chart.Weights(optlist, plot.type = "barplot")
  ))
})

test_that("chart.Weights.opt.list: legend.loc=NULL runs without error", {
  expect_true(plot_ok(
    chart.Weights(optlist, legend.loc = NULL)
  ))
})

test_that("chart.Weights.opt.list: errors on non-opt.list input", {
  # plain list has no chart.Weights method → UseMethod dispatch error
  expect_error(chart.Weights(list()))
})
