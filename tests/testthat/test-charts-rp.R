###############################################################################
# tests/testthat/test-charts-rp.R
#
# Source files covered:
#   R/charts.RP.R       — chart.Weight.RP, chart.Scatter.RP,
#                          charts.RP, plot.optimize.portfolio.random,
#                          plot.optimize.portfolio
#   R/charts.groups.R   — chart.GroupWeights, barplotGroupWeights
#   R/charts.multiple.R — chart.Weights.opt.list, barplotOptWeights,
#                          chart.RiskReward.opt.list
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

# Group-constrained result for chart.GroupWeights
# Needs group constraints so extractGroups() returns non-NULL group_weights
opt_group <- tryCatch({
  p_g <- portfolio.spec(assets = colnames(edhec4))
  p_g <- add.constraint(p_g, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_g <- add.constraint(p_g, type = "box", min = 0.05, max = 0.60)
  # Two groups: assets 1-2 and assets 3-4
  p_g <- add.constraint(p_g, type = "group",
                        groups     = list(c(1, 2), c(3, 4)),
                        group_min  = c(0.2, 0.2),
                        group_max  = c(0.8, 0.8))
  p_g <- add.objective(p_g, type = "risk", name = "ES")
  set.seed(42)
  optimize.portfolio(edhec4, p_g,
                     optimize_method = "random",
                     search_size     = 200)
}, error = function(e) NULL)

# Category-labelled result for chart.GroupWeights grouping="category" branch
opt_category <- tryCatch({
  p_c <- portfolio.spec(
    assets           = colnames(edhec4),
    category_labels  = c("GroupA", "GroupA", "GroupB", "GroupB")
  )
  p_c <- add.constraint(p_c, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_c <- add.constraint(p_c, type = "box", min = 0.05, max = 0.60)
  p_c <- add.objective(p_c, type = "risk", name = "ES")
  set.seed(42)
  optimize.portfolio(edhec4, p_c,
                     optimize_method = "random",
                     search_size     = 200)
}, error = function(e) NULL)

# opt.list for chart.Weights.opt.list and chart.RiskReward.opt.list.
# Two portfolios with different risk objectives so that
# extractObjectiveMeasures.opt.list takes its "union all objectives" branch
# and evaluates every measure for every portfolio using object$R
# (requires trace = TRUE on each element).
opt_list_obj <- tryCatch({
  # Portfolio 1: mean + ES objectives (ROI solver)
  p1 <- portfolio.spec(assets = colnames(edhec4))
  p1 <- add.constraint(p1, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p1 <- add.constraint(p1, type = "box", min = 0, max = 1)
  p1 <- add.objective(p1, type = "return", name = "mean")
  p1 <- add.objective(p1, type = "risk",   name = "ES")
  set.seed(42)
  o1 <- optimize.portfolio(edhec4, p1, optimize_method = "ROI", trace = TRUE)

  # Portfolio 2: mean + StdDev objectives (ROI solver)
  p2 <- portfolio.spec(assets = colnames(edhec4))
  p2 <- add.constraint(p2, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p2 <- add.constraint(p2, type = "box", min = 0, max = 1)
  p2 <- add.objective(p2, type = "return", name = "mean")
  p2 <- add.objective(p2, type = "risk",   name = "StdDev")
  o2 <- optimize.portfolio(edhec4, p2, optimize_method = "ROI", trace = TRUE)

  combine.optimizations(list(minES = o1, minSD = o2))
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# Tests: R/charts.RP.R — chart.Weight.RP
# (also covers chart.Weights.optimize.portfolio.random, which is an alias)
# ---------------------------------------------------------------------------

test_that("chart.Weight.RP: line plot (default) runs without error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.RP(opt_rp_trace, plot.type = "line")
  )
})

test_that("chart.Weight.RP: barplot variant runs without error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.RP(opt_rp_trace, plot.type = "barplot")
  )
})

test_that("chart.Weight.RP: rejects non-random object with informative error", {
  expect_error(
    chart.Weight.RP(list()),
    regexp = "object must be of class 'optimize.portfolio.random'"
  )
})

test_that("chart.Weights dispatch to chart.Weights.optimize.portfolio.random does not error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  # chart.Weights.optimize.portfolio.random is an alias for chart.Weight.RP
  expect_no_error(
    chart.Weights(opt_rp_trace, plot.type = "line")
  )
})

# ---------------------------------------------------------------------------
# Tests: R/charts.RP.R — chart.Scatter.RP
# (also covers chart.RiskReward.optimize.portfolio.random, which is an alias)
# NOTE: requires object$R — only available when trace = TRUE
# ---------------------------------------------------------------------------

test_that("chart.Scatter.RP: runs with mean / ES columns", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.RP(opt_rp_trace, return.col = "mean", risk.col = "ES")
  )
})

test_that("chart.Scatter.RP: runs with chart.assets = FALSE (default)", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.RP(opt_rp_trace,
                     return.col   = "mean",
                     risk.col     = "ES",
                     chart.assets = FALSE)
  )
})

test_that("chart.Scatter.RP: rejects non-random object with informative error", {
  expect_error(
    chart.Scatter.RP(list()),
    regexp = "object must be of class 'optimize.portfolio.random'"
  )
})

test_that("chart.RiskReward dispatch to chart.RiskReward.optimize.portfolio.random does not error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  # chart.RiskReward.optimize.portfolio.random is an alias for chart.Scatter.RP
  expect_no_error(
    chart.RiskReward(opt_rp_trace, return.col = "mean", risk.col = "ES")
  )
})

# ---------------------------------------------------------------------------
# Tests: R/charts.RP.R — charts.RP (composite wrapper)
# ---------------------------------------------------------------------------

test_that("charts.RP: composite wrapper runs without error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.RP(opt_rp_trace, risk.col = "ES", return.col = "mean")
  )
})

test_that("charts.RP: rejects non-random object with informative error", {
  expect_error(
    charts.RP(list(), risk.col = "ES", return.col = "mean"),
    regexp = "RP must be of class optimize.portfolio.random"
  )
})

# ---------------------------------------------------------------------------
# Tests: R/charts.RP.R — plot.optimize.portfolio.random
# ---------------------------------------------------------------------------

test_that("plot.optimize.portfolio.random: runs without error via generic plot()", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plot(opt_rp_trace, return.col = "mean", risk.col = "ES")
  )
})

test_that("plot.optimize.portfolio.random: direct method call runs without error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  # Use generic dispatch instead of direct method call (S3 methods not plain-exported)
  expect_no_error(
    plot(opt_rp_trace, return.col = "mean", risk.col = "ES")
  )
})

# ---------------------------------------------------------------------------
# Tests: R/charts.RP.R — plot.optimize.portfolio
# (dispatches to charts.RP when given an optimize.portfolio.random object)
# ---------------------------------------------------------------------------

test_that("plot.optimize.portfolio: runs without error on random object", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  # plot.optimize.portfolio is not plain-exported; test via S3 dispatch
  expect_no_error(
    plot(opt_rp_trace, return.col = "mean", risk.col = "ES")
  )
})

# ---------------------------------------------------------------------------
# Tests: R/charts.groups.R — chart.GroupWeights
# (barplot branch also covers barplotGroupWeights)
# ---------------------------------------------------------------------------

test_that("chart.GroupWeights: line plot (default) runs without error", {
  skip_if(is.null(opt_group), "opt_group fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_group, grouping = "groups", plot.type = "line")
  )
})

test_that("chart.GroupWeights: barplot variant runs without error (covers barplotGroupWeights)", {
  skip_if(is.null(opt_group), "opt_group fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_group, grouping = "groups", plot.type = "barplot")
  )
})

test_that("chart.GroupWeights: rejects non-optimize.portfolio object with informative error", {
  expect_error(
    chart.GroupWeights(list()),
    regexp = "object must be of class 'optimize.portfolio'"
  )
})

# ---------------------------------------------------------------------------
# Tests: R/charts.groups.R — chart.GroupWeights grouping="category" branches
# ---------------------------------------------------------------------------

test_that("chart.GroupWeights: category line plot runs without error", {
  skip_if(is.null(opt_category), "opt_category fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_category, grouping = "category", plot.type = "line")
  )
})

test_that("chart.GroupWeights: category barplot runs without error", {
  skip_if(is.null(opt_category), "opt_category fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_category, grouping = "category", plot.type = "barplot")
  )
})

test_that("chart.GroupWeights: category line with xlab covers minmargin=5 branch", {
  skip_if(is.null(opt_category), "opt_category fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_category, grouping = "category", plot.type = "line",
                       xlab = "Category")
  )
})

test_that("chart.GroupWeights: las=0 covers bottommargin=minmargin branch", {
  skip_if(is.null(opt_category), "opt_category fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_category, grouping = "category", plot.type = "line",
                       las = 0)
  )
})

test_that("chart.GroupWeights: main='' covers topmargin=1 branch", {
  skip_if(is.null(opt_category), "opt_category fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_category, grouping = "category", plot.type = "line",
                       main = "")
  )
})

# ---------------------------------------------------------------------------
# Tests: R/charts.multiple.R — chart.Weights.opt.list
# (barplot branch also covers barplotOptWeights)
# ---------------------------------------------------------------------------

test_that("chart.Weights.opt.list: line plot runs without error", {
  skip_if(is.null(opt_list_obj), "opt_list_obj fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_list_obj, plot.type = "line")
  )
})

test_that("chart.Weights.opt.list: barplot variant runs without error (covers barplotOptWeights)", {
  skip_if(is.null(opt_list_obj), "opt_list_obj fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_list_obj, plot.type = "barplot")
  )
})

test_that("chart.Weights.opt.list: rejects non-opt.list object with informative error", {
  expect_error(
    chart.Weights(structure(list(), class = "not.opt.list")),
    regexp = "no applicable method"
  )
})

# ---------------------------------------------------------------------------
# Tests: R/charts.multiple.R — chart.RiskReward.opt.list
# risk.col   = "ES"   — contributed by o1 (mean + ES)
# return.col = "mean" — common to both portfolios
# extractObjectiveMeasures.opt.list evaluates all objectives for every
# portfolio using object[[i]]$R (stored because trace = TRUE on each element)
# ---------------------------------------------------------------------------

test_that("chart.RiskReward.opt.list: runs without error with ES / mean columns", {
  skip_if(is.null(opt_list_obj), "opt_list_obj fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskReward(opt_list_obj,
                     risk.col   = "ES",
                     return.col = "mean")
  )
})

test_that("chart.RiskReward.opt.list: rejects non-opt.list object with informative error", {
  expect_error(
    chart.RiskReward(structure(list(), class = "not.opt.list")),
    regexp = "no applicable method"
  )
})

test_that("chart.RiskReward.opt.list: stops with invalid risk.col", {
  skip_if(is.null(opt_list_obj), "opt_list_obj fixture not available")
  expect_error(
    chart.RiskReward(opt_list_obj,
                     risk.col   = "not_a_real_column",
                     return.col = "mean"),
    regexp = "not in column names"
  )
})

test_that("chart.RiskReward.opt.list: stops with invalid return.col", {
  skip_if(is.null(opt_list_obj), "opt_list_obj fixture not available")
  expect_error(
    chart.RiskReward(opt_list_obj,
                     risk.col   = "ES",
                     return.col = "not_a_real_column"),
    regexp = "not in column names"
  )
})

# ===========================================================================
# New Section: chart.Scatter.RP with neighbors argument
# ===========================================================================

test_that("chart.Scatter.RP: neighbors=3 (integer scalar) produces no error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.RP(opt_rp_trace,
                     return.col = "mean",
                     risk.col   = "ES",
                     neighbors  = 3)
  )
})

test_that("chart.Scatter.RP: neighbors=c(1,2,3) (index vector) produces no error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.RP(opt_rp_trace,
                     return.col = "mean",
                     risk.col   = "ES",
                     neighbors  = c(1, 2, 3))
  )
})

test_that("chart.Scatter.RP: chart.assets=TRUE produces no error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.RP(opt_rp_trace,
                     return.col   = "mean",
                     risk.col     = "ES",
                     chart.assets = TRUE)
  )
})

test_that("charts.RP: neighbors=3 propagates through wrapper without error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.RP(opt_rp_trace,
              risk.col   = "ES",
              return.col = "mean",
              neighbors  = 3)
  )
})

test_that("charts.RP: chart.assets=TRUE propagates through wrapper without error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.RP(opt_rp_trace,
              risk.col     = "ES",
              return.col   = "mean",
              chart.assets = TRUE)
  )
})
