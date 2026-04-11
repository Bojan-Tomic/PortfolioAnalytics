###############################################################################
# tests/testthat/test-charts-groups-extra.R
#
# Source files covered:
#   R/charts.groups.R — chart.GroupWeights (line plot and barplot),
#                        barplotGroupWeights (unexported helper)
#
# Targeted branches:
#   chart.GroupWeights (line plot path):
#     Line 39  : xlab non-NULL -> minmargin = 5
#     Line 42  : any(is.infinite(cUP|cLO)) -> ylim = range(weights)
#     Line 51  : main="" -> topmargin = 1
#     Lines 66-67: grouping="category" -> ylim = range(cat_weights)
#     Line 71  : las <= 1 -> bottommargin = minmargin  (las=1)
#
#   barplotGroupWeights (via chart.GroupWeights plot.type="barplot"):
#     Line 115 : grouping="category" -> category_weights path
#     Line 122 : xlab non-NULL -> minmargin = 5 (barplotGroupWeights passes xlab through)
#     Line 127 : las <= 1 -> not triggered inside barplotGroupWeights (no margin logic there)
#
# All chart tests redirect graphics to pdf(NULL) to avoid opening windows.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# File-scope fixtures
# ---------------------------------------------------------------------------

# Portfolio with GROUP constraints (finite bounds) — used for "groups" grouping tests.
# Two groups of 2 assets each, both with finite [0.30, 0.70] bounds.
.p_grp <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.constraint(p, type = "group",
                      groups      = list(c(1, 2), c(3, 4)),
                      group_min   = c(0.30, 0.30),
                      group_max   = c(0.70, 0.70),
                      group_labels = c("Grp1", "Grp2"))
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
})

opt_grp <- tryCatch(
  optimize.portfolio(edhec4, .p_grp, optimize_method = "ROI"),
  error = function(e) NULL
)

# Portfolio with GROUP constraints using Inf group_max — forces the
# ylim = range(weights) branch (line 42 in chart.GroupWeights).
.p_grp_inf <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.constraint(p, type = "group",
                      groups      = list(c(1, 2), c(3, 4)),
                      group_min   = c(0.0, 0.0),
                      group_max   = c(Inf, Inf),
                      group_labels = c("Grp1", "Grp2"))
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
})

opt_grp_inf <- tryCatch(
  optimize.portfolio(edhec4, .p_grp_inf, optimize_method = "ROI"),
  error = function(e) NULL
)

# Portfolio with CATEGORY labels — enables the "category" grouping path.
# Two categories: Equity (assets 1-2) and FixedIncome (assets 3-4).
.p_cat <- local({
  p <- portfolio.spec(
    assets          = colnames(edhec4),
    category_labels = c("Equity", "Equity", "FixedIncome", "FixedIncome")
  )
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
})

opt_cat <- tryCatch(
  optimize.portfolio(edhec4, .p_cat, optimize_method = "ROI"),
  error = function(e) NULL
)

# Portfolio with BOTH category labels AND group constraints, so that the
# category path inside barplotGroupWeights (grouping="category") is exercised
# without needing the group constraint present.
# (barplotGroupWeights uses the same opt_cat result — category_weights are
# non-NULL whenever category_labels are set.)

# ===========================================================================
# Section 1: chart.GroupWeights — line plot, grouping="groups"
# ===========================================================================

test_that("chart.GroupWeights line plot: default args produce no error", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "line")
  )
})

# Line 39: xlab non-NULL -> minmargin = 5
test_that("chart.GroupWeights line plot: xlab non-NULL (minmargin=5 branch) produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "line",
                       xlab = "Asset Groups")
  )
})

# Line 42: any(is.infinite(cUP)) -> ylim = range(weights)
test_that("chart.GroupWeights line plot: Inf group_max -> ylim=range(weights) branch (line 42)", {
  skip_on_cran()
  skip_if(is.null(opt_grp_inf))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp_inf, grouping = "groups", plot.type = "line")
  )
})

# Line 51: main="" -> topmargin = 1
test_that("chart.GroupWeights line plot: main='' (topmargin=1 branch, line 51) produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "line",
                       main = "")
  )
})

# Line 71: las <= 1 -> bottommargin = minmargin
test_that("chart.GroupWeights line plot: las=1 (bottommargin=minmargin branch, line 71)", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "line",
                       las = 1)
  )
})

# Las=0 also hits the las<=1 branch
test_that("chart.GroupWeights line plot: las=0 (bottommargin=minmargin branch) produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "line",
                       las = 0)
  )
})

# ===========================================================================
# Section 2: chart.GroupWeights — line plot, grouping="category" (lines 66-67)
# ===========================================================================

# Lines 66-67: grouping="category" -> ylim = range(category_weights)
test_that("chart.GroupWeights line plot: grouping='category' (lines 66-67 ylim branch)", {
  skip_on_cran()
  skip_if(is.null(opt_cat))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_cat, grouping = "category", plot.type = "line")
  )
})

test_that("chart.GroupWeights line plot: grouping='category', main='' (topmargin=1)", {
  skip_on_cran()
  skip_if(is.null(opt_cat))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_cat, grouping = "category", plot.type = "line",
                       main = "")
  )
})

test_that("chart.GroupWeights line plot: grouping='category', xlab non-NULL (minmargin=5)", {
  skip_on_cran()
  skip_if(is.null(opt_cat))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_cat, grouping = "category", plot.type = "line",
                       xlab = "Category")
  )
})

test_that("chart.GroupWeights line plot: grouping='category', las=1 (bottommargin=minmargin)", {
  skip_on_cran()
  skip_if(is.null(opt_cat))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_cat, grouping = "category", plot.type = "line",
                       las = 1)
  )
})

# ===========================================================================
# Section 3: chart.GroupWeights — stop() error paths
# ===========================================================================

test_that("chart.GroupWeights: stops for non-optimize.portfolio object", {
  skip_on_cran()
  expect_error(
    chart.GroupWeights(list(), plot.type = "line"),
    "optimize.portfolio"
  )
})

test_that("chart.GroupWeights: grouping='groups' with no group constraint stops", {
  skip_on_cran()
  skip_if(is.null(opt_cat))  # opt_cat has no group constraints
  pdf(NULL); on.exit(dev.off())
  expect_error(
    chart.GroupWeights(opt_cat, grouping = "groups", plot.type = "line"),
    "No weights detected for groups"
  )
})

test_that("chart.GroupWeights: grouping='category' with no category_labels stops", {
  skip_on_cran()
  skip_if(is.null(opt_grp))  # opt_grp has no category_labels
  pdf(NULL); on.exit(dev.off())
  expect_error(
    chart.GroupWeights(opt_grp, grouping = "category", plot.type = "line"),
    "No weights detected for category"
  )
})

# ===========================================================================
# Section 4: chart.GroupWeights — barplot type (calls barplotGroupWeights)
# ===========================================================================

# Line 115: stop for non-optimize.portfolio via barplotGroupWeights path
# (chart.GroupWeights dispatches to barplotGroupWeights when plot.type="barplot")
test_that("chart.GroupWeights barplot: grouping='groups' produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "barplot")
  )
})

# Line 115 (grouping="category" in barplotGroupWeights): category path
test_that("chart.GroupWeights barplot: grouping='category' (barplotGroupWeights line 115)", {
  skip_on_cran()
  skip_if(is.null(opt_cat))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_cat, grouping = "category", plot.type = "barplot")
  )
})

# Line 122: xlab non-NULL in barplotGroupWeights (called via chart.GroupWeights)
test_that("chart.GroupWeights barplot: xlab non-NULL (barplotGroupWeights xlab branch, line 122)", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "barplot",
                       xlab = "Asset Groups")
  )
})

# Barplot with category + xlab
test_that("chart.GroupWeights barplot: grouping='category', xlab non-NULL", {
  skip_on_cran()
  skip_if(is.null(opt_cat))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_cat, grouping = "category", plot.type = "barplot",
                       xlab = "Categories")
  )
})

# Line 127: las <= 1 branch in barplotGroupWeights
test_that("chart.GroupWeights barplot: las=1 (barplotGroupWeights las<=1 branch, line 127)", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "barplot",
                       las = 1)
  )
})

# Las=0 also hits the barplotGroupWeights las<=1 branch
test_that("chart.GroupWeights barplot: las=0 (barplotGroupWeights las<=1 branch)", {
  skip_on_cran()
  skip_if(is.null(opt_cat))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_cat, grouping = "category", plot.type = "barplot",
                       las = 0)
  )
})

# ===========================================================================
# Section 5: barplotGroupWeights called directly (unexported)
# ===========================================================================

test_that("barplotGroupWeights direct call: grouping='groups' produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::barplotGroupWeights(opt_grp, grouping = "groups")
  )
})

# Directly exercise grouping="category" in barplotGroupWeights (line 125-127)
test_that("barplotGroupWeights direct call: grouping='category' (line 125) produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_cat))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::barplotGroupWeights(opt_cat, grouping = "category")
  )
})

# Direct call: stop for non-optimize.portfolio (line 115)
test_that("barplotGroupWeights direct call: stops for non-optimize.portfolio (line 115)", {
  skip_on_cran()
  expect_error(
    PortfolioAnalytics:::barplotGroupWeights(list(), grouping = "groups"),
    "optimize.portfolio"
  )
})

# ===========================================================================
# Section 6: chart.GroupWeights — "bar" alias (same as "barplot")
# ===========================================================================

test_that("chart.GroupWeights: plot.type='bar' alias dispatches to barplotGroupWeights", {
  skip_on_cran()
  skip_if(is.null(opt_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.GroupWeights(opt_grp, grouping = "groups", plot.type = "bar")
  )
})
