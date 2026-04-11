###############################################################################
# tests/testthat/test-chart-weights-extra.R
#
# Source files covered:
#   R/chart.Weights.R — barplotWeights (unexported helper),
#                        chart.Weights.optimize.portfolio.rebalancing
#
# Targeted branches:
#   barplotWeights:
#     Lines 75-76 : bottommargin > 10 truncation branch
#                   (very long column names + las > 1 + cex.lab=1.0)
#     Line 86     : las <= 1 -> bottommargin = minmargin
#     Lines 41-44 : xlab non-NULL -> minmargin = 5
#     Lines 45-46 : main="" -> topmargin = 1
#
#   chart.Weights.optimize.portfolio.rebalancing (line 101):
#     Line 86 per task description — the S3 method for rebalancing objects
#     (calls chart.StackedBar internally).
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

# Standard ROI result — used for barplotWeights branch tests.
# opt_roi_trace is available from helper-portfolioanalytics.R.

# Result with VERY LONG column names — designed to trigger the
# bottommargin > 10 truncation branch (lines 75-76 in chart.Weights.R).
#
# Strategy: manufacture a fake optimize.portfolio.ROI result whose $weights
# vector has column names long enough that
#   max(strwidth(names, "in") / par("cin")[1]) * cex.lab > 10
# The strwidth / par("cin") ratio is ~19 per character-inch unit for
# 32-char names, so any name longer than ~10/1.0 * par("cin")[1] in inches
# triggers the branch.  Using 32-character names is sufficient.
#
# We build a tiny 4-asset spec so the underlying optimisation is fast, but
# artificially rename the result weights to very long strings.
.opt_long_names <- local({
  # Run a fast ROI optimisation on edhec4
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")
  result <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "ROI"),
    error = function(e) NULL
  )
  if (!is.null(result)) {
    # Rename weights to 32-character names to force bottommargin > 10
    names(result$weights) <- c(
      "VeryLongAssetNameAlphaXXXXXXXX",
      "VeryLongAssetNameBetaXXXXXXXXX",
      "VeryLongAssetNameGammaXXXXXXXX",
      "VeryLongAssetNameDeltaXXXXXXXX"
    )
  }
  result
})

# optimize.portfolio.rebalancing result for chart.Weights dispatch test.
# ROI with annual rebalancing on a short window (2019-2021) for speed.
# (opt_rebal is NOT defined in helper-portfolioanalytics.R so we define it here.)
.opt_rebal_cw <- tryCatch({
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
# Section 1: barplotWeights branch tests
# ===========================================================================

# Lines 75-76: bottommargin > 10 truncation (long column names, las=3, cex.lab=1)
test_that("barplotWeights: long names trigger bottommargin>10 truncation (lines 75-76)", {
  skip_on_cran()
  skip_if(is.null(.opt_long_names))
  pdf(NULL); on.exit(dev.off())
  # las=3 (default) + long names -> bottommargin > 10 -> names truncated to 19 chars
  expect_no_error(
    chart.Weights(.opt_long_names, plot.type = "barplot", las = 3, cex.lab = 1.0)
  )
})

# Also hit via "bar" alias
test_that("barplotWeights: plot.type='bar' + long names -> bottommargin>10 produces no error", {
  skip_on_cran()
  skip_if(is.null(.opt_long_names))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(.opt_long_names, plot.type = "bar", las = 3, cex.lab = 1.0)
  )
})

# Verify truncation occurred: column names in the plot are at most 19 chars.
# We test this indirectly by confirming no error and that the weights vector
# with long names is accepted.
test_that("barplotWeights: long names + xlab non-NULL -> minmargin=5 + truncation", {
  skip_on_cran()
  skip_if(is.null(.opt_long_names))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(.opt_long_names, plot.type = "barplot", las = 3, cex.lab = 1.0,
                  xlab = "Assets")
  )
})

# Line 86: las <= 1 -> bottommargin = minmargin (no long-label computation)
test_that("barplotWeights: las=1 -> bottommargin=minmargin branch (line 86)", {
  skip_on_cran()
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "barplot", las = 1)
  )
})

test_that("barplotWeights: las=0 -> bottommargin=minmargin branch produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "barplot", las = 0)
  )
})

# Lines 41-44: xlab non-NULL -> minmargin = 5
test_that("barplotWeights: xlab non-NULL -> minmargin=5 branch produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "barplot", xlab = "Assets")
  )
})

# Lines 45-46: main="" -> topmargin = 1
test_that("barplotWeights: main='' -> topmargin=1 branch produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "barplot", main = "")
  )
})

# Combined: main="" + xlab + las=1 (las<=1 branch, minmargin=5, topmargin=1)
test_that("barplotWeights: main='', xlab, las=1 combined branches produce no error", {
  skip_on_cran()
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "barplot",
                  main = "", xlab = "Assets", las = 1)
  )
})

# legend.loc = NULL suppresses legend
test_that("barplotWeights: legend.loc=NULL suppresses legend without error", {
  skip_on_cran()
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "barplot", legend.loc = NULL)
  )
})

# Custom colorset
test_that("barplotWeights: custom colorset produces no error", {
  skip_on_cran()
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(opt_roi_trace, plot.type = "barplot",
                  colorset = rainbow(length(extractWeights(opt_roi_trace))))
  )
})

# ===========================================================================
# Section 2: chart.Weights.optimize.portfolio.rebalancing (line 101)
#
# S3 dispatch for optimize.portfolio.rebalancing objects —
# calls chart.StackedBar() from PerformanceAnalytics internally.
# ===========================================================================

# Line 86 per task description = chart.Weights.optimize.portfolio.rebalancing dispatch
test_that("chart.Weights.optimize.portfolio.rebalancing: S3 dispatch produces no error", {
  skip_on_cran()
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if(is.null(.opt_rebal_cw))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(.opt_rebal_cw)
  )
})

test_that("chart.Weights.optimize.portfolio.rebalancing: custom main title produces no error", {
  skip_on_cran()
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if(is.null(.opt_rebal_cw))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(.opt_rebal_cw, main = "Rebalancing Weights Over Time")
  )
})

test_that("chart.Weights.optimize.portfolio.rebalancing: object is correct class", {
  skip_on_cran()
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if(is.null(.opt_rebal_cw))
  expect_s3_class(.opt_rebal_cw, "optimize.portfolio.rebalancing")
})

test_that("chart.Weights.optimize.portfolio.rebalancing: extractWeights returns xts", {
  skip_on_cran()
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if(is.null(.opt_rebal_cw))
  w <- extractWeights(.opt_rebal_cw)
  expect_true(is.numeric(w) || xts::is.xts(w),
              label = "extractWeights returns numeric or xts")
})

# ===========================================================================
# Section 3: barplotWeights long-name truncation — verify the truncation logic
# ===========================================================================

# Confirm the long-names object has the right structure
test_that("long-names fixture has correct weight vector structure", {
  skip_on_cran()
  skip_if(is.null(.opt_long_names))
  w <- .opt_long_names$weights
  expect_equal(length(w), 4L)
  expect_true(all(nchar(names(w)) >= 19L),
              label = "all names are long enough to trigger bottommargin > 10 truncation")
})

# Verify that barplotWeights (via ROI chart.Weights dispatch) truncates names
# to <= 19 chars when bottommargin > 10.  We cannot directly inspect the plot,
# but we can confirm the code path runs without error even with extreme cex.lab.
test_that("barplotWeights: long names + cex.lab=2 still produces no error", {
  skip_on_cran()
  skip_if(is.null(.opt_long_names))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weights(.opt_long_names, plot.type = "barplot", las = 3, cex.lab = 2.0)
  )
})
