###############################################################################
# tests/testthat/test-charts-rp-extra.R
#
# Additional branch coverage for R/PortfolioAnalytics:::charts.RP.R — branches NOT yet covered
# by test-charts-rp.R:
#
#   PortfolioAnalytics:::chart.Weight.RP:
#     - neighbors as matrix/data.frame (lines 75-81)
#     - las <= 1 branch (bottommargin = minmargin)
#     - xlab non-NULL (minmargin = 5)
#     - main = "" (topmargin = 1)
#     - Infinite box constraints (ylim from weights)
#
#   PortfolioAnalytics:::chart.Scatter.RP:
#     - neighbors as matrix/data.frame (lines 201-212)
#     - non-matching return.col/risk.col (applyFUN branch, lines 128-156)
#     - optimal-weights applyFUN branch (lines 243-250) when return/risk
#       cols don't match objective_measures
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("PortfolioAnalytics")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

edhec4 <- edhec[, 1:4]

# Random portfolio optimisation with trace=TRUE (mean + ES objectives)
opt_rp_trace <- tryCatch({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box",        min = 0.05,     max = 0.60)
  p <- add.objective(p,  type = "return",     name = "mean")
  p <- add.objective(p,  type = "risk",       name = "ES")
  set.seed(42)
  optimize.portfolio(edhec4, p,
                     optimize_method = "random",
                     trace           = TRUE,
                     search_size     = 500L)
}, error = function(e) NULL)

# Random portfolio with INFINITE box constraints (to trigger the Inf ylim branch)
opt_rp_inf <- tryCatch({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box",        min = -Inf,     max = Inf)
  p <- add.objective(p,  type = "return",     name = "mean")
  p <- add.objective(p,  type = "risk",       name = "ES")
  set.seed(99)
  optimize.portfolio(edhec4, p,
                     optimize_method = "random",
                     trace           = TRUE,
                     search_size     = 300L)
}, error = function(e) NULL)

# ===========================================================================
# PortfolioAnalytics:::chart.Weight.RP — parameter branch variants
# ===========================================================================

test_that("PortfolioAnalytics:::chart.Weight.RP: las=1 uses minmargin path (no rotation)", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::chart.Weight.RP(opt_rp_trace, las = 1))
})

test_that("PortfolioAnalytics:::chart.Weight.RP: xlab non-NULL sets minmargin=5", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::chart.Weight.RP(opt_rp_trace, xlab = "Asset"))
})

test_that("PortfolioAnalytics:::chart.Weight.RP: main='' uses topmargin=1", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::chart.Weight.RP(opt_rp_trace, main = ""))
})

test_that("PortfolioAnalytics:::chart.Weight.RP: infinite box constraints use weight-based ylim", {
  skip_if(is.null(opt_rp_inf), "opt_rp_inf fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::chart.Weight.RP(opt_rp_inf, plot.type = "line"))
})

# ===========================================================================
# PortfolioAnalytics:::chart.Weight.RP — neighbors as matrix (lines 75-81)
# ===========================================================================

test_that("PortfolioAnalytics:::chart.Weight.RP: neighbors as matrix overplots neighbor weights", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  # Build a neighbors matrix from extractStats — same format the code expects
  xtract <- extractStats(opt_rp_trace)
  wts_idx <- grep("^w[.]", colnames(xtract))
  # Take 3 rows of extractStats output as a matrix-neighbors input
  nbmat <- xtract[1:3, ]
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.RP(opt_rp_trace,
                    neighbors = nbmat)
  )
})

test_that("PortfolioAnalytics:::chart.Weight.RP: neighbors as data.frame overplots neighbor weights", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  xtract <- extractStats(opt_rp_trace)
  nbdf <- as.data.frame(xtract[1:3, ])
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.RP(opt_rp_trace,
                    neighbors = nbdf)
  )
})

# ===========================================================================
# PortfolioAnalytics:::chart.Scatter.RP — neighbors as matrix/data.frame (lines 201-212)
# ===========================================================================

test_that("PortfolioAnalytics:::chart.Scatter.RP: neighbors as matrix overplots neighbor points", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  xtract <- extractStats(opt_rp_trace)
  nbmat <- xtract[1:3, ]
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Scatter.RP(opt_rp_trace,
                     return.col = "mean",
                     risk.col   = "ES",
                     neighbors  = nbmat)
  )
})

test_that("PortfolioAnalytics:::chart.Scatter.RP: neighbors as data.frame overplots neighbor points", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  xtract <- extractStats(opt_rp_trace)
  nbdf <- as.data.frame(xtract[1:3, ])
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Scatter.RP(opt_rp_trace,
                     return.col = "mean",
                     risk.col   = "ES",
                     neighbors  = nbdf)
  )
})

# ===========================================================================
# PortfolioAnalytics:::chart.Scatter.RP — non-matching return.col/risk.col (applyFUN branch)
# lines 128-156: triggers when return.col or risk.col is not in extractStats
# ===========================================================================

test_that("PortfolioAnalytics:::chart.Scatter.RP: non-matching risk.col falls back to applyFUN (StdDev)", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  # "StdDev" is not in extractStats (which has mean/ES), so it goes to applyFUN
  expect_no_error(
    suppressWarnings(
      PortfolioAnalytics:::chart.Scatter.RP(opt_rp_trace,
                       return.col = "mean",
                       risk.col   = "StdDev")
    )
  )
})

test_that("PortfolioAnalytics:::chart.Scatter.RP: non-matching return.col falls back to applyFUN (sd)", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  # Suppress the "do not match extractStats" warning
  expect_no_error(
    suppressWarnings(
      PortfolioAnalytics:::chart.Scatter.RP(opt_rp_trace,
                       return.col = "sd",
                       risk.col   = "ES")
    )
  )
})

test_that("PortfolioAnalytics:::chart.Scatter.RP: both return.col and risk.col non-matching (both applyFUN)", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    suppressWarnings(
      PortfolioAnalytics:::chart.Scatter.RP(opt_rp_trace,
                       return.col = "sd",
                       risk.col   = "StdDev")
    )
  )
})

# ===========================================================================
# PortfolioAnalytics:::chart.Scatter.RP — optimal-weights applyFUN branch (lines 243-250)
# When return.col or risk.col don't match objective_measures slot
# ===========================================================================

test_that("PortfolioAnalytics:::chart.Scatter.RP: optimal applyFUN branch fires for non-objective risk.col", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  # "StdDev" is not in objective_measures (which has mean and ES), so
  # lines 243-250 (applyFUN on optimal weights) should be executed
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    suppressWarnings(
      PortfolioAnalytics:::chart.Scatter.RP(opt_rp_trace,
                       return.col = "mean",
                       risk.col   = "StdDev")
    )
  )
})

# ===========================================================================
# PortfolioAnalytics:::charts.RP — propagation of non-matching cols
# ===========================================================================

test_that("PortfolioAnalytics:::charts.RP: non-matching risk.col falls back via applyFUN without error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    suppressWarnings(
      PortfolioAnalytics:::charts.RP(opt_rp_trace,
                risk.col   = "StdDev",
                return.col = "mean")
    )
  )
})

# ===========================================================================
# plot.optimize.portfolio — propagation
# ===========================================================================

test_that("plot.optimize.portfolio: non-matching risk.col falls back without error", {
  skip_if(is.null(opt_rp_trace), "opt_rp_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    suppressWarnings(
      plot(opt_rp_trace,
           return.col = "mean",
           risk.col   = "StdDev")
    )
  )
})
