###############################################################################
# tests/testthat/test-charts-gensa-extra.R
#
# Additional branch coverage for R/charts.GenSA.R — specifically the
# branches NOT yet covered by test-charts-other.R:
#
#   chart.Scatter.GenSA: rp=TRUE branch (generates random portfolios)
#   chart.Scatter.GenSA: chart.assets=TRUE branch
#   chart.Weight.GenSA:  las <= 1 branch (bottommargin = minmargin)
#   chart.Weight.GenSA:  Infinite box-constraint branch (ylim from weights)
#   chart.Scatter.GenSA: no-trace-R stop() branch
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("PortfolioAnalytics")
skip_if_not_installed("GenSA")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

edhec4 <- edhec[, 1:4]

# GenSA with trace=TRUE (mean + StdDev objectives)
opt_gensa_trace <- tryCatch({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box",        min = 0.05,     max = 0.60)
  p <- add.objective(p,  type = "return",     name = "mean")
  p <- add.objective(p,  type = "risk",       name = "StdDev")
  set.seed(42)
  optimize.portfolio(edhec4, p,
                     optimize_method = "GenSA",
                     trace           = TRUE,
                     maxit           = 30)
}, error = function(e) NULL)

# GenSA WITHOUT trace (no object$R) — for the stop() branch
opt_gensa_notrace <- tryCatch({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box",        min = 0.05,     max = 0.60)
  p <- add.objective(p,  type = "return",     name = "mean")
  p <- add.objective(p,  type = "risk",       name = "StdDev")
  set.seed(99)
  optimize.portfolio(edhec4, p,
                     optimize_method = "GenSA",
                     trace           = FALSE,
                     maxit           = 30)
}, error = function(e) NULL)

# GenSA with infinite box constraints (to hit the Inf ylim branch in chart.Weight.GenSA)
opt_gensa_inf <- tryCatch({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  # No box constraint means defaults use -Inf/Inf
  p <- add.constraint(p, type = "box", min = -Inf, max = Inf)
  p <- add.objective(p,  type = "return",     name = "mean")
  p <- add.objective(p,  type = "risk",       name = "StdDev")
  set.seed(7)
  optimize.portfolio(edhec4, p,
                     optimize_method = "GenSA",
                     trace           = TRUE,
                     maxit           = 20)
}, error = function(e) NULL)

# ===========================================================================
# Tests: chart.Scatter.GenSA — rp=TRUE branch
# ===========================================================================

test_that("chart.Scatter.GenSA: rp=TRUE generates random portfolios without error", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.GenSA(opt_gensa_trace,
                        return.col = "mean",
                        risk.col   = "StdDev",
                        rp         = TRUE)
  )
})

# ===========================================================================
# Tests: chart.Scatter.GenSA — chart.assets=TRUE branch
# ===========================================================================

test_that("chart.Scatter.GenSA: chart.assets=TRUE produces no error", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.GenSA(opt_gensa_trace,
                        return.col   = "mean",
                        risk.col     = "StdDev",
                        chart.assets = TRUE)
  )
})

test_that("chart.Scatter.GenSA: rp=TRUE and chart.assets=TRUE together produce no error", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Scatter.GenSA(opt_gensa_trace,
                        return.col   = "mean",
                        risk.col     = "StdDev",
                        rp           = TRUE,
                        chart.assets = TRUE)
  )
})

# ===========================================================================
# Tests: chart.Scatter.GenSA — stop() when object$R is NULL (no trace)
# ===========================================================================

test_that("chart.Scatter.GenSA: stops when object$R is NULL (no trace)", {
  skip_if(is.null(opt_gensa_notrace), "opt_gensa_notrace fixture not available")
  # object$R is NULL when trace=FALSE
  expect_error(
    chart.Scatter.GenSA(opt_gensa_notrace,
                        return.col = "mean",
                        risk.col   = "StdDev"),
    regexp = "Returns object not detected"
  )
})

# ===========================================================================
# Tests: chart.Weight.GenSA — las <= 1 branch (bottommargin = minmargin)
# ===========================================================================

test_that("chart.Weight.GenSA: las=1 uses minmargin (no rotation)", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.GenSA(opt_gensa_trace, las = 1)
  )
})

test_that("chart.Weight.GenSA: las=0 uses minmargin (no rotation)", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.GenSA(opt_gensa_trace, las = 0)
  )
})

# ===========================================================================
# Tests: chart.Weight.GenSA — xlab non-NULL (minmargin=5 branch)
# ===========================================================================

test_that("chart.Weight.GenSA: xlab non-NULL (minmargin=5 path)", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.GenSA(opt_gensa_trace, xlab = "Assets")
  )
})

# ===========================================================================
# Tests: chart.Weight.GenSA — Infinite box constraints (ylim from weights)
# ===========================================================================

test_that("chart.Weight.GenSA: infinite box constraints use weight-based ylim", {
  skip_if(is.null(opt_gensa_inf), "opt_gensa_inf fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.GenSA(opt_gensa_inf, plot.type = "line")
  )
})

# ===========================================================================
# Tests: chart.Weight.GenSA — empty/blank main (topmargin=1 branch)
# ===========================================================================

test_that("chart.Weight.GenSA: main='' uses topmargin=1", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.Weight.GenSA(opt_gensa_trace, main = "")
  )
})

# ===========================================================================
# Tests: charts.GenSA with rp=TRUE and chart.assets=TRUE
# ===========================================================================

test_that("charts.GenSA: rp=TRUE propagates to chart.Scatter.GenSA without error", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.GenSA(opt_gensa_trace,
                 rp         = TRUE,
                 return.col = "mean",
                 risk.col   = "StdDev")
  )
})

test_that("charts.GenSA: chart.assets=TRUE propagates without error", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    charts.GenSA(opt_gensa_trace,
                 chart.assets = TRUE,
                 return.col   = "mean",
                 risk.col     = "StdDev")
  )
})

# ===========================================================================
# Tests: plot.optimize.portfolio.GenSA with non-default args
# ===========================================================================

test_that("plot.optimize.portfolio.GenSA: rp=TRUE via S3 plot() without error", {
  skip_if(is.null(opt_gensa_trace), "opt_gensa_trace fixture not available")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plot(opt_gensa_trace,
         rp         = TRUE,
         return.col = "mean",
         risk.col   = "StdDev")
  )
})
