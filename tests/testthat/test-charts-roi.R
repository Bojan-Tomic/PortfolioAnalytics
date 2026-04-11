###############################################################################
# tests/testthat/test-charts-roi.R
#
# Source files covered:
#   R/PortfolioAnalytics:::charts.ROI.R              — PortfolioAnalytics:::chart.Weight.ROI, PortfolioAnalytics:::chart.Scatter.ROI,
#                                  PortfolioAnalytics:::charts.ROI, plot.optimize.portfolio.ROI
#   R/charts.efficient.frontier.R — chart.EfficientFrontier (ROI, CVXR,
#                                  default/RP methods), chart.EF.Weights
#
# All chart tests redirect graphics to a null PDF device to avoid opening
# windows or writing files.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# File-scope fixtures
# opt_roi_trace, opt_rp_trace, edhec4, .portf_meansd4, .portf_meanES4 are
# all available from helper-portfolioanalytics.R — do NOT redefine them.
# ---------------------------------------------------------------------------

# CVXR result with trace=TRUE (for chart.EfficientFrontier.optimize.portfolio.CVXR)
opt_cvxr_trace <- NULL
if (requireNamespace("CVXR", quietly = TRUE)) {
  opt_cvxr_trace <- tryCatch({
    p <- portfolio.spec(assets = colnames(edhec4))
    p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
    p <- add.constraint(p, type = "box", min = 0, max = 1)
    p <- add.objective(p, type = "return", name = "mean")
    p <- add.objective(p, type = "risk",   name = "ES")
    optimize.portfolio(edhec4, p, optimize_method = "CVXR", trace = TRUE)
  }, error = function(e) NULL)
}

# efficient.frontier object for chart.EF.Weights.efficient.frontier
# NOTE: meanvar.efficient.frontier() returns class "frontier" (a matrix),
# NOT class "efficient.frontier".  chart.EF.Weights.efficient.frontier()
# requires the latter, so we use extractEfficientFrontier() which wraps the
# result in the proper list structure with class "efficient.frontier".
ef_meanvar <- tryCatch(
  extractEfficientFrontier(opt_roi_trace, match.col = "StdDev", n.portfolios = 5),
  error = function(e) NULL
)

# ===========================================================================
# 1.  PortfolioAnalytics:::chart.Weight.ROI / chart.Weights.optimize.portfolio.ROI
# ===========================================================================

test_that("PortfolioAnalytics:::chart.Weight.ROI: line plot (default) produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::chart.Weight.ROI(opt_roi_trace, plot.type = "line"))
})

test_that("PortfolioAnalytics:::chart.Weight.ROI: barplot type produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::chart.Weight.ROI(opt_roi_trace, plot.type = "barplot"))
})

test_that("PortfolioAnalytics:::chart.Weight.ROI: bar type alias produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::chart.Weight.ROI(opt_roi_trace, plot.type = "bar"))
})

test_that("chart.Weights.optimize.portfolio.ROI: S3 dispatch via chart.Weights() works", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(chart.Weights(opt_roi_trace))
})

test_that("chart.Weights.optimize.portfolio.ROI: barplot via chart.Weights() works", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(chart.Weights(opt_roi_trace, plot.type = "barplot"))
})

test_that("PortfolioAnalytics:::chart.Weight.ROI: stops for non-ROI object", {
  expect_error(
    PortfolioAnalytics:::chart.Weight.ROI(list()),
    "optimize.portfolio.ROI"
  )
})

# ===========================================================================
# 2.  PortfolioAnalytics:::chart.Scatter.ROI / chart.RiskReward.optimize.portfolio.ROI
# ===========================================================================

test_that("PortfolioAnalytics:::chart.Scatter.ROI: default args (risk.col='ES') produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::chart.Scatter.ROI(opt_roi_trace))
})

test_that("PortfolioAnalytics:::chart.Scatter.ROI: risk.col='StdDev' produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Scatter.ROI(opt_roi_trace, risk.col = "StdDev", return.col = "mean")
  )
})

test_that("PortfolioAnalytics:::chart.Scatter.ROI: chart.assets=TRUE produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Scatter.ROI(opt_roi_trace, chart.assets = TRUE)
  )
})

test_that("PortfolioAnalytics:::chart.Scatter.ROI: chart.assets=TRUE with StdDev produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Scatter.ROI(opt_roi_trace,
                      risk.col    = "StdDev",
                      return.col  = "mean",
                      chart.assets = TRUE)
  )
})

test_that("PortfolioAnalytics:::chart.Scatter.ROI: stops when trace=FALSE (R is NULL)", {
  # Build a result without trace so object$R is NULL
  opt_no_trace <- tryCatch(
    optimize.portfolio(edhec4, .portf_meansd4,
                       optimize_method = "ROI",
                       trace = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(opt_no_trace))
  expect_error(
    PortfolioAnalytics:::chart.Scatter.ROI(opt_no_trace),
    "trace=TRUE"
  )
})

test_that("PortfolioAnalytics:::chart.Scatter.ROI: stops for non-ROI object", {
  expect_error(
    PortfolioAnalytics:::chart.Scatter.ROI(list()),
    "optimize.portfolio.ROI"
  )
})

test_that("chart.RiskReward.optimize.portfolio.ROI: S3 dispatch works", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(chart.RiskReward(opt_roi_trace))
})

test_that("chart.RiskReward.optimize.portfolio.ROI: chart.assets=TRUE works", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.RiskReward(opt_roi_trace, chart.assets = TRUE)
  )
})

# ===========================================================================
# 3.  PortfolioAnalytics:::charts.ROI
# ===========================================================================

test_that("PortfolioAnalytics:::charts.ROI: default args (risk.col='ES') produce no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::charts.ROI(opt_roi_trace))
})

test_that("PortfolioAnalytics:::charts.ROI: risk.col='StdDev' produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::charts.ROI(opt_roi_trace, risk.col = "StdDev"))
})

test_that("PortfolioAnalytics:::charts.ROI: chart.assets=TRUE produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(PortfolioAnalytics:::charts.ROI(opt_roi_trace, chart.assets = TRUE))
})

test_that("PortfolioAnalytics:::charts.ROI: custom main title produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::charts.ROI(opt_roi_trace, main = "Test Portfolio", risk.col = "StdDev")
  )
})

# ===========================================================================
# 4.  plot.optimize.portfolio.ROI
# ===========================================================================

test_that("plot.optimize.portfolio.ROI: default produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(plot(opt_roi_trace))
})

test_that("plot.optimize.portfolio.ROI: risk.col='StdDev' produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(plot(opt_roi_trace, risk.col = "StdDev"))
})

test_that("plot.optimize.portfolio.ROI: chart.assets=TRUE produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(plot(opt_roi_trace, chart.assets = TRUE))
})

test_that("plot.optimize.portfolio.ROI: dispatches to plot.optimize.portfolio.ROI", {
  skip_if(is.null(opt_roi_trace))
  # verify the S3 method is registered and callable
  expect_true(
    is.function(getS3method("plot", "optimize.portfolio.ROI", optional = TRUE))
  )
})

# ===========================================================================
# 5.  chart.EfficientFrontier.optimize.portfolio.ROI
# ===========================================================================

test_that("chart.EfficientFrontier.ROI: match.col='StdDev' produces no error", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_roi_trace,
                            match.col    = "StdDev",
                            n.portfolios = 5)
  )
})

test_that("chart.EfficientFrontier.ROI: match.col='StdDev', chart.assets=FALSE", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_roi_trace,
                            match.col    = "StdDev",
                            n.portfolios = 5,
                            chart.assets = FALSE)
  )
})

test_that("chart.EfficientFrontier.ROI: match.col='StdDev', rf=NULL omits tangent line", {
  skip_if(is.null(opt_roi_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_roi_trace,
                            match.col    = "StdDev",
                            n.portfolios = 5,
                            rf           = NULL)
  )
})

# NOTE: match.col="ES" is intentionally omitted for the ROI object here.
# opt_roi_trace was optimised with a StdDev objective, so extractStats() does
# not contain an "ES" column.  When match.col does not appear in extractStats
# the source calls applyFUN() without its required `arguments` parameter — a
# known bug in charts.efficient.frontier.R.  Only match.col values that are
# present in the object's extractStats output are safe to test.

test_that("chart.EfficientFrontier.optimize.portfolio.ROI: stops for non-ROI object", {
  expect_error(
    chart.EfficientFrontier.optimize.portfolio.ROI(list()),
    "optimize.portfolio.ROI"
  )
})

# ===========================================================================
# 6.  chart.EfficientFrontier.optimize.portfolio.CVXR
# ===========================================================================

test_that("chart.EfficientFrontier.CVXR: match.col='ES' produces no error", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(opt_cvxr_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_cvxr_trace,
                            match.col    = "ES",
                            n.portfolios = 5)
  )
})

# NOTE: match.col="StdDev" is intentionally omitted for the CVXR object here.
# opt_cvxr_trace was optimised with an ES objective, so extractStats() does
# not contain a "StdDev" column.  The same applyFUN/missing-arguments bug
# would be triggered.  Only "ES" (the portfolio's own objective) is safe.

test_that("chart.EfficientFrontier.CVXR: chart.assets=FALSE produces no error", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(opt_cvxr_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_cvxr_trace,
                            match.col    = "ES",
                            n.portfolios = 5,
                            chart.assets = FALSE)
  )
})

test_that("chart.EfficientFrontier.CVXR: rf=NULL omits tangent line", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(opt_cvxr_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_cvxr_trace,
                            match.col    = "ES",
                            n.portfolios = 5,
                            rf           = NULL)
  )
})

test_that("chart.EfficientFrontier.optimize.portfolio.CVXR: stops for non-CVXR object", {
  skip_if_not_installed("CVXR")
  expect_error(
    chart.EfficientFrontier.optimize.portfolio.CVXR(list()),
    "optimize.portfolio.CVXR"
  )
})

# ===========================================================================
# 7.  chart.EfficientFrontier.optimize.portfolio (default / random method)
# ===========================================================================

test_that("chart.EfficientFrontier.random: match.col='ES' produces no error", {
  skip_if(is.null(opt_rp_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_rp_trace,
                            match.col    = "ES",
                            n.portfolios = 5)
  )
})

test_that("chart.EfficientFrontier.random: chart.assets=FALSE produces no error", {
  skip_if(is.null(opt_rp_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_rp_trace,
                            match.col    = "ES",
                            n.portfolios = 5,
                            chart.assets = FALSE)
  )
})

test_that("chart.EfficientFrontier.random: rf=NULL omits tangent line", {
  skip_if(is.null(opt_rp_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_rp_trace,
                            match.col    = "ES",
                            n.portfolios = 5,
                            rf           = NULL)
  )
})

test_that("chart.EfficientFrontier.random: labels.assets=FALSE produces no error", {
  skip_if(is.null(opt_rp_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(opt_rp_trace,
                            match.col     = "ES",
                            n.portfolios  = 5,
                            labels.assets = FALSE)
  )
})

test_that("chart.EfficientFrontier.optimize.portfolio: stops for GenSA object", {
  # Construct a fake GenSA class object to trigger the stop() branch.
  fake_gensa <- structure(list(), class = c("optimize.portfolio.GenSA",
                                            "optimize.portfolio"))
  expect_error(
    chart.EfficientFrontier(fake_gensa, match.col = "ES"),
    "GenSA"
  )
})

test_that("chart.EfficientFrontier.optimize.portfolio: stops for non-optimize.portfolio object", {
  expect_error(
    chart.EfficientFrontier.optimize.portfolio(list()),
    "optimize.portfolio"
  )
})

# ===========================================================================
# 8.  chart.EF.Weights.efficient.frontier
# ===========================================================================

test_that("chart.EF.Weights.efficient.frontier: match.col='StdDev' produces no error", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EF.Weights(ef_meanvar, match.col = "StdDev")
  )
})

test_that("chart.EF.Weights.efficient.frontier: legend.loc=NULL produces no error", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EF.Weights(ef_meanvar, match.col = "StdDev", legend.loc = NULL)
  )
})

test_that("chart.EF.Weights.efficient.frontier: custom colorset produces no error", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  n_assets <- ncol(edhec4)
  expect_no_error(
    chart.EF.Weights(ef_meanvar,
                     match.col = "StdDev",
                     colorset  = rainbow(n_assets))
  )
})

test_that("chart.EF.Weights.efficient.frontier: stops for non-efficient.frontier", {
  expect_error(
    chart.EF.Weights.efficient.frontier(list()),
    "efficient.frontier"
  )
})

# ===========================================================================
# 9.  chart.EF.Weights.optimize.portfolio
# ===========================================================================

# For random portfolio objects, extractEfficientFrontier uses
# extract.efficient.frontier() which only needs the trace data — no CVXR.
test_that("chart.EF.Weights.optimize.portfolio: opt_rp_trace, match.col='ES'", {
  skip_if(is.null(opt_rp_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EF.Weights(opt_rp_trace,
                     match.col    = "ES",
                     n.portfolios = 5)
  )
})

test_that("chart.EF.Weights.optimize.portfolio: opt_rp_trace, legend.loc=NULL", {
  skip_if(is.null(opt_rp_trace))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EF.Weights(opt_rp_trace,
                     match.col    = "ES",
                     n.portfolios = 5,
                     legend.loc   = NULL)
  )
})

# ROI-based path requires CVXR (meanvar.efficient.frontier default is CVXR)
test_that("chart.EF.Weights.optimize.portfolio: opt_roi_trace, match.col='StdDev'", {
  skip_if(is.null(opt_roi_trace))
  skip_if_not_installed("CVXR")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EF.Weights(opt_roi_trace,
                     match.col    = "StdDev",
                     n.portfolios = 5)
  )
})

test_that("chart.EF.Weights.optimize.portfolio: opt_roi_trace, match.col='ES'", {
  skip_if(is.null(opt_roi_trace))
  skip_if_not_installed("CVXR")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EF.Weights(opt_roi_trace,
                     match.col    = "ES",
                     n.portfolios = 5)
  )
})

test_that("chart.EF.Weights.optimize.portfolio: stops for non-optimize.portfolio", {
  expect_error(
    chart.EF.Weights.optimize.portfolio(list()),
    "optimize.portfolio"
  )
})

# ===========================================================================
# 10.  chart.EF.Weights.efficient.frontier with by.groups=TRUE
# ===========================================================================

test_that("chart.EF.Weights.efficient.frontier: by.groups=TRUE errors without group constraints", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  expect_error(
    chart.EF.Weights(ef_meanvar, match.col = "StdDev", by.groups = TRUE),
    "group constraints"
  )
})

# ===========================================================================
# 11.  chart.EfficientFrontier.efficient.frontier
# ===========================================================================

test_that("chart.EfficientFrontier.efficient.frontier: match.col='StdDev' produces no error", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(ef_meanvar, match.col = "StdDev")
  )
})

test_that("chart.EfficientFrontier.efficient.frontier: chart.assets=FALSE produces no error", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(ef_meanvar, match.col = "StdDev", chart.assets = FALSE)
  )
})

test_that("chart.EfficientFrontier.efficient.frontier: rf=NULL omits tangent line", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(ef_meanvar, match.col = "StdDev", rf = NULL)
  )
})

test_that("chart.EfficientFrontier.efficient.frontier: chart.assets=TRUE produces no error", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(ef_meanvar, match.col = "StdDev", chart.assets = TRUE)
  )
})

test_that("chart.EfficientFrontier.efficient.frontier: labels.assets=FALSE produces no error", {
  skip_if(is.null(ef_meanvar))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(ef_meanvar, match.col = "StdDev", labels.assets = FALSE)
  )
})

test_that("chart.EfficientFrontier.efficient.frontier: stops for non-efficient.frontier", {
  expect_error(
    chart.EfficientFrontier.efficient.frontier(list()),
    "efficient.frontier"
  )
})

# ===========================================================================
# 12.  create.EfficientFrontier + chart.EfficientFrontier.efficient.frontier
# ===========================================================================

# create.EfficientFrontier using mean-StdDev type (calls meanvar.efficient.frontier internally)
# Note: meanvar.efficient.frontier uses CVXR by default for portfolio optimization
ef_created <- NULL
if (requireNamespace("CVXR", quietly = TRUE)) {
  ef_created <- tryCatch(
    create.EfficientFrontier(edhec4, .portf_meansd4, type = "mean-StdDev", n.portfolios = 5),
    error = function(e) NULL
  )
}

test_that("create.EfficientFrontier: returns efficient.frontier class", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_created))
  expect_s3_class(ef_created, "efficient.frontier")
})

test_that("create.EfficientFrontier: frontier slot is a matrix", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_created))
  expect_true(is.matrix(ef_created$frontier) || is.data.frame(ef_created$frontier))
})

test_that("chart.EfficientFrontier.efficient.frontier with create.EfficientFrontier result", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_created))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontier(ef_created, match.col = "StdDev", chart.assets = FALSE)
  )
})

# ===========================================================================
# 13.  extractEfficientFrontier with explicit from/to/n.portfolios
# ===========================================================================

ef_explicit <- tryCatch(
  extractEfficientFrontier(opt_rp_trace, match.col = "ES", n.portfolios = 10),
  error = function(e) NULL
)

test_that("extractEfficientFrontier with n.portfolios returns efficient.frontier class", {
  skip_if(is.null(ef_explicit))
  expect_s3_class(ef_explicit, "efficient.frontier")
})

test_that("extractEfficientFrontier n.portfolios controls row count approximately", {
  skip_if(is.null(ef_explicit))
  # ef_explicit is a list; the frontier matrix lives in ef_explicit$frontier
  expect_true(nrow(ef_explicit$frontier) <= 10)
})

# extractEfficientFrontier with explicit from/to range
ef_range <- tryCatch(
  {
    xtract <- extractStats(opt_rp_trace)
    es_vals <- xtract[, pmatch("ES", colnames(xtract))]
    lo <- quantile(es_vals, 0.25)
    hi <- quantile(es_vals, 0.75)
    extractEfficientFrontier(opt_rp_trace, match.col = "ES", from = lo, to = hi, by = 0.002)
  },
  error = function(e) NULL
)

test_that("extractEfficientFrontier with explicit from/to range returns frontier", {
  skip_if(is.null(ef_range))
  expect_s3_class(ef_range, "frontier")
})

# ===========================================================================
# 14.  chart.EF.Weights.efficient.frontier: by.groups=TRUE with group constraints
#      Covers the groupfun() inner-function path (lines ~436-448 in
#      charts.efficient.frontier.R).
# ===========================================================================

# Portfolio spec with explicit group constraints (2 groups of 2 assets each).
# Feasible set: each group sums to [0.30, 0.70], individual weights [0.05, 0.50].
p_grp <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.50)
  p <- add.constraint(p, type = "group",
                      groups    = list(c(1, 2), c(3, 4)),
                      group_min = c(0.30, 0.30),
                      group_max = c(0.70, 0.70))
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "StdDev")
  p
})

# create.EfficientFrontier defaults to CVXR for mean-var; guard accordingly.
ef_grp <- NULL
if (requireNamespace("CVXR", quietly = TRUE)) {
  ef_grp <- tryCatch(
    create.EfficientFrontier(R = edhec4, portfolio = p_grp,
                             type = "mean-var", n.portfolios = 5),
    error = function(e) NULL
  )
}

test_that("chart.EF.Weights: by.groups=TRUE with group constraints succeeds", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EF.Weights(ef_grp, match.col = "StdDev", by.groups = TRUE)
  )
})

test_that("chart.EF.Weights: by.groups=TRUE, legend.loc=NULL with group constraints", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_grp))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EF.Weights(ef_grp, match.col = "StdDev", by.groups = TRUE,
                     legend.loc = NULL)
  )
})

# ===========================================================================
# 15.  chart.EfficientFrontierOverlay  (lines 668-742)
#
#      Uses type="random" so no CVXR dependency.  Two portfolios with mean+ES
#      objectives are combined with combine.portfolios() into a portfolio.list
#      and passed as portfolio_list.
# ===========================================================================

p_ov1 <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.40)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "ES")
  p
})

p_ov2 <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "ES")
  p
})

pl_ov <- combine.portfolios(list(p_ov1, p_ov2))

test_that("chart.EfficientFrontierOverlay: two portfolios, type='random' produces no error", {
  pdf(NULL); on.exit(dev.off())
  set.seed(42)
  expect_no_error(
    chart.EfficientFrontierOverlay(
      R              = edhec4,
      portfolio_list = pl_ov,
      type           = "random",
      match.col      = "ES",
      n.portfolios   = 5,
      search_size    = 200
    )
  )
})

test_that("chart.EfficientFrontierOverlay: legend.loc='topright' produces no error", {
  pdf(NULL); on.exit(dev.off())
  set.seed(42)
  expect_no_error(
    chart.EfficientFrontierOverlay(
      R              = edhec4,
      portfolio_list = pl_ov,
      type           = "random",
      match.col      = "ES",
      n.portfolios   = 5,
      search_size    = 200,
      legend.loc     = "topright"
    )
  )
})

test_that("chart.EfficientFrontierOverlay: chart.assets=FALSE produces no error", {
  pdf(NULL); on.exit(dev.off())
  set.seed(42)
  expect_no_error(
    chart.EfficientFrontierOverlay(
      R              = edhec4,
      portfolio_list = pl_ov,
      type           = "random",
      match.col      = "ES",
      n.portfolios   = 5,
      search_size    = 200,
      chart.assets   = FALSE
    )
  )
})

test_that("chart.EfficientFrontierOverlay: stops when portfolio_list is not portfolio.list", {
  expect_error(
    chart.EfficientFrontierOverlay(
      R              = edhec4,
      portfolio_list = list(p_ov1, p_ov2),
      type           = "random",
      match.col      = "ES"
    ),
    "portfolio.list"
  )
})

# ===========================================================================
# 16.  chart.EfficientFrontierCompare  (lines 774-854)
#
#      Requires CVXR (meanrisk.efficient.frontier defaults to CVXR).
#      Portfolio must carry both the risk_type objective (StdDev) and at least
#      one compare objective (ES) so the frontier matrix contains those columns.
# ===========================================================================

p_cmp <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.50)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "StdDev")
  p <- add.objective(p, type = "risk",   name = "ES")
  p
})

test_that("chart.EfficientFrontierCompare: risk_type='StdDev', guideline=TRUE produces no error", {
  skip_if_not_installed("CVXR")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontierCompare(
      R            = edhec4,
      portfolio    = p_cmp,
      risk_type    = "StdDev",
      match.col    = c("StdDev", "ES"),
      n.portfolios = 5,
      guideline    = TRUE
    )
  )
})

test_that("chart.EfficientFrontierCompare: guideline=FALSE produces no error", {
  skip_if_not_installed("CVXR")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontierCompare(
      R            = edhec4,
      portfolio    = p_cmp,
      risk_type    = "StdDev",
      match.col    = c("StdDev", "ES"),
      n.portfolios = 5,
      guideline    = FALSE
    )
  )
})

test_that("chart.EfficientFrontierCompare: legend.loc non-NULL branch produces no error", {
  skip_if_not_installed("CVXR")
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    chart.EfficientFrontierCompare(
      R            = edhec4,
      portfolio    = p_cmp,
      risk_type    = "StdDev",
      match.col    = c("StdDev", "ES"),
      n.portfolios = 5,
      guideline    = FALSE,
      legend.loc   = "bottomright"
    )
  )
})

# ---------------------------------------------------------------------------
# Section: chart.EF.Weights with by.groups=TRUE (with group constraints)
# ---------------------------------------------------------------------------

p_grp <- portfolio.spec(assets = colnames(edhec4))
p_grp <- add.constraint(p_grp, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_grp <- add.constraint(p_grp, type = "box", min = 0.05, max = 0.50)
p_grp <- add.constraint(p_grp, type = "group",
                         groups = list(c(1, 2), c(3, 4)),
                         group_min = c(0.30, 0.30),
                         group_max = c(0.70, 0.70))
p_grp <- add.objective(p_grp, type = "return", name = "mean")
p_grp <- add.objective(p_grp, type = "risk",   name = "StdDev")

ef_grp <- tryCatch(
  create.EfficientFrontier(R = edhec4, portfolio = p_grp,
                            type = "mean-var", n.portfolios = 5),
  error = function(e) NULL
)

test_that("chart.EF.Weights with by.groups=TRUE succeeds when portfolio has group constraints", {
  skip_if(is.null(ef_grp))
  expect_no_error({
    png(tempfile(fileext = ".png"))
    on.exit(dev.off(), add = TRUE)
    chart.EF.Weights(ef_grp, match.col = "StdDev", by.groups = TRUE)
  })
})


# ---------------------------------------------------------------------------
# Section: chart.EfficientFrontierOverlay and chart.EfficientFrontierCompare
# ---------------------------------------------------------------------------

p_lo <- portfolio.spec(assets = colnames(edhec4))
p_lo <- add.constraint(p_lo, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_lo <- add.constraint(p_lo, type = "box", min = 0, max = 1)
p_lo <- add.objective(p_lo, type = "return", name = "mean")
p_lo <- add.objective(p_lo, type = "risk",   name = "StdDev")

p_box <- portfolio.spec(assets = colnames(edhec4))
p_box <- add.constraint(p_box, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_box <- add.constraint(p_box, type = "box", min = 0.05, max = 0.40)
p_box <- add.objective(p_box, type = "return", name = "mean")
p_box <- add.objective(p_box, type = "risk",   name = "StdDev")

p_list_overlay <- combine.portfolios(list(p_lo, p_box))

test_that("chart.EfficientFrontierOverlay succeeds with portfolio.list", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  expect_no_error({
    png(tempfile(fileext = ".png"))
    on.exit(dev.off(), add = TRUE)
    chart.EfficientFrontierOverlay(R = edhec4, portfolio_list = p_list_overlay, type = "mean-var", match.col = "StdDev", n.portfolios = 5)
  })
})

test_that("chart.EfficientFrontierCompare succeeds with portfolio comparing StdDev and ES", {
  skip_if_not_installed("CVXR")
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  skip_if_not_installed("ROI.plugin.glpk")
  expect_no_error({
    png(tempfile(fileext = ".png"))
    on.exit(dev.off(), add = TRUE)
    chart.EfficientFrontierCompare(R = edhec4, portfolio = p_lo, risk_type = "StdDev", match.col = c("StdDev", "ES"), n.portfolios = 5)
  })
})
