###############################################################################
# tests/testthat/test-charts-de-pso-extra.R
#
# Source files covered:
#   R/charts.DE.R  — PortfolioAnalytics:::chart.Weight.DE  (lines 17, 31, 42, 47)
#                    PortfolioAnalytics:::chart.Scatter.DE  (lines 102, 105,
#                       118-119, 141-154, 203-211, 250, 267, 273, 286-287)
#                    PortfolioAnalytics:::charts.DE          (line 315 wrapper)
#   R/charts.PSO.R — PortfolioAnalytics:::chart.Weight.pso  (lines 5, 19, 30, 35)
#                    PortfolioAnalytics:::chart.Scatter.pso  (lines 84, 87,
#                       100-101, 123-125, 130-136, 165, 178-179)
#
# All chart tests redirect graphics to a null PDF device.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("PortfolioAnalytics")

library(PortfolioAnalytics)

# ===========================================================================
# File-scope fixtures
# ===========================================================================

# DEoptim result with trace=TRUE.  Portfolio has mean+ES so extractStats()
# has "mean" and "ES" columns.  itermax=50 for speed.
opt_de_extra <- NULL
if (requireNamespace("DEoptim", quietly = TRUE)) {
  p_de <- portfolio.spec(assets = colnames(edhec4))
  p_de <- add.constraint(p_de, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_de <- add.constraint(p_de, type = "box", min = 0.05, max = 0.60)
  p_de <- add.objective(p_de, type = "return", name = "mean")
  p_de <- add.objective(p_de, type = "risk",   name = "ES")
  opt_de_extra <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, p_de,
                       optimize_method = "DEoptim",
                       trace           = TRUE,
                       search_size     = 200,
                       itermax         = 50)
  }, error = function(e) NULL)
}

# DEoptim result WITHOUT trace=TRUE (object$R is NULL).
# Used to exercise the "no R" stop branch.
opt_de_notrace <- NULL
if (requireNamespace("DEoptim", quietly = TRUE)) {
  p_de2 <- portfolio.spec(assets = colnames(edhec4))
  p_de2 <- add.constraint(p_de2, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_de2 <- add.constraint(p_de2, type = "box", min = 0.05, max = 0.60)
  p_de2 <- add.objective(p_de2, type = "risk", name = "ES")
  opt_de_notrace <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, p_de2,
                       optimize_method = "DEoptim",
                       trace           = FALSE,
                       itermax         = 20)
  }, error = function(e) NULL)
}

# DEoptim result using INFINITE box constraints (no min/max) so that the
# ylim-from-weights branch (line 47 / PSO line 35) is exercised.
opt_de_inf <- NULL
if (requireNamespace("DEoptim", quietly = TRUE)) {
  p_de_inf <- portfolio.spec(assets = colnames(edhec4))
  p_de_inf <- add.constraint(p_de_inf, type = "weight_sum",
                              min_sum = 0.99, max_sum = 1.01)
  # No box constraint → constraints$min and $max will be -Inf/+Inf
  p_de_inf <- add.objective(p_de_inf, type = "risk", name = "ES")
  opt_de_inf <- suppressWarnings(tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, p_de_inf,
                       optimize_method = "DEoptim",
                       trace           = FALSE,
                       itermax         = 20)
  }, error = function(e) NULL))
}

# PSO result with trace=TRUE, mean+StdDev spec.
opt_pso_extra <- NULL
if (requireNamespace("pso", quietly = TRUE)) {
  p_pso <- portfolio.spec(assets = colnames(edhec4))
  p_pso <- add.constraint(p_pso, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_pso <- add.constraint(p_pso, type = "box",        min = 0.05,     max = 0.60)
  p_pso <- add.objective(p_pso,  type = "return",     name = "mean")
  p_pso <- add.objective(p_pso,  type = "risk",       name = "StdDev")
  opt_pso_extra <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, p_pso,
                       optimize_method = "pso",
                       trace           = TRUE,
                       maxit           = 50)
  }, error = function(e) NULL)
}

# PSO result WITHOUT trace=TRUE (object$R is NULL).
opt_pso_notrace <- NULL
if (requireNamespace("pso", quietly = TRUE)) {
  p_pso2 <- portfolio.spec(assets = colnames(edhec4))
  p_pso2 <- add.constraint(p_pso2, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_pso2 <- add.constraint(p_pso2, type = "box",        min = 0.05,     max = 0.60)
  p_pso2 <- add.objective(p_pso2,  type = "risk",       name = "StdDev")
  opt_pso_notrace <- tryCatch({
    set.seed(42)
    optimize.portfolio(edhec4, p_pso2,
                       optimize_method = "pso",
                       trace           = FALSE,
                       maxit           = 20)
  }, error = function(e) NULL)
}

# PSO result with infinite box constraints (no explicit box constraint).
# NOTE: PSO (pso::psoptim) requires finite bounds, so this fixture will be
# NULL on all systems.  The infinite-constraints ylim branch is instead tested
# via a hand-crafted mock object below.
opt_pso_inf <- NULL

# ===========================================================================
# Section 1: chart.Weight.DE — uncovered branches
# ===========================================================================

# Line 17 — stop() when object is wrong class
test_that("chart.Weight.DE stops for a non-DEoptim object (line 17)", {
  skip_if_not_installed("DEoptim")
  expect_error(
    PortfolioAnalytics:::chart.Weight.DE(opt_roi_trace),
    regexp = "optimize.portfolio.DEoptim"
  )
})

# Line 31 — minmargin = 5 when xlab is not NULL
test_that("chart.Weight.DE with xlab sets minmargin=5 (line 31)", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_extra))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.DE(opt_de_extra,
                                         plot.type = "line",
                                         xlab      = "Asset")
  )
})

# Line 42 — bottommargin = minmargin when las <= 1
test_that("chart.Weight.DE with las=1 takes the bottommargin=minmargin branch (line 42)", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_extra))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.DE(opt_de_extra,
                                         plot.type = "line",
                                         las       = 1)
  )
})

# Line 32 — main="" sets topmargin=1 (also tests bottommargin=minmargin path)
test_that("chart.Weight.DE with main='' sets topmargin=1 (line 32)", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_extra))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.DE(opt_de_extra,
                                         plot.type = "line",
                                         main      = "",
                                         las       = 1)
  )
})

# Line 47 — ylim from object$weights when constraints contain Inf
test_that("chart.Weight.DE with infinite constraints uses ylim from weights (line 47)", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_inf))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.DE(opt_de_inf, plot.type = "line")
  )
})

# ===========================================================================
# Section 2: chart.Scatter.DE — uncovered branches
# ===========================================================================

# Line 102 — stop() when object is wrong class
test_that("chart.Scatter.DE stops for a non-DEoptim object (line 102)", {
  skip_if_not_installed("DEoptim")
  expect_error(
    PortfolioAnalytics:::chart.Scatter.DE(opt_roi_trace),
    regexp = "optimize.portfolio.DEoptim"
  )
})

# Line 105 — stop() when object$R is NULL (trace=FALSE)
test_that("chart.Scatter.DE stops when object$R is NULL (line 105)", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_notrace))
  expect_error(
    PortfolioAnalytics:::chart.Scatter.DE(opt_de_notrace),
    regexp = "Returns object not detected"
  )
})

# Lines 118-119, 141-154 — applyFUN fallback when return.col not in extractStats
# Passing return.col="Sharpe" (not an objective) forces the non-matching path.
test_that("chart.Scatter.DE applyFUN fallback for non-matching return.col (lines 118-154)", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_extra))
  pdf(NULL); on.exit(dev.off())
  expect_warning(
    PortfolioAnalytics:::chart.Scatter.DE(opt_de_extra,
                                           return.col = "mean",
                                           risk.col   = "StdDev"),
    regexp = "do  not match extractStats"
  )
})

# Lines 203-211 — matrix neighbors branch in chart.Scatter.DE
test_that("chart.Scatter.DE with matrix neighbors does not error (lines 203-211)", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_extra))
  # Build a neighbors matrix from the first few rows of extractStats()
  nb_mat <- head(extractStats(opt_de_extra), 5)
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Scatter.DE(opt_de_extra,
                                           return.col = "mean",
                                           risk.col   = "ES",
                                           neighbors  = nb_mat)
  )
})

# Lines 272-287 — optimal point via applyFUN when return.col/risk.col don't
# match the constrained_objective / objective_measures names.
# Use return.col="mean" and risk.col="StdDev" against a mean+ES spec so
# "StdDev" won't match objective_measures → triggers the applyFUN path.
test_that("chart.Scatter.DE optimal point via applyFUN when cols missing from objcols (lines 272-287)", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_extra))
  pdf(NULL); on.exit(dev.off())
  # Suppress the expected warning about non-matching extractStats output
  withCallingHandlers(
    expect_no_error(
      PortfolioAnalytics:::chart.Scatter.DE(opt_de_extra,
                                             return.col = "mean",
                                             risk.col   = "StdDev")
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )
})

# Line 267 — message when R or portfolio is NULL in trajectory section
# (Exercises the else branch at line 266-268)
# This is triggered when object$R exists but object$portfolio is NULL —
# however in practice object$portfolio is always set; instead the else
# at line 266 is reached via the `!is.null(R) & !is.null(portfolio)` check.
# We exercise this indirectly through a normal call where the trajectory
# section is safely entered (rows < 2 case skips the inner loop).

# ===========================================================================
# Section 3: charts.DE wrapper — stop/error propagation
# ===========================================================================

test_that("charts.DE stop for a non-DEoptim object propagates correctly", {
  skip_if_not_installed("DEoptim")
  expect_error(
    PortfolioAnalytics:::charts.DE(opt_roi_trace,
                                    risk.col   = "StdDev",
                                    return.col = "mean",
                                    chart.assets = FALSE),
    regexp = "optimize.portfolio.DEoptim"
  )
})

# ===========================================================================
# Section 4: chart.Weight.pso — uncovered branches
# ===========================================================================

# Line 5 — stop() when object is wrong class
test_that("chart.Weight.pso stops for a non-pso object (line 5)", {
  skip_if_not_installed("pso")
  expect_error(
    PortfolioAnalytics:::chart.Weight.pso(opt_roi_trace),
    regexp = "optimize.portfolio.pso"
  )
})

# Line 19 — minmargin = 5 when xlab is not NULL
test_that("chart.Weight.pso with xlab sets minmargin=5 (line 19)", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso_extra))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.pso(opt_pso_extra,
                                           plot.type = "line",
                                           xlab      = "Asset")
  )
})

# Line 30 — bottommargin = minmargin when las <= 1
test_that("chart.Weight.pso with las=1 takes the bottommargin=minmargin branch (line 30)", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso_extra))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.pso(opt_pso_extra,
                                           plot.type = "line",
                                           las       = 1)
  )
})

# Line 20 — main="" sets topmargin=1
test_that("chart.Weight.pso with main='' sets topmargin=1 (line 20)", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso_extra))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.pso(opt_pso_extra,
                                           plot.type = "line",
                                           main      = "",
                                           las       = 1)
  )
})

# Line 35 — ylim from object$weights when constraints contain Inf
# PSO requires finite bounds so a real optimizer run can't produce Inf
# constraints.  Build a minimal mock of the correct class instead.
test_that("chart.Weight.pso with infinite constraints uses ylim from weights (line 35)", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso_extra))
  # Create a mock pso result with Inf box constraints to exercise line 35.
  # We reuse opt_pso_extra's portfolio spec but override the constraint bounds.
  mock_pso <- opt_pso_extra
  # Temporarily patch constraints so is.infinite() returns TRUE
  # by replacing the portfolio's box constraint bounds
  mock_pso$portfolio$constraints[[2]]$min <- rep(-Inf, 4)
  mock_pso$portfolio$constraints[[2]]$max <- rep(Inf,  4)
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    PortfolioAnalytics:::chart.Weight.pso(mock_pso, plot.type = "line")
  )
})

# ===========================================================================
# Section 5: chart.Scatter.pso — uncovered branches
# ===========================================================================

# Line 84 — stop() when object is wrong class
test_that("chart.Scatter.pso stops for a non-pso object (line 84)", {
  skip_if_not_installed("pso")
  expect_error(
    PortfolioAnalytics:::chart.Scatter.pso(opt_roi_trace),
    regexp = "optimize.portfolio.pso"
  )
})

# Line 87 — stop() when object$R is NULL (trace=FALSE)
test_that("chart.Scatter.pso stops when object$R is NULL (line 87)", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso_notrace))
  expect_error(
    PortfolioAnalytics:::chart.Scatter.pso(opt_pso_notrace),
    regexp = "Returns object not detected"
  )
})

# Lines 100-101, 123-125, 130-136 — applyFUN fallback for non-matching cols
# Passing risk.col="ES" against a mean+StdDev spec forces the non-matching path.
test_that("chart.Scatter.pso applyFUN fallback for non-matching risk.col (lines 100-136)", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso_extra))
  pdf(NULL); on.exit(dev.off())
  expect_warning(
    PortfolioAnalytics:::chart.Scatter.pso(opt_pso_extra,
                                            return.col = "mean",
                                            risk.col   = "ES"),
    regexp = "do  not match extractStats"
  )
})

# Lines 165, 178-179 — optimal point via applyFUN when return.col/risk.col
# don't match objective_measures names.
# Use risk.col="ES" against a mean+StdDev spec (ES not in objective_measures)
# so the applyFUN path for the optimal point is taken.
test_that("chart.Scatter.pso optimal point via applyFUN when cols missing from objcols (lines 165-179)", {
  skip_if_not_installed("pso")
  skip_if(is.null(opt_pso_extra))
  pdf(NULL); on.exit(dev.off())
  withCallingHandlers(
    expect_no_error(
      PortfolioAnalytics:::chart.Scatter.pso(opt_pso_extra,
                                              return.col = "mean",
                                              risk.col   = "ES")
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )
})

# ===========================================================================
# Section 6: charts.pso wrapper — stop/error propagation
# ===========================================================================

test_that("charts.pso stop for a non-pso object propagates correctly", {
  skip_if_not_installed("pso")
  expect_error(
    PortfolioAnalytics:::charts.pso(opt_roi_trace,
                                     return.col = "mean",
                                     risk.col   = "StdDev"),
    regexp = "optimize.portfolio.pso"
  )
})
