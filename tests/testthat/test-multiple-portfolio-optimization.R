###############################################################################
# tests/testthat/test-multiple-portfolio-optimization.R
#
# Source files covered:
#   R/utility.combine.R  — combine.portfolios(), combine.optimizations()
#   R/generics.R         — print.portfolio.list(), print.opt.list()
#   R/optimize.portfolio.R — portfolio.list dispatch: passing a portfolio.list
#                            to optimize.portfolio() returns an opt.list
#   R/extractstats.R     — extractWeights.opt.list(), extractStats.opt.list(),
#                          extractObjectiveMeasures.opt.list()
#
# Tests that combine.portfolios() produces a valid portfolio.list, that
# optimizing over a portfolio.list produces a valid opt.list, and that
# all extract functions behave correctly on the resulting opt.list.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("ROI.plugin.glpk")

library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

# ---------------------------------------------------------------------------
# Shared data and portfolio specs (file scope)
# ---------------------------------------------------------------------------

R   <- edhec4
nms <- funds4

portf_init <- portfolio.spec(assets = nms)
portf_init <- add.constraint(portf_init, type = "weight_sum",
                             min_sum = 0.99, max_sum = 1.01)
portf_init <- add.constraint(portf_init, type = "long_only")

portf_minsd  <- add.objective(portf_init, type = "risk",   name = "StdDev")
portf_mines  <- add.objective(portf_init, type = "risk",   name = "ES")
portf_maxret <- add.objective(portf_init, type = "return", name = "mean")

# Combine into a portfolio.list and optimise all portfolios in one call
# trace=TRUE is required so that each result stores $R, which is needed by
# extractObjectiveMeasures.opt.list when portfolios have differing objectives
mult_portf <- combine.portfolios(list(portf_minsd, portf_mines, portf_maxret))
mult_opt   <- optimize.portfolio(R, mult_portf, optimize_method = "ROI", trace = TRUE)

# ---------------------------------------------------------------------------
# portfolio.list constructor checks
# ---------------------------------------------------------------------------

test_that("combine.portfolios returns class portfolio.list", {
  expect_s3_class(mult_portf, "portfolio.list")
})

test_that("portfolio.list has length 3", {
  expect_equal(length(mult_portf), 3L)
})

test_that("print.portfolio.list produces output without error", {
  expect_output(print(mult_portf))
})

# ---------------------------------------------------------------------------
# opt.list returned by optimize.portfolio on a portfolio.list
# ---------------------------------------------------------------------------

test_that("optimize.portfolio on portfolio.list returns class opt.list", {
  expect_s3_class(mult_opt, "opt.list")
})

test_that("opt.list has length 3 (one result per portfolio)", {
  expect_equal(length(mult_opt), 3L)
})

test_that("print.opt.list produces output without error", {
  expect_output(print(mult_opt))
})

# ---------------------------------------------------------------------------
# Each individual result inside the opt.list is a valid ROI result
# ---------------------------------------------------------------------------

test_that("each element of opt.list has class optimize.portfolio.ROI", {
  for (i in seq_along(mult_opt)) {
    expect_s3_class(mult_opt[[i]], "optimize.portfolio.ROI")
  }
})

# ---------------------------------------------------------------------------
# extractWeights on opt.list
# ---------------------------------------------------------------------------

test_that("extractWeights on opt.list returns a matrix", {
  w_mat <- extractWeights(mult_opt)
  expect_true(is.matrix(w_mat))
})

test_that("extractWeights matrix has 3 rows (one per portfolio)", {
  w_mat <- extractWeights(mult_opt)
  expect_equal(nrow(w_mat), 3L)
})

test_that("extractWeights matrix has 4 columns (one per asset)", {
  w_mat <- extractWeights(mult_opt)
  expect_equal(ncol(w_mat), 4L)
})

test_that("extractWeights matrix rows each sum to approximately 1 (within 0.02)", {
  w_mat    <- extractWeights(mult_opt)
  row_sums <- rowSums(w_mat)
  # weight_sum constraint allows [0.99, 1.01]; add small numerical slack
  expect_true(all(abs(row_sums - 1) < 0.02))
})

test_that("extractWeights matrix all weights are non-negative (long only)", {
  w_mat <- extractWeights(mult_opt)
  expect_true(all(w_mat >= -TOL_WSUM))
})

test_that("extractWeights matrix has asset names as column names", {
  w_mat <- extractWeights(mult_opt)
  expect_true(!is.null(colnames(w_mat)))
  expect_true(all(colnames(w_mat) %in% nms))
})

# ---------------------------------------------------------------------------
# extractObjectiveMeasures on opt.list
# ---------------------------------------------------------------------------

test_that("extractObjectiveMeasures on opt.list is non-null", {
  obj_measures <- extractObjectiveMeasures(mult_opt)
  expect_false(is.null(obj_measures))
})

test_that("extractObjectiveMeasures on opt.list covers all 3 portfolios", {
  obj_measures <- extractObjectiveMeasures(mult_opt)
  # may be a matrix (nrow=3) or a named list (length=3)
  n <- if (is.null(dim(obj_measures))) length(obj_measures) else nrow(obj_measures)
  expect_equal(n, 3L)
})

test_that("extractObjectiveMeasures on opt.list: no row is entirely NA", {
  obj_measures <- extractObjectiveMeasures(mult_opt)
  if (is.matrix(obj_measures) || is.data.frame(obj_measures)) {
    for (i in seq_len(nrow(obj_measures))) {
      expect_false(all(is.na(obj_measures[i, ])))
    }
  } else {
    for (i in seq_along(obj_measures)) {
      expect_false(is.null(obj_measures[[i]]))
    }
  }
})

# ---------------------------------------------------------------------------
# extractStats on opt.list
# ---------------------------------------------------------------------------

test_that("extractStats on opt.list returns a list", {
  stats_list <- extractStats(mult_opt)
  expect_true(is.list(stats_list))
})

test_that("extractStats on opt.list has length 3", {
  stats_list <- extractStats(mult_opt)
  expect_equal(length(stats_list), 3L)
})

test_that("extractStats on opt.list: each element is a named numeric vector", {
  stats_list <- extractStats(mult_opt)
  for (i in seq_along(stats_list)) {
    expect_true(is.numeric(stats_list[[i]]),
                label = paste("element", i, "is numeric"))
    expect_false(is.null(names(stats_list[[i]])),
                 label = paste("element", i, "has names"))
  }
})

test_that("extractStats on opt.list: each element contains weight columns", {
  stats_list <- extractStats(mult_opt)
  for (i in seq_along(stats_list)) {
    has_w_cols <- any(grepl("^w\\.", names(stats_list[[i]])))
    expect_true(has_w_cols,
                label = paste("element", i, "has w.* weight columns"))
  }
})

test_that("extractStats on opt.list: each element contains 'out' column", {
  stats_list <- extractStats(mult_opt)
  for (i in seq_along(stats_list)) {
    expect_true("out" %in% names(stats_list[[i]]),
                label = paste("element", i, "has 'out' in names"))
  }
})

# ---------------------------------------------------------------------------
# Consistency: extractWeights row-by-row matches individual result weights
# ---------------------------------------------------------------------------

test_that("extractWeights matrix rows match individual optimize.portfolio weights", {
  w_mat <- extractWeights(mult_opt)
  for (i in seq_along(mult_opt)) {
    w_i <- extractWeights(mult_opt[[i]])
    expect_equal(as.numeric(w_mat[i, ]), as.numeric(w_i), tolerance = TOL_OPT)
  }
})

# ---------------------------------------------------------------------------
# combine.optimizations() creates the same opt.list class
# ---------------------------------------------------------------------------

test_that("combine.optimizations wraps individual results into an opt.list", {
  # Run three independent optimisations and combine manually
  o1 <- optimize.portfolio(R, portf_minsd,  optimize_method = "ROI", trace = TRUE)
  o2 <- optimize.portfolio(R, portf_mines,  optimize_method = "ROI", trace = TRUE)
  o3 <- optimize.portfolio(R, portf_maxret, optimize_method = "ROI", trace = TRUE)
  combined <- combine.optimizations(list(o1, o2, o3))
  expect_s3_class(combined, "opt.list")
  expect_equal(length(combined), 3L)
})
