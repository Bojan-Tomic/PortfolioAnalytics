###############################################################################
# tests/testthat/test-backtest-apply.R
#
# Source files covered:
#   R/backtest.plot.R — backtest.plot (plotType variants, log_return, drawdown_on)
#   R/applyFUN.R      — applyFUN (matrix and vector paths), scatterFUN
#   R/plotFrontiers.R — plotFrontiers (StdDev, ES, multi-frontier)
#
# All chart tests redirect graphics to a null PDF device.
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
# ---------------------------------------------------------------------------

# Multi-column returns for backtest.plot (needs >=2 portfolios to trigger
# the multi-series code paths inside backtest.plot)
R_bt <- local({
  w1 <- c(0.25, 0.25, 0.25, 0.25)  # equal weight
  w2 <- c(0.40, 0.30, 0.20, 0.10)  # tilted
  p1 <- xts::xts(as.numeric(edhec4 %*% w1), order.by = zoo::index(edhec4))
  p2 <- xts::xts(as.numeric(edhec4 %*% w2), order.by = zoo::index(edhec4))
  r  <- cbind(p1, p2)
  colnames(r) <- c("EW", "Tilted")
  r
})

# Weights matrix for applyFUN — matrix path exercises the for-loop branch
wts_mat <- matrix(
  c(0.25, 0.25, 0.25, 0.25,
    0.40, 0.30, 0.20, 0.10,
    0.10, 0.20, 0.30, 0.40),
  nrow = 3, byrow = TRUE
)
colnames(wts_mat) <- colnames(edhec4)

# Single weight vector for applyFUN — exercises the else (single-vector) branch
wts_vec <- c(0.25, 0.25, 0.25, 0.25)
names(wts_vec) <- colnames(edhec4)

# Efficient frontiers for plotFrontiers — wrapped in tryCatch so a solver
# failure (e.g. CVXR not available) degrades gracefully to skip
ef_sd <- tryCatch(
  meanvar.efficient.frontier(.portf_meansd4, edhec4, n.portfolios = 10),
  error = function(e) NULL
)

ef_es <- tryCatch(
  meanetl.efficient.frontier(.portf_meanES4, edhec4, n.portfolios = 10),
  error = function(e) NULL
)

# ===========================================================================
# backtest.plot tests
# ===========================================================================

test_that("backtest.plot plotType='both' (default) runs without error", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(backtest.plot(R_bt))
})

test_that("backtest.plot plotType='cumRet' runs without error", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(backtest.plot(R_bt, plotType = "cumRet"))
})

test_that("backtest.plot plotType='ret' alias runs without error", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(backtest.plot(R_bt, plotType = "ret"))
})

test_that("backtest.plot plotType='drawdown' runs without error", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(backtest.plot(R_bt, plotType = "drawdown"))
})

test_that("backtest.plot log_return=TRUE runs without error", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(backtest.plot(R_bt, log_return = TRUE))
})

test_that("backtest.plot drawdown_on=NULL skips interval highlighting", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(backtest.plot(R_bt, drawdown_on = NULL))
})

test_that("backtest.plot returns an object invisibly", {
  pdf(NULL); on.exit(dev.off())
  result <- backtest.plot(R_bt)
  expect_false(is.null(result))
})

test_that("backtest.plot plotType='both' with log_return=TRUE runs without error", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(backtest.plot(R_bt, log_return = TRUE, plotType = "both"))
})

test_that("backtest.plot plotType='drawdown' with drawdown_on=NULL runs without error", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(backtest.plot(R_bt, plotType = "drawdown", drawdown_on = NULL))
})

test_that("backtest.plot accepts custom colorSet, ltySet, lwdSet", {
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    backtest.plot(R_bt,
                  colorSet = c("steelblue", "firebrick"),
                  ltySet   = c(1L, 2L),
                  lwdSet   = c(2, 2))
  )
})

# ===========================================================================
# applyFUN tests
# ===========================================================================

test_that("applyFUN with matrix weights and FUN='mean' returns numeric vector", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_mat, FUN = "mean", arguments = args)
  expect_true(is.numeric(out))
  expect_length(out, nrow(wts_mat))
  expect_false(any(is.na(out)))
})

test_that("applyFUN matrix-weights mean values are plausible portfolio returns", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_mat, FUN = "mean", arguments = args)
  # Monthly mean returns of edhec funds are small positive numbers
  expect_true(all(out > -0.1))
  expect_true(all(out <  0.1))
})

test_that("applyFUN with matrix weights and FUN='StdDev' returns numeric vector", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_mat, FUN = "StdDev", arguments = args)
  expect_true(is.numeric(out))
  expect_length(out, nrow(wts_mat))
  expect_true(all(out > 0))
})

test_that("applyFUN with matrix weights and FUN='ES' returns numeric vector", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_mat, FUN = "ES", arguments = args)
  expect_true(is.numeric(out))
  expect_length(out, nrow(wts_mat))
})

test_that("applyFUN with matrix weights and FUN='VaR' returns numeric vector", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_mat, FUN = "VaR", arguments = args)
  expect_true(is.numeric(out))
  expect_length(out, nrow(wts_mat))
})

test_that("applyFUN with single vector weights and FUN='mean' returns scalar", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_vec, FUN = "mean", arguments = args)
  expect_true(is.numeric(out))
  expect_length(out, 1L)
  expect_false(is.na(out))
})

test_that("applyFUN with single vector weights and FUN='StdDev' returns scalar", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_vec, FUN = "StdDev", arguments = args)
  expect_true(is.numeric(out))
  expect_length(out, 1L)
  expect_true(out > 0)
})

test_that("applyFUN with single vector weights and FUN='ES' returns scalar", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_vec, FUN = "ES", arguments = args)
  expect_true(is.numeric(out))
  expect_length(out, 1L)
})

test_that("applyFUN with single vector weights and FUN='VaR' returns scalar", {
  args <- list()
  out  <- applyFUN(R = edhec4, weights = wts_vec, FUN = "VaR", arguments = args)
  expect_true(is.numeric(out))
  expect_length(out, 1L)
})

test_that("applyFUN matrix path: equal-weight mean equals mean of row-wise portfolio returns", {
  args   <- list()
  out    <- applyFUN(R = edhec4, weights = wts_mat, FUN = "mean", arguments = args)
  # row 1 is equal-weight (0.25 each); compute reference directly
  portR1 <- as.numeric(edhec4 %*% wts_mat[1, ])
  expect_equal(out[1], mean(portR1), tolerance = 1e-10)
})

# ===========================================================================
# scatterFUN tests
# ===========================================================================

test_that("scatterFUN FUN='mean' returns numeric vector of length ncol(R)", {
  out <- scatterFUN(R = edhec4, FUN = "mean")
  expect_true(is.numeric(out))
  expect_length(out, ncol(edhec4))
  expect_false(any(is.na(out)))
})

test_that("scatterFUN FUN='mean' values match colMeans", {
  out <- scatterFUN(R = edhec4, FUN = "mean")
  ref <- as.numeric(colMeans(edhec4))
  expect_equal(out, ref, tolerance = 1e-12)
})

test_that("scatterFUN FUN='StdDev' returns numeric vector of length ncol(R)", {
  out <- scatterFUN(R = edhec4, FUN = "StdDev")
  expect_true(is.numeric(out))
  expect_length(out, ncol(edhec4))
  expect_true(all(out > 0))
})

test_that("scatterFUN FUN='ES' returns numeric vector of length ncol(R)", {
  out <- scatterFUN(R = edhec4, FUN = "ES")
  expect_true(is.numeric(out))
  expect_length(out, ncol(edhec4))
  expect_false(any(is.na(out)))
})

test_that("scatterFUN FUN='VaR' returns numeric vector of length ncol(R)", {
  out <- scatterFUN(R = edhec4, FUN = "VaR")
  expect_true(is.numeric(out))
  expect_length(out, ncol(edhec4))
})

test_that("scatterFUN accepts explicit arguments list", {
  out <- scatterFUN(R = edhec4, FUN = "ES", arguments = list(p = 0.95))
  expect_true(is.numeric(out))
  expect_length(out, ncol(edhec4))
})

# ===========================================================================
# plotFrontiers tests
# ===========================================================================

test_that("plotFrontiers with StdDev frontier runs without error", {
  skip_if(is.null(ef_sd))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plotFrontiers(R = edhec4, frontiers = list(ef_sd), risk = "StdDev")
  )
})

test_that("plotFrontiers with ES frontier runs without error", {
  skip_if(is.null(ef_es))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plotFrontiers(R = edhec4, frontiers = list(ef_es), risk = "ES")
  )
})

test_that("plotFrontiers with two StdDev frontiers (n > 1 loop) runs without error", {
  skip_if(is.null(ef_sd))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plotFrontiers(R = edhec4, frontiers = list(ef_sd, ef_sd), risk = "StdDev")
  )
})

test_that("plotFrontiers with legend.loc='bottomright' triggers legend code path", {
  skip_if(is.null(ef_sd))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plotFrontiers(R = edhec4, frontiers = list(ef_sd), risk = "StdDev",
                  legend.loc = "bottomright")
  )
})

test_that("plotFrontiers returns invisible list with mean and risk components", {
  skip_if(is.null(ef_sd))
  pdf(NULL); on.exit(dev.off())
  result <- plotFrontiers(R = edhec4, frontiers = list(ef_sd), risk = "StdDev")
  expect_true(is.list(result))
  expect_true(!is.null(result$mean))
  expect_true(!is.null(result$risk))
})

test_that("plotFrontiers mean and risk lists have length equal to number of frontiers", {
  skip_if(is.null(ef_sd))
  pdf(NULL); on.exit(dev.off())
  n_frontiers <- 2L
  result <- plotFrontiers(R = edhec4,
                          frontiers = list(ef_sd, ef_sd),
                          risk = "StdDev")
  expect_length(result$mean, n_frontiers)
  expect_length(result$risk, n_frontiers)
})

test_that("plotFrontiers with ES frontier and two copies (n > 1) runs without error", {
  skip_if(is.null(ef_es))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plotFrontiers(R = edhec4, frontiers = list(ef_es, ef_es), risk = "ES")
  )
})

test_that("plotFrontiers accepts custom col, lty, lwd vectors", {
  skip_if(is.null(ef_sd))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plotFrontiers(R = edhec4, frontiers = list(ef_sd),
                  risk = "StdDev",
                  col = "steelblue", lty = 2L, lwd = 2)
  )
})

test_that("plotFrontiers accepts custom legend.labels", {
  skip_if(is.null(ef_sd))
  pdf(NULL); on.exit(dev.off())
  expect_no_error(
    plotFrontiers(R = edhec4, frontiers = list(ef_sd),
                  risk      = "StdDev",
                  legend.loc    = "bottomright",
                  legend.labels = "Min-Var")
  )
})
