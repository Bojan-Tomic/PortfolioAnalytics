###############################################################################
# tests/testthat/test-generics.R
#
# Tests for print/summary S3 methods in R/generics.R
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------
utils::data(edhec)
edhec4 <- edhec[, 1:4]

# --- Rebalancing result ---
p_rb <- portfolio.spec(assets = colnames(edhec4))
p_rb <- add.constraint(p_rb, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_rb <- add.constraint(p_rb, type = "box", min = 0, max = 1)
p_rb <- add.objective(p_rb, type = "risk", name = "StdDev")
opt_rebal_g <- tryCatch(
  optimize.portfolio.rebalancing(edhec4["2018/"], p_rb,
                                  optimize_method = "ROI",
                                  rebalance_on = "years",
                                  training_period = 12),
  error = function(e) NULL
)

# --- Full-constraint portfolio for summary.optimize.portfolio branches ---
p_full <- portfolio.spec(assets = colnames(edhec4))
p_full <- add.constraint(p_full, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_full <- add.constraint(p_full, type = "box", min = 0, max = 1)
p_full <- add.constraint(p_full, type = "group",
                          groups = list(c(1, 2), c(3, 4)),
                          group_min = c(0.3, 0.3), group_max = c(0.7, 0.7))
p_full <- add.constraint(p_full, type = "diversification", div_target = 0.5)
p_full <- add.constraint(p_full, type = "turnover", turnover_target = 0.5)
p_full <- add.constraint(p_full, type = "position_limit", max_pos = 3)
p_full <- add.objective(p_full, type = "risk", name = "StdDev")
opt_full_g <- tryCatch(
  optimize.portfolio(edhec4, p_full, optimize_method = "ROI"),
  error = function(e) NULL
)

# --- Efficient frontier ---
p_ef <- portfolio.spec(assets = colnames(edhec4))
p_ef <- add.constraint(p_ef, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_ef <- add.constraint(p_ef, type = "box", min = 0, max = 1)
p_ef <- add.objective(p_ef, type = "return", name = "mean")
p_ef <- add.objective(p_ef, type = "risk", name = "StdDev")
ef_g <- tryCatch(
  create.EfficientFrontier(R = edhec4, portfolio = p_ef, type = "mean-var", n.portfolios = 5),
  error = function(e) NULL
)

# --- portfolio.list and opt.list ---
p_lo <- portfolio.spec(assets = colnames(edhec4))
p_lo <- add.constraint(p_lo, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_lo <- add.constraint(p_lo, type = "box", min = 0, max = 1)
p_lo <- add.objective(p_lo, type = "risk", name = "StdDev")
p_mr <- portfolio.spec(assets = colnames(edhec4))
p_mr <- add.constraint(p_mr, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_mr <- add.constraint(p_mr, type = "box", min = 0, max = 1)
p_mr <- add.objective(p_mr, type = "return", name = "mean")
p_list_g <- combine.portfolios(list(p_lo, p_mr))

opt_list_g <- tryCatch(
  optimize.portfolio(edhec4, p_list_g, optimize_method = "ROI"),
  error = function(e) NULL
)

opt_rebal_list_g <- tryCatch(
  optimize.portfolio.rebalancing(edhec4["2018/"], p_list_g,
                                  optimize_method = "ROI",
                                  rebalance_on = "years",
                                  training_period = 12),
  error = function(e) NULL
)

# Regime Portfolios
regime_xts <- xts(sample(1:2, nrow(edhec4), replace=TRUE), index(edhec4))
regime_p <- tryCatch(regime.portfolios(regime = regime_xts, portfolios = p_list_g), error=function(e) NULL)

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("print.optimize.portfolio.rebalancing produces output", {
  skip_if(is.null(opt_rebal_g))
  expect_output(print(opt_rebal_g))
})

test_that("summary.optimize.portfolio.rebalancing returns a list", {
  skip_if(is.null(opt_rebal_g))
  s <- summary(opt_rebal_g)
  expect_true(is.list(s))
})

test_that("print.summary.optimize.portfolio.rebalancing produces output", {
  skip_if(is.null(opt_rebal_g))
  s <- summary(opt_rebal_g)
  expect_output(print(s))
})

test_that("print.summary.optimize.portfolio produces output with full constraints", {
  skip_if(is.null(opt_full_g))
  s <- summary(opt_full_g)
  expect_output(print(s))
})

test_that("print.efficient.frontier produces output", {
  skip_if(is.null(ef_g))
  expect_output(print(ef_g))
})

test_that("summary.efficient.frontier returns a list", {
  skip_if(is.null(ef_g))
  s <- summary(ef_g)
  expect_true(is.list(s))
})

test_that("print.portfolio.list produces output", {
  skip_if(is.null(p_list_g))
  expect_output(print(p_list_g))
})

test_that("print.opt.list produces output", {
  skip_if(is.null(opt_list_g))
  expect_output(print(opt_list_g))
})

test_that("print.opt.rebal.list produces output", {
  skip_if(is.null(opt_rebal_list_g))
  expect_output(print(opt_rebal_list_g))
})

test_that("print.regime.portfolios produces output", {
  skip_if(is.null(regime_p))
  expect_output(print(regime_p))
})

# Note: We skipped print.optimize.portfolio.parallel, GenSA, PSO, MCO
# because these are stochastics that take a long time and some packages are not guaranteed to be installed.
# We will use the ones generated in the helper file.

test_that("print.optimize.portfolio.GenSA produces output", {
  skip_if(is.null(opt_gensa))
  expect_output(print(opt_gensa))
})

test_that("print.optimize.portfolio.pso produces output", {
  skip_if(is.null(opt_pso))
  expect_output(print(opt_pso))
})

test_that("print.optimize.portfolio.mco produces output", {
  skip_if(is.null(opt_mco))
  expect_output(print(opt_mco))
})

test_that("print.optimize.portfolio.parallel produces output", {
  skip_if(is.null(opt_parallel))
  expect_output(print(opt_parallel))
})

test_that("summary.optimize.portfolio.parallel returns a list", {
  skip_if(is.null(opt_parallel))
  s <- summary(opt_parallel)
  expect_true(is.list(s))
})

