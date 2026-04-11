###############################################################################
# tests/testthat/test-examples.R
#
# Exercises every \examples{} block from the 21 PortfolioAnalytics man pages
# that contain examples.  Each test_that() block corresponds to one Rd file
# and is named after the function documented there.
#
# File-level guards (evaluated before any test_that block):
#   skip_on_cran()   -- skip the whole file on CRAN
#
# Per-test guards for the rebalancing example:
#   skip_if_not_installed("ROI")
#   skip_if_not_installed("ROI.plugin.quadprog")
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Shared data -- loaded once at the file level so each test block can use it
# without repeating utils::data(edhec).  Individual blocks may also call data()
# locally to match the verbatim Rd example exactly.
# ---------------------------------------------------------------------------
utils::data(edhec)
ret <- edhec[, 1:5]


# ===========================================================================
# 1.  ac.ranking
# ===========================================================================
test_that("ac.ranking example runs without error", {
  R <- edhec[, 1:4]
  result <- ac.ranking(R, c(2, 3, 1, 4))
  expect_true(is.numeric(result))
})


# ===========================================================================
# 2.  add.constraint  (long example covering every constraint type)
# ===========================================================================
test_that("add.constraint example runs without error", {
  utils::data(edhec)
  returns <- edhec[, 1:4]
  fund.names <- colnames(returns)
  pspec <- portfolio.spec(assets = fund.names)

  # weight_sum variants
  pspec <- add.constraint(
    portfolio = pspec, type = "weight_sum",
    min_sum = 1, max_sum = 1
  )
  pspec <- add.constraint(portfolio = pspec, type = "full_investment")
  pspec <- add.constraint(
    portfolio = pspec, type = "weight_sum",
    min_sum = 0, max_sum = 0
  )
  pspec <- add.constraint(portfolio = pspec, type = "dollar_neutral")
  pspec <- add.constraint(portfolio = pspec, type = "active")

  # box constraints -- scalar, per-asset, and special cases
  pspec <- add.constraint(
    portfolio = pspec, type = "box",
    min = 0.05, max = 0.4
  )
  pspec <- add.constraint(
    portfolio = pspec,
    type = "box",
    min = c(0.05, 0, 0.08, 0.1),
    max = c(0.4, 0.3, 0.7, 0.55)
  )
  pspec <- add.constraint(portfolio = pspec, type = "box")
  pspec <- add.constraint(portfolio = pspec, type = "long_only")

  # group constraint
  pspec <- add.constraint(
    portfolio = pspec,
    type = "group",
    groups = list(c(1, 2, 1), 4),
    group_min = c(0.1, 0.15),
    group_max = c(0.85, 0.55),
    group_labels = c("GroupA", "GroupB"),
    group_pos = c(2, 1)
  )

  # remaining constraint types
  pspec <- add.constraint(
    portfolio = pspec, type = "position_limit",
    max_pos = 3
  )
  pspec <- add.constraint(
    portfolio = pspec, type = "diversification",
    div_target = 0.7
  )
  pspec <- add.constraint(
    portfolio = pspec, type = "turnover",
    turnover_target = 0.2
  )
  pspec <- add.constraint(
    portfolio = pspec, type = "return",
    return_target = 0.007
  )

  # ---  indexnum workflow: create, inspect, then update in place  -----------
  portf <- portfolio.spec(assets = fund.names)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")

  portf$constraints[[1]] # inspect (no assignment)
  portf$constraints[[2]]

  portf <- add.constraint(portf,
    type = "weight_sum",
    min_sum = 0.99, max_sum = 1.01,
    indexnum = 1
  )
  portf <- add.constraint(portf,
    type = "box",
    min = 0.1, max = 0.8,
    indexnum = 2
  )

  # updating via indexnum must NOT add new constraints
  expect_equal(length(portf$constraints), 2)
})


# ===========================================================================
# 3.  add.objective
# ===========================================================================
test_that("add.objective example runs without error", {
  utils::data(edhec)
  returns <- edhec[, 1:4]
  fund.names <- colnames(returns)
  portf <- portfolio.spec(assets = fund.names)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")

  portf.maxQU <- add.objective(portf,
    type = "quadratic_utility",
    risk_aversion = 0.25
  )

  portf.maxMean <- add.objective(portf, type = "return", name = "mean")

  portf.minStdDev <- add.objective(portf, type = "risk", name = "StdDev")

  portf.minES <- add.objective(portf,
    type = "risk", name = "ES",
    arguments = list(p = 0.925, clean = "boudt")
  )

  portf.RiskBudgetES <- add.objective(portf.minES,
    type = "risk_budget",
    name = "ES",
    arguments = list(
      p = 0.925,
      clean = "boudt"
    ),
    min_prisk = 0,
    max_prisk = 0.6
  )

  portf.EqRiskES <- add.objective(portf.minES,
    type = "risk_budget",
    name = "ES",
    arguments = list(
      p = 0.925,
      clean = "boudt"
    ),
    min_concentration = TRUE
  )

  portf.conc <- add.objective(portf,
    type = "weight_concentration",
    name = "HHI", conc_aversion = 0.01
  )

  expect_s3_class(portf.conc, "portfolio")
})


# ===========================================================================
# 4.  box_constraint
# ===========================================================================
test_that("box_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))

  # defaults to min=0, max=1
  pspec <- add.constraint(pspec, type = "box")

  # scalar bounds
  pspec <- add.constraint(pspec, type = "box", min = 0.05, max = 0.45)

  # per-asset bounds
  pspec <- add.constraint(pspec,
    type = "box",
    min  = c(0.05, 0.10, 0.08, 0.06),
    max  = c(0.45, 0.55, 0.35, 0.65)
  )

  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 5.  centroid.complete.mc
# ===========================================================================
test_that("centroid.complete.mc example runs without error", {
  # Express a view: R_2 < R_1 < R_3 < R_4
  result <- centroid.complete.mc(c(2, 1, 3, 4))
  expect_true(is.numeric(result))
})


# ===========================================================================
# 6.  centroid.sectors
# ===========================================================================
test_that("centroid.sectors example runs without error", {
  # Sector 1 view: R_2 < R_1 < R_3
  # Sector 2 view: R_5 < R_4
  x <- list()
  x[[1]] <- c(2, 1, 3)
  x[[2]] <- c(5, 4)
  result <- centroid.sectors(x)
  expect_true(is.numeric(result))
})


# ===========================================================================
# 7.  centroid.sign
# ===========================================================================
test_that("centroid.sign example runs without error", {
  # Express a view that R_1 < R_2 < 0 < R_3 < R_4
  result <- centroid.sign(c(1, 2), c(4, 3))
  expect_true(is.numeric(result))
})


# ===========================================================================
# 8.  diversification_constraint
# ===========================================================================
test_that("diversification_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))
  pspec <- add.constraint(
    portfolio = pspec, type = "diversification",
    div_target = 0.7
  )
  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 9.  group_constraint
# ===========================================================================
test_that("group_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))

  # Two named groups across 4 assets
  pspec <- add.constraint(
    portfolio = pspec,
    type = "group",
    groups = list(
      groupA = c(1, 3),
      groupB = c(2, 4)
    ),
    group_min = c(0.15, 0.25),
    group_max = c(0.65, 0.55)
  )

  # Two levels of grouping (e.g. sector and geography) across 5 assets
  pspec <- portfolio.spec(assets = 5)
  group_list <- list(
    group1 = c(1, 3, 5),
    group2 = c(2, 4),
    groupA = c(2, 4, 5),
    groupB = c(1, 3)
  )
  pspec <- add.constraint(
    portfolio = pspec,
    type = "group",
    groups = group_list,
    group_min = c(0.15, 0.25, 0.2, 0.1),
    group_max = c(0.65, 0.55, 0.5, 0.4)
  )

  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 10.  indexes
# ===========================================================================
test_that("indexes example runs without error", {
  data(indexes)
  head(indexes)
  summary(indexes)
  expect_true(xts::is.xts(indexes))
  expect_true(ncol(indexes) > 0)
})


# ===========================================================================
# 11.  leverage_exposure_constraint
# ===========================================================================
test_that("leverage_exposure_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))
  pspec <- add.constraint(
    portfolio = pspec, type = "leverage_exposure",
    leverage = 1.6
  )
  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 12.  meucci.ranking
# ===========================================================================
test_that("meucci.ranking example runs without error", {
  utils::data(edhec)
  R <- edhec[, 1:4]
  p <- rep(1 / nrow(R), nrow(R))
  result <- meucci.ranking(R, p, c(2, 3, 1, 4))
  expect_true(all(c("mu", "sigma") %in% names(result)))
})


# ===========================================================================
# 13.  opt.outputMvo
# ===========================================================================
test_that("opt.outputMvo example runs without error", {
  args(opt.outputMvo) # as shown in the Rd file
  expect_true(!is.null(formals(opt.outputMvo))) # function has formal args
})


# ===========================================================================
# 14.  optimize.portfolio.rebalancing  (originally wrapped in \dontrun{})
# ===========================================================================
test_that("optimize.portfolio.rebalancing example runs without error", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  utils::data(edhec)
  R <- edhec[, 1:4]
  funds <- colnames(R)

  portf <- portfolio.spec(funds)
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "long_only")
  portf <- add.objective(portf, type = "risk", name = "StdDev")

  # Quarterly rebalancing with 5-year training period
  bt.opt1 <- optimize.portfolio.rebalancing(R, portf,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 60
  )

  # Monthly rebalancing with 5-year training period and 4-year rolling window
  bt.opt2 <- optimize.portfolio.rebalancing(R, portf,
    optimize_method = "ROI",
    rebalance_on    = "months",
    training_period = 60,
    rolling_window  = 48
  )

  expect_s3_class(bt.opt1, "optimize.portfolio.rebalancing")
  expect_s3_class(bt.opt2, "optimize.portfolio.rebalancing")
})


# ===========================================================================
# 15.  portfolio.spec
# ===========================================================================
test_that("portfolio.spec example runs without error", {
  utils::data(edhec)
  pspec <- portfolio.spec(assets = colnames(edhec))
  pspec <- portfolio.spec(assets = 10, weight_seq = generatesequence())
  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 16.  position_limit_constraint
# ===========================================================================
test_that("position_limit_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))
  pspec <- add.constraint(
    portfolio = pspec, type = "position_limit",
    max_pos = 3
  )
  pspec <- add.constraint(
    portfolio = pspec, type = "position_limit",
    max_pos_long = 3, max_pos_short = 1
  )
  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 17.  random_portfolios_v1  (legacy v1 API)
# ===========================================================================
test_that("random_portfolios_v1 example runs without error", {
  rpconstraint <- constraint_v1(
    assets = 10,
    min_mult = -Inf,
    max_mult = Inf,
    min_sum = .99,
    max_sum = 1.01,
    min = .01,
    max = .4,
    weight_seq = generatesequence()
  )

  rp <- random_portfolios_v1(
    rpconstraints = rpconstraint,
    permutations = 1000
  )
  head(rp)
  expect_true(is.matrix(rp))
})


# ===========================================================================
# 18.  return_constraint
# ===========================================================================
test_that("return_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))
  pspec <- add.constraint(
    portfolio = pspec, type = "return",
    return_target = mean(colMeans(ret))
  )
  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 19.  transaction_cost_constraint
# ===========================================================================
test_that("transaction_cost_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))
  pspec <- add.constraint(
    portfolio = pspec, type = "transaction_cost",
    ptc = 0.01
  )
  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 20.  turnover_constraint
# ===========================================================================
test_that("turnover_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))
  pspec <- add.constraint(
    portfolio = pspec, type = "turnover",
    turnover_target = 0.6
  )
  expect_s3_class(pspec, "portfolio")
})


# ===========================================================================
# 21.  weight_sum_constraint
# ===========================================================================
test_that("weight_sum_constraint example runs without error", {
  utils::data(edhec)
  ret <- edhec[, 1:4]
  pspec <- portfolio.spec(assets = colnames(ret))

  # type="weight_sum" or type="leverage" both accept min_sum/max_sum
  pspec <- add.constraint(pspec,
    type = "weight_sum",
    min_sum = 1, max_sum = 1
  )
  # Convenience aliases
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "dollar_neutral")
  pspec <- add.constraint(pspec, type = "active")

  expect_s3_class(pspec, "portfolio")
})
