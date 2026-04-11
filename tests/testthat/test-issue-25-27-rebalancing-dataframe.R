###############################################################################
# tests/testthat/test-issue-25-27-rebalancing-dataframe.R
#
# Regression tests for:
#   Issue #25 — optimize.portfolio.rebalancing() crashes when R is a data.frame
#   Issue #27 — optimize.portfolio.rebalancing_v1() ignores rolling_window when
#               training_period is NULL (defaults to 36 instead)
#
# Both bugs are in R/optimize.portfolio.R.
#   #25 fix: add checkData(R) near the top of both rebalancing functions so
#            that xts-specific operations (endpoints, index, "[" row-selection)
#            work correctly regardless of input class.
#   #27 fix: add `if (is.null(training_period) & !is.null(rolling_window))`
#            block to _v1 function (mirroring the fix already present in v2).
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

library(ROI)
library(ROI.plugin.quadprog)

# ---------------------------------------------------------------------------
# Shared data — xts and data.frame versions of the same return series
# ---------------------------------------------------------------------------

utils::data(edhec)
R_xts <- edhec[, 1:5]
R_df  <- as.data.frame(R_xts)        # strip xts; becomes plain data.frame
nms   <- colnames(R_xts)

# Simple long-only, min-StdDev portfolio
portf_v2 <- portfolio.spec(assets = nms)
portf_v2 <- add.constraint(portf_v2, type = "full_investment")
portf_v2 <- add.constraint(portf_v2, type = "long_only")
portf_v2 <- add.objective(portf_v2, type = "risk", name = "StdDev")

# v1 constraint object (still accepted by optimize.portfolio.rebalancing_v1)
# constraint_v1() is the old v1 constructor — returns a list of class c("v1_constraint","constraint")
v1_constraints <- constraint_v1(
  assets  = nms,
  min     = rep(0, 5),
  max     = rep(1, 5),
  min_sum = 1,
  max_sum = 1
)

# ---------------------------------------------------------------------------
# Issue #25 — v2: data.frame input must not crash
# ---------------------------------------------------------------------------

test_that("#25 v2: optimize.portfolio.rebalancing does not crash with data.frame R", {
  # Before the fix this threw:
  #   Error in index(R[ep.i]) : could not find function "index"
  # (because R[ep.i] selected columns instead of rows for a data.frame)
  expect_no_error(
    optimize.portfolio.rebalancing(
      R               = R_df,
      portfolio       = portf_v2,
      optimize_method = "ROI",
      rebalance_on    = "quarters",
      training_period = 36,
      rolling_window  = 36
    )
  )
})

test_that("#25 v2: result with data.frame R has same class as result with xts R", {
  res_xts <- optimize.portfolio.rebalancing(
    R               = R_xts,
    portfolio       = portf_v2,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 36,
    rolling_window  = 36
  )
  res_df <- optimize.portfolio.rebalancing(
    R               = R_df,
    portfolio       = portf_v2,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 36,
    rolling_window  = 36
  )
  expect_s3_class(res_df, "optimize.portfolio.rebalancing")
  expect_equal(length(res_df$opt_rebalancing), length(res_xts$opt_rebalancing))
})

test_that("#25 v2: extractWeights works on result produced from data.frame R", {
  res_df <- optimize.portfolio.rebalancing(
    R               = R_df,
    portfolio       = portf_v2,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 36,
    rolling_window  = 36
  )
  w <- extractWeights(res_df)
  expect_true(is.matrix(w) || xts::is.xts(w))
  expect_equal(ncol(w), length(nms))
  expect_true(all(abs(rowSums(w) - 1) < 1e-4))
})

# ---------------------------------------------------------------------------
# Issue #25 — v1: data.frame input must not crash
# ---------------------------------------------------------------------------

test_that("#25 v1: optimize.portfolio.rebalancing_v1 does not crash with data.frame R", {
  expect_no_error(
    optimize.portfolio.rebalancing_v1(
      R               = R_df,
      constraints     = v1_constraints,
      optimize_method = "ROI",
      rebalance_on    = "quarters",
      training_period = 36
    )
  )
})

test_that("#25 v1: result with data.frame R has correct S3 class", {
  res <- optimize.portfolio.rebalancing_v1(
    R               = R_df,
    constraints     = v1_constraints,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 36
  )
  expect_s3_class(res, "optimize.portfolio.rebalancing")
  # v1 returns a flat named list (one element per rebalancing date); check it
  # has at least one period
  expect_true(length(res) > 0)
})

# ---------------------------------------------------------------------------
# Issue #27 — v1: training_period should default to rolling_window when
#             training_period is NULL and rolling_window is provided
# ---------------------------------------------------------------------------

test_that("#27 v1: training_period defaults to rolling_window when NULL", {
  # Run with rolling_window = 24 and training_period = NULL.
  # After the fix, training_period becomes 24, so the first rebalancing
  # date is the first endpoint >= row 24 (quarterly endpoint ~= row 24).
  # Before the fix, training_period defaulted to 36, so the first date
  # would be at endpoint >= 36, giving a different (smaller) number of
  # rebalancing periods.
  res_fixed <- optimize.portfolio.rebalancing_v1(
    R               = R_xts,
    constraints     = v1_constraints,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    rolling_window  = 24          # training_period intentionally omitted (NULL)
  )

  # Same call but with training_period explicitly equal to rolling_window.
  res_explicit <- optimize.portfolio.rebalancing_v1(
    R               = R_xts,
    constraints     = v1_constraints,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 24,
    rolling_window  = 24
  )

  # v1 returns a flat named list (one element per rebalancing date).
  # The two should produce the same number of rebalancing periods.
  expect_equal(
    length(res_fixed),
    length(res_explicit),
    label = "v1 training_period=NULL with rolling_window=24 yields same period count as training_period=24"
  )
})

test_that("#27 v1: with rolling_window=24 and NULL training_period, more periods than with default 36", {
  # With rolling_window = 24 and training_period = NULL (should become 24),
  # there should be MORE rebalancing periods than when training_period = 36.
  res_24 <- optimize.portfolio.rebalancing_v1(
    R               = R_xts,
    constraints     = v1_constraints,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    rolling_window  = 24          # training_period intentionally NULL
  )

  res_36 <- optimize.portfolio.rebalancing_v1(
    R               = R_xts,
    constraints     = v1_constraints,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 36,
    rolling_window  = 24
  )

  # v1 returns a flat named list (one element per rebalancing date).
  expect_gt(
    length(res_24),
    length(res_36),
    label = "training_period=NULL(->24) yields more periods than training_period=36"
  )
})

# ---------------------------------------------------------------------------
# Issue #27 — v2 (existing behavior must be preserved)
# ---------------------------------------------------------------------------

test_that("#27 v2: training_period already defaults to rolling_window correctly", {
  res_null <- optimize.portfolio.rebalancing(
    R               = R_xts,
    portfolio       = portf_v2,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    rolling_window  = 24          # training_period intentionally NULL
  )

  res_explicit <- optimize.portfolio.rebalancing(
    R               = R_xts,
    portfolio       = portf_v2,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 24,
    rolling_window  = 24
  )

  expect_equal(
    length(res_null$opt_rebalancing),
    length(res_explicit$opt_rebalancing),
    label = "v2 training_period=NULL with rolling_window=24 yields same periods as training_period=24"
  )
})

# ---------------------------------------------------------------------------
# Expanded coverage
# ---------------------------------------------------------------------------

test_that("#25 v2: matrix input coerced to xts and rebalancing works", {
  # Plain numeric matrix (row-named with dates) should be accepted.
  R_mat <- coredata(R_xts)
  rownames(R_mat) <- format(index(R_xts))

  expect_no_error(
    res <- optimize.portfolio.rebalancing(
      R               = R_mat,
      portfolio       = portf_v2,
      optimize_method = "ROI",
      rebalance_on    = "quarters",
      training_period = 36,
      rolling_window  = 36
    )
  )
  expect_s3_class(res, "optimize.portfolio.rebalancing")
  expect_gte(length(res$opt_rebalancing), 1L)
})

test_that("#25 v2: extractWeights on xts result gives sums ≈ 1", {
  res <- optimize.portfolio.rebalancing(
    R               = R_xts,
    portfolio       = portf_v2,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 36,
    rolling_window  = 36
  )
  w <- extractWeights(res)
  expect_true(all(abs(rowSums(w) - 1) < 1e-4))
  expect_equal(ncol(w), length(nms))
})

test_that("#27 v1: when rolling_window and training_period both given, training_period wins", {
  # training_period=48 > rolling_window=24 → should give fewer periods than when
  # training_period=24
  res_tp48 <- optimize.portfolio.rebalancing_v1(
    R               = R_xts,
    constraints     = v1_constraints,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 48,
    rolling_window  = 24
  )
  res_tp24 <- optimize.portfolio.rebalancing_v1(
    R               = R_xts,
    constraints     = v1_constraints,
    optimize_method = "ROI",
    rebalance_on    = "quarters",
    training_period = 24,
    rolling_window  = 24
  )
  expect_lt(length(res_tp48), length(res_tp24),
            label = "larger training_period yields fewer rebalancing periods")
})
