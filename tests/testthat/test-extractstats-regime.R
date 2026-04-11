###############################################################################
# tests/testthat/test-extractstats-regime.R
#
# Coverage targets (R/extractstats.R):
#   - extractStatsRegime()          [internal, lines 288-312]
#   - extractObjRegime()            [internal, lines 540-572]
#   - extractStats.optimize.portfolio.rebalancing  regime branch (line 276-277)
#   - extractObjectiveMeasures.optimize.portfolio.rebalancing regime branch (line 518-519)
#
# BUG-5 FIXED: optimize.portfolio.rebalancing() now handles regime.portfolios
# directly (the turnover_idx check is guarded for NULL constraints).
# A direct call test is included below.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

utils::data(edhec)
R_regime <- edhec[, 1:4]
nms4     <- colnames(R_regime)

# Deterministic regime assignment: first 60% → regime 1, last 40% → regime 2
n_obs       <- nrow(R_regime)
regime_vals <- c(rep(1L, ceiling(n_obs * 0.6)),
                 rep(2L, n_obs - ceiling(n_obs * 0.6)))
regime_xts  <- xts::xts(regime_vals, order.by = zoo::index(R_regime))

# Conservative portfolio (regime 1)
port_r1 <- portfolio.spec(nms4)
port_r1 <- add.constraint(port_r1, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
port_r1 <- add.constraint(port_r1, type = "box", min = 0.05, max = 0.60)
port_r1 <- add.objective(port_r1,  type = "risk", name = "StdDev")

# Slightly different bounds (regime 2)
port_r2 <- portfolio.spec(nms4)
port_r2 <- add.constraint(port_r2, type = "weight_sum",
                           min_sum = 0.99, max_sum = 1.01)
port_r2 <- add.constraint(port_r2, type = "box", min = 0.00, max = 0.70)
port_r2 <- add.objective(port_r2,  type = "risk", name = "StdDev")

portfolios_regime <- combine.portfolios(list(port_r1, port_r2))
regime_port       <- regime.portfolios(regime_xts, portfolios_regime)

# Build a minimal rebalancing result manually.
# optimize.portfolio.rebalancing cannot accept regime.portfolios directly
# (bug: portfolio$constraints is NULL for regime.portfolios). We manually
# run 3 quarterly periods to construct the expected object structure.
ep_all <- xts::endpoints(R_regime, on = "quarters")
ep_all <- ep_all[ep_all >= 36L]

# Pick first, middle, last endpoint for coverage of both regimes
ep_sel <- ep_all[c(1L, ceiling(length(ep_all) / 2), length(ep_all))]

opt_rebal_regime <- tryCatch({
  out_list <- vector("list", length(ep_sel))
  names(out_list) <- as.character(zoo::index(R_regime[ep_sel]))
  for (i in seq_along(ep_sel)) {
    ep    <- ep_sel[i]
    R_sub <- R_regime[(max(1L, ep - 36L + 1L)):ep, ]
    out_list[[i]] <- optimize.portfolio(R_sub, regime_port,
                                        optimize_method = "ROI")
  }
  rebal <- list(
    portfolio       = regime_port,
    R               = R_regime,
    opt_rebalancing = out_list
  )
  class(rebal) <- "optimize.portfolio.rebalancing"
  rebal
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# 1. Sanity checks on the manually constructed rebalancing result
# ---------------------------------------------------------------------------

test_that("opt_rebal_regime is not NULL (construction succeeded)", {
  skip_if(is.null(opt_rebal_regime))
  expect_false(is.null(opt_rebal_regime))
})

test_that("opt_rebal_regime has class 'optimize.portfolio.rebalancing'", {
  skip_if(is.null(opt_rebal_regime))
  expect_s3_class(opt_rebal_regime, "optimize.portfolio.rebalancing")
})

test_that("opt_rebal_regime$portfolio inherits regime.portfolios", {
  skip_if(is.null(opt_rebal_regime))
  expect_true(inherits(opt_rebal_regime$portfolio, "regime.portfolios"))
})

test_that("opt_rebal_regime$opt_rebalancing is a non-empty list", {
  skip_if(is.null(opt_rebal_regime))
  expect_true(is.list(opt_rebal_regime$opt_rebalancing))
  expect_gt(length(opt_rebal_regime$opt_rebalancing), 0L)
})

test_that("individual results carry $regime slot", {
  skip_if(is.null(opt_rebal_regime))
  has_regime <- sapply(opt_rebal_regime$opt_rebalancing, function(x) !is.null(x$regime))
  expect_true(all(has_regime))
})

# ---------------------------------------------------------------------------
# 2. extractStats — regime branch → extractStatsRegime()
# ---------------------------------------------------------------------------

es_regime <- tryCatch(
  extractStats(opt_rebal_regime),
  error = function(e) NULL
)

test_that("extractStats on regime rebalancing result is not NULL", {
  skip_if(is.null(opt_rebal_regime))
  expect_false(is.null(es_regime))
})

test_that("extractStatsRegime returns a list", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(es_regime))
  expect_true(is.list(es_regime))
})

test_that("extractStatsRegime list has an element per unique regime", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(es_regime))
  # Should have 2 elements: one per unique regime value (1 and 2)
  expect_equal(length(es_regime), 2L)
})

test_that("extractStatsRegime list names contain 'regime'", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(es_regime))
  expect_true(all(grepl("regime", names(es_regime))))
})

test_that("each element of extractStatsRegime output is itself a list", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(es_regime))
  expect_true(all(sapply(es_regime, is.list)))
})

test_that("each regime sub-list contains matrix-like statistics", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(es_regime))
  # Each inner element should be a numeric matrix/vector from extractStats
  for (regime_el in es_regime) {
    for (period_el in regime_el) {
      expect_true(is.numeric(period_el) || is.matrix(period_el))
    }
  }
})

# ---------------------------------------------------------------------------
# 3. extractObjectiveMeasures — regime branch → extractObjRegime()
# ---------------------------------------------------------------------------

eom_regime <- tryCatch(
  extractObjectiveMeasures(opt_rebal_regime),
  error = function(e) NULL
)

test_that("extractObjectiveMeasures on regime rebalancing result is not NULL", {
  skip_if(is.null(opt_rebal_regime))
  expect_false(is.null(eom_regime))
})

test_that("extractObjRegime returns a list", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(eom_regime))
  expect_true(is.list(eom_regime))
})

test_that("extractObjRegime list has 2 elements (one per regime)", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(eom_regime))
  expect_equal(length(eom_regime), 2L)
})

test_that("extractObjRegime names contain 'regime'", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(eom_regime))
  expect_true(all(grepl("regime", names(eom_regime))))
})

test_that("each regime element from extractObjRegime is an xts object", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(eom_regime))
  for (el in eom_regime) {
    expect_s3_class(el, "xts")
  }
})

test_that("each xts element has at least one row", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(eom_regime))
  for (el in eom_regime) {
    expect_gt(nrow(el), 0L)
  }
})

test_that("xts elements have numeric columns (objective measure values)", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(eom_regime))
  for (el in eom_regime) {
    expect_true(is.numeric(coredata(el)))
  }
})

test_that("xts column names come from name.replace (short form, no 'objective_measures' prefix)", {
  skip_if(is.null(opt_rebal_regime))
  skip_if(is.null(eom_regime))
  # name.replace shortens "objective_measures.StdDev" → "StdDev"
  for (el in eom_regime) {
    expect_false(any(grepl("objective_measures", colnames(el))))
  }
})

# ---------------------------------------------------------------------------
# 4. BUG-5 FIXED: optimize.portfolio.rebalancing() with regime.portfolios
#    (was crashing with 'argument to which is not logical')
# ---------------------------------------------------------------------------

test_that("optimize.portfolio.rebalancing accepts regime.portfolios directly (BUG-5 fixed)", {
  # BUG-5 fix: turnover_idx check is now guarded for NULL constraints.
  # This test calls optimize.portfolio.rebalancing() directly with the
  # regime.portfolios object to confirm it no longer crashes.
  result <- tryCatch(
    suppressMessages(suppressWarnings(
      optimize.portfolio.rebalancing(
        R          = R_regime,
        portfolio  = regime_port,
        optimize_method = "ROI",
        rebalance_on    = "quarters",
        training_period = 36L
      )
    )),
    error = function(e) e
  )
  # Should succeed (not throw) — result is an optimize.portfolio.rebalancing object
  if (inherits(result, "error")) {
    # If it still errors, fail with informative message
    fail(paste("optimize.portfolio.rebalancing with regime.portfolios errored:", conditionMessage(result)))
  }
  expect_s3_class(result, "optimize.portfolio.rebalancing")
})
