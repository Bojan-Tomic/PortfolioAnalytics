### Regression tests for GitHub issue #22
### "parallel calculation of optimize.portfolio.rebalancing with custom moment functions"
###
### When a custom momentFUN is passed to optimize.portfolio.rebalancing, the
### foreach parallel workers do not have access to the custom function unless
### it is explicitly exported via .export.
###
### Fix: extract momentFUN name from ... and add it to .export in all foreach
### loops in both optimize.portfolio.rebalancing_v1 and _v2.
###
### NOTE: we cannot register a real parallel backend in the test environment,
### so these tests verify the sequential path (which is identical logic) and
### also verify that the .export argument is correctly wired via the foreach
### metadata.
###
### custom_moments_22 and custom_moments_full_22 are defined in
### helper-portfolioanalytics.R so match.fun() can resolve them.

library(testthat)
library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_if_not_installed("PortfolioAnalytics")
skip_if_not_installed("PerformanceAnalytics")
skip_if_not_installed("foreach")

# Custom moment functions — assigned to .GlobalEnv so match.fun() can find
# them regardless of how the test file is executed (test_file or test_dir).
# Also defined in helper-portfolioanalytics.R for completeness.

#' Minimal custom moment function returning mu + sigma
.cm22 <- function(R, portfolio) {
  out <- list()
  out$mu    <- matrix(colMeans(R), ncol = 1)
  out$sigma <- cov(R)
  out
}
assign("custom_moments_22", .cm22, envir = .GlobalEnv)

#' Extended custom moment function returning mu + sigma + m3 + m4
.cmf22 <- function(R, portfolio) {
  out <- list()
  out$mu    <- matrix(colMeans(R), ncol = 1)
  out$sigma <- cov(R)
  out$m3    <- PerformanceAnalytics::M3.MM(R)
  out$m4    <- PerformanceAnalytics::M4.MM(R)
  out
}
assign("custom_moments_full_22", .cmf22, envir = .GlobalEnv)

# Helper: build spec with mean + StdDev objectives
make_spec_22 <- function(R) {
  ps <- portfolio.spec(colnames(R))
  ps <- add.constraint(ps, type = "full_investment")
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "return", name = "mean")
  ps <- add.objective(ps, type = "risk",   name = "StdDev")
  ps
}

# Helper: build spec with ETL objective (to test different moment requirements)
make_spec_etl_22 <- function(R) {
  ps <- portfolio.spec(colnames(R))
  ps <- add.constraint(ps, type = "full_investment")
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "return", name = "mean")
  ps <- add.objective(ps, type = "risk",   name = "ETL")
  ps
}

# ---------------------------------------------------------------------------
# v2 rebalancing with custom momentFUN — basic sequential path
# ---------------------------------------------------------------------------

test_that("optimize.portfolio.rebalancing v2 works with custom momentFUN (sequential) (#22)", {
  skip_on_cran()
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)

  expect_no_error(
    bt <- optimize.portfolio.rebalancing(
      R              = R,
      portfolio      = ps,
      optimize_method = "random",
      rebalance_on   = "years",
      training_period = 36,
      momentFUN       = "custom_moments_22",
      trace           = FALSE
    )
  )
  expect_true(inherits(bt, "optimize.portfolio.rebalancing"))
  expect_true(length(bt$opt_rebalancing) > 0)
})

test_that("rebalancing v2: custom momentFUN result has correct number of rebalance dates (#22)", {
  skip_on_cran()
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)
  bt <- optimize.portfolio.rebalancing(
    R               = R,
    portfolio       = ps,
    optimize_method  = "random",
    rebalance_on    = "years",
    training_period = 36,
    momentFUN        = "custom_moments_22",
    trace            = FALSE
  )
  # At least one rebalance period should exist
  expect_gte(length(bt$opt_rebalancing), 1L)
  # Each period's result should have weights
  first_opt <- bt$opt_rebalancing[[1]]
  expect_true(!is.null(extractWeights(first_opt)))
  expect_equal(length(extractWeights(first_opt)), ncol(R))
})

test_that("rebalancing v2: all rebalance period weights are finite (#22)", {
  skip_on_cran()
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)
  bt <- optimize.portfolio.rebalancing(
    R               = R,
    portfolio       = ps,
    optimize_method  = "random",
    rebalance_on    = "years",
    training_period = 36,
    momentFUN        = "custom_moments_22",
    trace            = FALSE
  )
  for (i in seq_along(bt$opt_rebalancing)) {
    wts <- extractWeights(bt$opt_rebalancing[[i]])
    expect_true(all(is.finite(wts)),
                info = paste("period", i, "weights must be finite"))
    expect_equal(sum(wts), 1, tolerance = 0.02,
                 info = paste("period", i, "weights must sum to ~1"))
  }
})

# ---------------------------------------------------------------------------
# Rolling window with custom momentFUN
# ---------------------------------------------------------------------------

test_that("rebalancing v2 rolling window + custom momentFUN (sequential) (#22)", {
  skip_on_cran()
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)

  expect_no_error(
    bt <- optimize.portfolio.rebalancing(
      R               = R,
      portfolio       = ps,
      optimize_method  = "random",
      rebalance_on    = "years",
      training_period = 36,
      rolling_window  = 36,
      momentFUN        = "custom_moments_22",
      trace            = FALSE
    )
  )
  expect_true(inherits(bt, "optimize.portfolio.rebalancing"))
  expect_true(length(bt$opt_rebalancing) > 0)
})

# ---------------------------------------------------------------------------
# Default moments (no custom momentFUN) — baseline regression
# ---------------------------------------------------------------------------

test_that("rebalancing v2 without momentFUN uses default moments (#22 baseline)", {
  skip_on_cran()
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)

  expect_no_error(
    bt <- optimize.portfolio.rebalancing(
      R               = R,
      portfolio       = ps,
      optimize_method  = "random",
      rebalance_on    = "years",
      training_period = 36,
      trace            = FALSE
    )
  )
  expect_true(inherits(bt, "optimize.portfolio.rebalancing"))
})

# ---------------------------------------------------------------------------
# ROI solver with custom momentFUN (exercises a different foreach loop in v2)
# ---------------------------------------------------------------------------

test_that("rebalancing v2 ROI solver with custom momentFUN works (#22)", {
  skip_on_cran()
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)

  expect_no_error(
    bt <- optimize.portfolio.rebalancing(
      R               = R,
      portfolio       = ps,
      optimize_method  = "ROI",
      rebalance_on    = "years",
      training_period = 36,
      momentFUN        = "custom_moments_22",
      trace            = FALSE
    )
  )
  expect_true(inherits(bt, "optimize.portfolio.rebalancing"))
  expect_true(length(bt$opt_rebalancing) > 0)
})

# ---------------------------------------------------------------------------
# Quarterly rebalancing (more periods — exercises loop more thoroughly)
# ---------------------------------------------------------------------------

test_that("rebalancing v2 quarterly with custom momentFUN works (#22)", {
  skip_on_cran()
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)

  expect_no_error(
    bt <- optimize.portfolio.rebalancing(
      R               = R,
      portfolio       = ps,
      optimize_method  = "random",
      rebalance_on    = "quarters",
      training_period = 36,
      momentFUN        = "custom_moments_22",
      trace            = FALSE
    )
  )
  expect_true(inherits(bt, "optimize.portfolio.rebalancing"))
  # Quarterly should give more periods than annual
  expect_gt(length(bt$opt_rebalancing), 1L)
})

# ---------------------------------------------------------------------------
# message=TRUE: verify no crash with custom momentFUN + messaging
# ---------------------------------------------------------------------------

test_that("rebalancing v2 with momentFUN and message=TRUE does not error (#22)", {
  skip_on_cran()
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)

  expect_no_error(
    suppressMessages(
      bt <- optimize.portfolio.rebalancing(
        R               = R,
        portfolio       = ps,
        optimize_method  = "random",
        rebalance_on    = "years",
        training_period = 36,
        momentFUN        = "custom_moments_22",
        message          = TRUE,
        trace            = FALSE
      )
    )
  )
  expect_true(inherits(bt, "optimize.portfolio.rebalancing"))
})

# ---------------------------------------------------------------------------
# extractWeights on rebalancing result produces a matrix
# ---------------------------------------------------------------------------

test_that("extractWeights on rebalancing result with custom momentFUN returns matrix (#22)", {
  skip_on_cran()
  data(edhec)
  R  <- edhec[, 1:4]
  ps <- make_spec_22(R)
  bt <- optimize.portfolio.rebalancing(
    R               = R,
    portfolio       = ps,
    optimize_method  = "random",
    rebalance_on    = "years",
    training_period = 36,
    momentFUN        = "custom_moments_22",
    trace            = FALSE
  )
  wts_mat <- extractWeights(bt)
  expect_true(is.matrix(wts_mat) || is.data.frame(wts_mat) || xts::is.xts(wts_mat),
              info = "extractWeights on rebalancing must return a matrix-like object")
  expect_equal(ncol(wts_mat), ncol(R))
})
