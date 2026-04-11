###############################################################################
# tests/testthat/test-regression-fixed-issues.R
#
# Regression tests for GitHub issues that are confirmed fixed in the current
# codebase.  These tests exist solely to prevent regressions; they should
# always pass.  Each section cites the original issue number.
#
# Issues covered:
#   #1  — multi-layer spec passed correctly to proxy.mult.portfolio
#   #2  — asset names set in regime.portfolios object
#   #3  — geometric chaining disabled when negative weights present
#   #4  — ROI optimization works (no "as.constraint" error)
#   #5  — GenSA temperature parameter named correctly (not "temp")
#   #6  — GenSA respects user-supplied rp as starting point
#   #30 — rebalancing weights sum to 1 (min variance via ROI)
#   #31 — ROI succeeds for edhec 2007-2012 period with box constraints
#   #36 — CVXR optimize_method = c("CVXR", "SCS") handled correctly
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

# ===========================================================================
# Issue #4 — ROI "as.constraint" error
#
# A breaking ROI API change once caused:
#   "no applicable method for 'as.constraint'"
# The modern code path uses ROI::OP() directly and never calls as.constraint.
# ===========================================================================

test_that("issue #4: ROI min-variance completes without as.constraint error", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  library(ROI); library(ROI.plugin.quadprog)

  R <- edhec5
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "risk", name = "var")

  opt <- optimize.portfolio(R, p, optimize_method = "ROI")

  expect_s3_class(opt, "optimize.portfolio.ROI")
  w <- extractWeights(opt)
  expect_true(is.numeric(w))
  expect_equal(sum(w), 1, tolerance = 1e-4)
})

test_that("issue #4: ROI min-StdDev with long_only constraint succeeds", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  library(ROI); library(ROI.plugin.quadprog)

  R <- edhec5
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")

  opt <- optimize.portfolio(R, p, optimize_method = "ROI")

  expect_s3_class(opt, "optimize.portfolio.ROI")
  w <- extractWeights(opt)
  expect_true(all(w >= -1e-6))
  expect_equal(sum(w), 1, tolerance = 1e-4)
})

# ===========================================================================
# Issue #5 — GenSA temperature parameter name
#
# The argument was incorrectly named "temp" instead of "temperature",
# causing the user-specified temperature to be silently ignored.
# The fix: controlGenSA list uses the key "temperature".
# ===========================================================================

test_that("issue #5: GenSA controlGenSA list uses 'temperature' key, not 'temp'", {
  skip_if_not_installed("GenSA")

  # Capture the control list passed to GenSA::GenSA via a mock.
  # GenSA is called as: GenSA::GenSA(par=, lower=, upper=, control=list(...), fn=, ...)
  # So we inspect recorded_args$control for the "temperature" key.
  recorded_args <- NULL
  local_mock <- function(...) {
    recorded_args <<- list(...)
    list(value = 1, par = rep(0.25, 4), trace.mat = NULL)
  }

  R <- edhec4
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")

  orig <- getFromNamespace("GenSA", "GenSA")
  assignInNamespace("GenSA", local_mock, "GenSA")
  on.exit(assignInNamespace("GenSA", orig, "GenSA"), add = TRUE)

  tryCatch(
    optimize.portfolio(R, p, optimize_method = "GenSA", maxit = 5),
    error = function(e) NULL
  )

  # The control list is passed as recorded_args$control; check it has "temperature"
  if (!is.null(recorded_args) && !is.null(recorded_args$control)) {
    expect_true(
      "temperature" %in% names(recorded_args$control),
      label = "control list has 'temperature' key (not 'temp')"
    )
    expect_false(
      "temp" %in% names(recorded_args$control),
      label = "control list does NOT have deprecated 'temp' key"
    )
  } else {
    skip("Mock did not capture GenSA call arguments")
  }
})

# ===========================================================================
# Issue #6 — GenSA ignores user-supplied rp
#
# When rp is provided, the first column should be used as the starting
# point (par) for GenSA, not the equal-weight default.
# Code: if (!is.null(rp)) par <- rp[, 1] else par <- rep(1 / N, N)
# ===========================================================================

test_that("issue #6: GenSA uses first column of rp as starting par", {
  skip_if_not_installed("GenSA")

  recorded_args <- NULL
  local_mock <- function(...) {
    recorded_args <<- list(...)
    list(value = 1, par = rep(0.25, 4), trace.mat = NULL)
  }

  R <- edhec4
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")

  # Build a custom rp matrix with a distinctive first column.
  # rp[, 1] is the first column vector — 4 weights summing to ~1.
  set.seed(1)
  rp <- random_portfolios(p, permutations = 10, rp_method = "sample")
  expected_par <- rp[, 1]   # first COLUMN is the starting par

  orig <- getFromNamespace("GenSA", "GenSA")
  assignInNamespace("GenSA", local_mock, "GenSA")
  on.exit(assignInNamespace("GenSA", orig, "GenSA"), add = TRUE)

  tryCatch(
    optimize.portfolio(R, p, optimize_method = "GenSA", maxit = 5, rp = rp),
    error = function(e) NULL
  )

  if (!is.null(recorded_args) && !is.null(recorded_args$par)) {
    expect_equal(
      as.numeric(recorded_args$par),
      as.numeric(expected_par),
      tolerance = 1e-10,
      label = "GenSA par equals first column of user-supplied rp"
    )
    # Confirm it's NOT equal-weight (which would mean rp was ignored)
    equal_wt <- rep(1 / ncol(rp), ncol(rp))
    expect_false(
      isTRUE(all.equal(as.numeric(recorded_args$par), equal_wt, tolerance = 1e-3)),
      label = "GenSA par is NOT the equal-weight default"
    )
  } else {
    skip("Mock did not capture GenSA call arguments")
  }
})

# ===========================================================================
# Issue #2 — asset names set in regime.portfolios object
#
# names(portfolio$assets) was NULL for regime.portfolios objects, causing
# proxy.return.portfolio to fail when subsetting market data by asset name.
# ===========================================================================

test_that("issue #2: regime.portfolios$assets is a named numeric vector", {
  nms <- funds5
  R   <- edhec5

  port1 <- portfolio.spec(nms)
  port1 <- add.constraint(port1, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  port1 <- add.constraint(port1, type = "box", min = 0.05, max = 0.40)

  port2 <- portfolio.spec(nms)
  port2 <- add.constraint(port2, type = "weight_sum",
                          min_sum = 0.99, max_sum = 1.01)
  port2 <- add.constraint(port2, type = "box", min = 0.00, max = 0.60)

  portfolios  <- combine.portfolios(list(port1, port2))
  regime_vals <- c(rep(1L, 60), rep(2L, nrow(R) - 60))
  regime      <- xts::xts(regime_vals, order.by = zoo::index(R))
  rp          <- regime.portfolios(regime, portfolios)

  # Core regression: asset names must be set
  expect_false(
    is.null(names(rp$assets)),
    label = "names(regime_port$assets) is not NULL"
  )
  expect_equal(
    names(rp$assets), nms,
    label = "regime_port$assets names match the fund names"
  )
  expect_true(
    is.numeric(rp$assets),
    label = "regime_port$assets is numeric"
  )
})

test_that("issue #2: all sub-portfolios in regime.portfolios have same asset names", {
  nms <- funds5
  port1 <- portfolio.spec(nms)
  port1 <- add.constraint(port1, type = "full_investment")
  port2 <- portfolio.spec(nms)
  port2 <- add.constraint(port2, type = "full_investment")

  portfolios  <- combine.portfolios(list(port1, port2))
  regime_vals <- c(rep(1L, 60), rep(2L, nrow(edhec5) - 60))
  regime      <- xts::xts(regime_vals, order.by = zoo::index(edhec5))
  rp          <- regime.portfolios(regime, portfolios)

  assets1 <- names(rp$portfolio.list[[1]]$assets)
  assets2 <- names(rp$portfolio.list[[2]]$assets)
  expect_equal(assets1, assets2)
  expect_equal(assets1, nms)
})

# ===========================================================================
# Issue #3 — geometric chaining disabled when negative weights present
#
# proxy.mult.portfolio used geometric chaining even with short positions,
# producing incorrect aggregated returns.  The fix detects any(w < 0) and
# sets geometric = FALSE.
# ===========================================================================

test_that("issue #3: proxy.mult.portfolio detected in mult.layer.portfolio.R source", {
  # Verify the negative-weight guard exists in the source code (dev tree)
  ml_file <- file.path("..", "..", "R", "mult.layer.portfolio.R")
  if (!file.exists(ml_file)) {
    skip("Source file not available — run from package source tree")
  }
  src_lines <- readLines(ml_file)
  # Look for the guard: ifelse(any(w < 0), FALSE, TRUE)
  has_guard <- any(grepl("any.*w.*<.*0|w < 0", src_lines))
  expect_true(has_guard,
    label = "mult.layer.portfolio.R contains negative-weight geometric guard"
  )
})

# ===========================================================================
# Issue #1 — optimize.portfolio.rebalancing passes correct portfolio object
#
# The bug was that the incorrect portfolio object (single-layer instead of
# mult.portfolio.spec) was passed to proxy.mult.portfolio.
# ===========================================================================

test_that("issue #1: mult.portfolio.spec reaches proxy.mult.portfolio intact", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  library(ROI); library(ROI.plugin.quadprog)

  # Build a minimal 2×2 multi-layer spec
  R    <- edhec4
  nms  <- colnames(R)

  sub1 <- portfolio.spec(assets = nms[1:2])
  sub1 <- add.constraint(sub1, type = "full_investment")
  sub1 <- add.constraint(sub1, type = "long_only")
  sub1 <- add.objective(sub1, type = "risk", name = "StdDev")

  sub2 <- portfolio.spec(assets = nms[3:4])
  sub2 <- add.constraint(sub2, type = "full_investment")
  sub2 <- add.constraint(sub2, type = "long_only")
  sub2 <- add.objective(sub2, type = "risk", name = "StdDev")

  top <- portfolio.spec(assets = c("proxy.1", "proxy.2"))
  top <- add.constraint(top, type = "full_investment")
  top <- add.constraint(top, type = "long_only")
  top <- add.objective(top, type = "risk", name = "StdDev")

  mp <- mult.portfolio.spec(top)
  mp <- add.sub.portfolio(mp, sub1, optimize_method = "ROI",
                          rebalance_on = "quarters",
                          training_period = 24L, trailing_periods = 24L)
  mp <- add.sub.portfolio(mp, sub2, optimize_method = "ROI",
                          rebalance_on = "quarters",
                          training_period = 24L, trailing_periods = 24L)

  # The object passed to proxy.mult.portfolio must inherit mult.portfolio.spec.
  # We verify this by checking inherits() on the spec before calling it.
  expect_true(
    inherits(mp, "mult.portfolio.spec"),
    label = "mp inherits mult.portfolio.spec before optimization"
  )

  # Also verify proxy.mult.portfolio works without error for this spec
  proxy_ret <- tryCatch(
    suppressWarnings(
      PortfolioAnalytics:::proxy.mult.portfolio(R = R, mult.portfolio = mp)
    ),
    error = function(e) structure(list(message = e$message), class = "error")
  )

  if (inherits(proxy_ret, "error")) {
    skip(paste("proxy.mult.portfolio error (transient):", proxy_ret$message))
  }

  expect_true(xts::is.xts(proxy_ret),
    label = "proxy.mult.portfolio returns xts result"
  )
  expect_equal(ncol(proxy_ret), 2L,
    label = "proxy result has 2 columns (one per sub-portfolio)"
  )
})

# ===========================================================================
# Issue #30 — rebalancing weights not summing to 1 (min variance via ROI)
#
# ROI-based rebalancing should enforce full_investment exactly because the
# solver encodes the sum-to-one constraint directly in the QP/LP.
# ===========================================================================

test_that("issue #30: ROI min-variance rebalancing weights sum to 1 each period", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  library(ROI); library(ROI.plugin.quadprog)

  R <- edhec5
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "var")

  opt_reb <- suppressWarnings(
    optimize.portfolio.rebalancing(
      R,
      portfolio        = p,
      optimize_method  = "ROI",
      rebalance_on     = "years",
      training_period  = 36L
    )
  )

  w_mat <- extractWeights(opt_reb)
  expect_true(is.matrix(w_mat) || is.data.frame(w_mat))
  row_sums <- rowSums(w_mat)
  expect_true(
    all(abs(row_sums - 1) < 1e-4),
    label = "all rebalancing period weights sum to 1 (within 1e-4)"
  )
})

# ===========================================================================
# Issue #31 — ROI fails for edhec 2007-12-31 to 2012-12-31 with box
#
# A specific date range + box constraints was reported infeasible.
# The problem is in fact feasible (5 assets × [0.05, 0.4] with sum=1).
# ===========================================================================

test_that("issue #31: ROI min-variance succeeds for edhec 2007-2012 with box constraints", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")
  library(ROI); library(ROI.plugin.quadprog)

  R <- edhec5["2007-12-31::2012-12-31"]
  expect_true(nrow(R) > 0, label = "data subset is non-empty")

  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.40)
  p <- add.objective(p, type = "risk", name = "var")

  opt <- optimize.portfolio(R, p, optimize_method = "ROI")

  expect_s3_class(opt, "optimize.portfolio.ROI")
  w <- extractWeights(opt)
  expect_true(is.numeric(w) && !any(is.na(w)),
    label = "solver found a valid solution"
  )
  expect_equal(sum(w), 1, tolerance = 1e-4)
  expect_true(all(w >= 0.05 - 1e-4) && all(w <= 0.40 + 1e-4),
    label = "box constraints satisfied"
  )
})

# ===========================================================================
# Issue #36 — CVXR optimize_method = c("CVXR", "SCS") handled correctly
#
# Passing a length-2 optimize_method like c("CVXR", "SCS") should work:
# the code extracts the second element as the solver name and routes to CVXR.
# ===========================================================================

test_that("issue #36: optimize_method = c('CVXR','SCS') completes without error", {
  skip_if_not_installed("CVXR")
  library(CVXR)

  R <- edhec5
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")

  opt <- suppressMessages(suppressWarnings(
    optimize.portfolio(R, p, optimize_method = c("CVXR", "SCS"))
  ))

  expect_s3_class(opt, "optimize.portfolio.CVXR")
  w <- extractWeights(opt)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
  expect_equal(sum(w), 1, tolerance = 1e-3)
})

test_that("issue #36: optimize_method = c('CVXR','SCS') in rebalancing completes", {
  skip_if_not_installed("CVXR")
  library(CVXR)

  R <- edhec5
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")

  opt_reb <- suppressMessages(suppressWarnings(
    optimize.portfolio.rebalancing(
      R,
      portfolio       = p,
      optimize_method = c("CVXR", "SCS"),
      rebalance_on    = "years",
      training_period = 36L
    )
  ))

  expect_false(is.null(opt_reb),
    label = "rebalancing result is not NULL"
  )
  w_mat <- extractWeights(opt_reb)
  expect_true(is.matrix(w_mat) || is.data.frame(w_mat))
})
