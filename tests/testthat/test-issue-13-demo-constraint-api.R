###############################################################################
# tests/testthat/test-issue-13-demo-constraint-api.R
#
# Tests for issue #13: demo scripts use old constraint() API.
#
# The old constraint() function (pre-0.8.3) has been aliased to
# constraint_v2() which now requires a 'type' argument.  Demo files that
# were written against v1 must call constraint_v1() instead.
#
# This test file verifies:
#   1. constraint_v1() is exported and callable without a 'type' argument.
#   2. constraint() without 'type' errors (documents the breakage that was
#      present before the fix).
#   3. The demo-style constraint_v1() calls produce valid portfolio objects
#      accepted by optimize.portfolio.
#   4. demo_risk_budgets: rbStdDev.portf is the correct object name (not
#      the now-fixed typo SDRB.portf).
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Shared data
# ---------------------------------------------------------------------------
data(edhec)
R4  <- edhec[, 1:4]
funds4 <- colnames(R4)

# ---------------------------------------------------------------------------
# 1. constraint_v1() is callable without 'type'
# ---------------------------------------------------------------------------

test_that("constraint_v1() creates a v1 constraint without requiring 'type'", {
  skip_if_not_installed("PortfolioAnalytics")
  pobj <- constraint_v1(assets = funds4, min = 0, max = 1,
                        min_sum = 0.99, max_sum = 1.01,
                        risk_aversion = 1)
  expect_s3_class(pobj, "constraint")
  expect_equal(unname(pobj$assets), rep(1 / 4, 4))
  expect_equal(unname(pobj$min), rep(0, 4))
  expect_equal(unname(pobj$max), rep(1, 4))
})

# ---------------------------------------------------------------------------
# 2. constraint() without 'type' now errors (regression guard for the fix)
# ---------------------------------------------------------------------------

test_that("constraint() without 'type' argument errors in the current API", {
  skip_if_not_installed("PortfolioAnalytics")
  expect_error(
    constraint(assets = funds4, min = 0, max = 1,
               min_sum = 0.99, max_sum = 1.01),
    regexp = "type"
  )
})

# ---------------------------------------------------------------------------
# 3a. testing_ROI demo style: constraint_v1() + add.objective + optimize
#     (minimal subset — just max-return under box constraints)
# ---------------------------------------------------------------------------

test_that("testing_ROI demo: constraint_v1 + add.objective + ROI optimize works", {
  skip_if_not_installed("PortfolioAnalytics")
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  N <- length(funds4)
  mu.port <- mean(colMeans(R4))

  gen.constr <- constraint_v1(assets = funds4, min = -Inf, max = Inf,
                               min_sum = 1, max_sum = 1, risk_aversion = 1)
  gen.constr <- add.objective(constraints = gen.constr, type = "return",
                               name = "mean", enabled = FALSE, multiplier = 0,
                               target = mu.port)
  gen.constr <- add.objective(constraints = gen.constr, type = "risk",
                               name = "var", enabled = FALSE, multiplier = 0,
                               risk_aversion = 10)

  # Max return under box constraints, fully invested
  max.port <- gen.constr
  max.port$min <- rep(0.01, N)
  max.port$max <- rep(0.30, N)
  max.port$objectives[[1]]$enabled <- TRUE
  max.port$objectives[[1]]$target  <- NA

  result <- optimize.portfolio(R = R4, constraints = max.port,
                               optimize_method = "ROI")
  expect_s3_class(result, "optimize.portfolio.ROI")
  w <- result$weights
  expect_length(w, N)
  expect_true(all(w >= 0.009))   # box min ≥ 0.01 (numerical slack)
  expect_true(all(w <= 0.301))
  expect_equal(sum(w), 1, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 3b. testing_ROI demo style: GMV (var objective)
# ---------------------------------------------------------------------------

test_that("testing_ROI demo: constraint_v1 + var objective + ROI GMV works", {
  skip_if_not_installed("PortfolioAnalytics")
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.quadprog")

  N <- length(funds4)

  gen.constr <- constraint_v1(assets = funds4, min = -Inf, max = Inf,
                               min_sum = 1, max_sum = 1, risk_aversion = 1)
  gen.constr <- add.objective(constraints = gen.constr, type = "return",
                               name = "mean", enabled = FALSE, multiplier = 0,
                               target = 0)
  gen.constr <- add.objective(constraints = gen.constr, type = "risk",
                               name = "var", enabled = FALSE, multiplier = 0,
                               risk_aversion = 1)

  gmv.port <- gen.constr
  gmv.port$objectives[[2]]$enabled       <- TRUE
  gmv.port$objectives[[2]]$risk_aversion <- 1

  result <- optimize.portfolio(R = R4, constraints = gmv.port,
                               optimize_method = "ROI")
  expect_s3_class(result, "optimize.portfolio.ROI")
  w <- result$weights
  expect_length(w, N)
  expect_equal(sum(w), 1, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 3c. testing_pso demo style: constraint_v1 with min/max bounds
# ---------------------------------------------------------------------------

test_that("testing_pso demo: constraint_v1 with numeric min/max is valid", {
  skip_if_not_installed("PortfolioAnalytics")

  pobj <- constraint_v1(assets = funds4, min = -2, max = 2,
                        min_sum = 0.99, max_sum = 1.01,
                        risk_aversion = 1)
  expect_s3_class(pobj, "constraint")
  expect_equal(unname(pobj$min), rep(-2, 4))
  expect_equal(unname(pobj$max), rep( 2, 4))
})

# ---------------------------------------------------------------------------
# 3d. sortino demo style: constraint_v1 with weight_seq
# ---------------------------------------------------------------------------

test_that("sortino demo: constraint_v1 with weight_seq is valid", {
  skip_if_not_installed("PortfolioAnalytics")

  data(indexes)
  idx4 <- colnames(indexes[, 1:4])
  pobj <- constraint_v1(assets = idx4, min = 0.05, max = 1,
                        min_sum = 0.99, max_sum = 1.01,
                        weight_seq = generatesequence(by = 0.05))
  expect_s3_class(pobj, "constraint")
  expect_true(!is.null(pobj$weight_seq))
})

# ---------------------------------------------------------------------------
# 4. demo_risk_budgets: rbStdDev.portf is defined after the StdDev risk
#    budget objective is added — verify object exists with correct class
# ---------------------------------------------------------------------------

test_that("demo_risk_budgets: rbStdDev.portf (not SDRB.portf) is the correct object", {
  skip_if_not_installed("PortfolioAnalytics")

  R8 <- edhec[, 1:8]

  init.portf <- portfolio.spec(assets = colnames(R8))
  init.portf <- add.constraint(portfolio = init.portf, type = "leverage",
                               min_sum = 0.99, max_sum = 1.01)
  init.portf <- add.constraint(portfolio = init.portf, type = "long_only")

  rbStdDev.portf <- add.objective(portfolio = init.portf, type = "return",
                                  name = "mean")
  rbStdDev.portf <- add.objective(portfolio = rbStdDev.portf,
                                  type = "risk_budget",
                                  name = "StdDev", max_prisk = 0.25)

  expect_s3_class(rbStdDev.portf, "portfolio")
  # Two objectives: mean return + StdDev risk budget
  expect_length(rbStdDev.portf$objectives, 2)
  expect_equal(rbStdDev.portf$objectives[[2]]$name, "StdDev")
})

# ---------------------------------------------------------------------------
# 5. Verify all six demo files parse without error
#    (parse() not source() — avoids actually running heavy optimizations)
# ---------------------------------------------------------------------------

test_that("demo/testing_ROI.R parses without error", {
  demo_path <- system.file("demo", "testing_ROI.R",
                            package = "PortfolioAnalytics")
  skip_if(nchar(demo_path) == 0, "demo not found in installed package")
  expect_silent(parse(demo_path))
})

test_that("demo/testing_pso.R parses without error", {
  demo_path <- system.file("demo", "testing_pso.R",
                            package = "PortfolioAnalytics")
  skip_if(nchar(demo_path) == 0, "demo not found in installed package")
  expect_silent(parse(demo_path))
})

test_that("demo/sortino.R parses without error", {
  demo_path <- system.file("demo", "sortino.R",
                            package = "PortfolioAnalytics")
  skip_if(nchar(demo_path) == 0, "demo not found in installed package")
  expect_silent(parse(demo_path))
})

test_that("demo/backwards_compat.R parses without error", {
  demo_path <- system.file("demo", "backwards_compat.R",
                            package = "PortfolioAnalytics")
  skip_if(nchar(demo_path) == 0, "demo not found in installed package")
  expect_silent(parse(demo_path))
})

test_that("demo/constrained_optim.R parses without error", {
  demo_path <- system.file("demo", "constrained_optim.R",
                            package = "PortfolioAnalytics")
  skip_if(nchar(demo_path) == 0, "demo not found in installed package")
  expect_silent(parse(demo_path))
})

test_that("demo/demo_risk_budgets.R parses without error", {
  demo_path <- system.file("demo", "demo_risk_budgets.R",
                            package = "PortfolioAnalytics")
  skip_if(nchar(demo_path) == 0, "demo not found in installed package")
  expect_silent(parse(demo_path))
})
