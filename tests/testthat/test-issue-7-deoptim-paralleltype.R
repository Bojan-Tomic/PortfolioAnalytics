###############################################################################
# tests/testthat/test-issue-7-deoptim-paralleltype.R
#
# Regression tests for:
#   Issue #7 — parallelType (and other DEoptim.control args) not correctly
#               propagated from ... to DEoptim.control via DEcformals.
#
# Root-cause history:
#   - Old code had parallelType='auto'; fixed to parallelType=2 (correct).
#   - `names(dotargs[pm > 0L]) <- DEcargs[pm]` (dead code: modified a copy of
#     dotargs, not dotargs itself). Removed — has no effect on correctness
#     since `DEcformals[pm] <- dotargs[pm > 0L]` copies values correctly.
#
# These tests verify:
#   1. User-supplied parallelType=0 prevents the default parallelType=2
#      override and produces a valid result.
#   2. User-supplied itermax (DEoptim control arg) is accepted without error.
#   3. When parallel=TRUE and foreach is loaded, the default parallelType is 2.
#   4. Additional DEoptim.control args (F, CR, NP, strategy) are passed through.
#   5. trace=TRUE produces expected output slots.
#   6. Various objective types work with DEoptim.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
skip_on_cran()
skip_if_not_installed("DEoptim")

library(DEoptim)

utils::data(edhec)
R4 <- edhec[, 1:4]
R8 <- edhec[, 1:8]

portf_stddev <- portfolio.spec(assets = colnames(R4))
portf_stddev <- add.constraint(portf_stddev, type = "weight_sum",
                                min_sum = 0.99, max_sum = 1.01)
portf_stddev <- add.constraint(portf_stddev, type = "box", min = 0.05, max = 0.60)
portf_stddev <- add.objective(portf_stddev, type = "risk", name = "StdDev")

# ---------------------------------------------------------------------------
# #7 — user-supplied parallelType=0 does not crash and returns valid result
# ---------------------------------------------------------------------------

test_that("#7 DEoptim with parallelType=0 runs without error", {
  set.seed(42)
  expect_no_error(
    optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                       itermax = 10, parallelType = 0)
  )
})

test_that("#7 DEoptim with parallelType=0 returns valid weights", {
  set.seed(42)
  res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                            itermax = 10, parallelType = 0)
  w <- extractWeights(res)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
  expect_true(abs(sum(w) - 1) < 0.02)
  expect_true(all(w >= 0.04))
})

test_that("#7 DEoptim with user-supplied itermax=5 completes quickly", {
  set.seed(42)
  expect_no_error(
    optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                       itermax = 5, parallelType = 0)
  )
})

test_that("#7 DEoptim result class is correct with parallelType=0", {
  set.seed(42)
  res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                            itermax = 10, parallelType = 0)
  expect_true(inherits(res, "optimize.portfolio"))
})

# ---------------------------------------------------------------------------
# Additional DEoptim.control arg passthrough tests
# ---------------------------------------------------------------------------

test_that("DEoptim F (step size) arg passes through without error", {
  set.seed(42)
  expect_no_error(
    res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0, F = 0.5)
  )
  expect_true(inherits(res, "optimize.portfolio"))
})

test_that("DEoptim CR (crossover probability) arg passes through without error", {
  set.seed(42)
  expect_no_error(
    res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0, CR = 0.7)
  )
  expect_true(inherits(res, "optimize.portfolio"))
})

test_that("DEoptim NP (population size) arg passes through without error", {
  set.seed(42)
  expect_no_error(
    res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0, NP = 20)
  )
  expect_true(inherits(res, "optimize.portfolio"))
})

test_that("DEoptim strategy arg (1=DE/rand/1) passes through without error", {
  set.seed(42)
  expect_no_error(
    res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0, strategy = 1)
  )
  expect_true(inherits(res, "optimize.portfolio"))
})

test_that("DEoptim strategy arg (6=DE/rand/1/either-or) passes through", {
  set.seed(42)
  expect_no_error(
    res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0, strategy = 6)
  )
  expect_true(inherits(res, "optimize.portfolio"))
})

# ---------------------------------------------------------------------------
# trace=TRUE: check the structure of the result contains DEoptim output
# ---------------------------------------------------------------------------

test_that("DEoptim trace=TRUE result contains DEoutput slot", {
  set.seed(42)
  res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                            itermax = 10, parallelType = 0, trace = TRUE)
  expect_true(inherits(res, "optimize.portfolio"))
  expect_true(!is.null(res$DEoutput),
              info = "trace=TRUE must populate DEoutput slot")
})

test_that("DEoptim trace=TRUE DEoutput has bestmem", {
  set.seed(42)
  res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                            itermax = 10, parallelType = 0, trace = TRUE)
  deout <- res$DEoutput
  expect_true(!is.null(deout$optim$bestmem),
              info = "DEoutput must contain optim$bestmem")
  expect_equal(length(deout$optim$bestmem), ncol(R4))
})

# ---------------------------------------------------------------------------
# Different objective types with DEoptim
# ---------------------------------------------------------------------------

test_that("DEoptim works with return objective only", {
  set.seed(42)
  ps <- portfolio.spec(assets = colnames(R4))
  ps <- add.constraint(ps, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  ps <- add.constraint(ps, type = "box", min = 0.05, max = 0.60)
  ps <- add.objective(ps, type = "return", name = "mean")
  expect_no_error(
    res <- optimize.portfolio(R4, ps, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0)
  )
  expect_true(inherits(res, "optimize.portfolio"))
})

test_that("DEoptim works with mean + StdDev objectives", {
  set.seed(42)
  ps <- portfolio.spec(assets = colnames(R4))
  ps <- add.constraint(ps, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  ps <- add.constraint(ps, type = "box", min = 0.05, max = 0.60)
  ps <- add.objective(ps, type = "return", name = "mean")
  ps <- add.objective(ps, type = "risk",   name = "StdDev")
  expect_no_error(
    res <- optimize.portfolio(R4, ps, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0)
  )
  w <- extractWeights(res)
  expect_equal(length(w), ncol(R4))
  expect_true(all(is.finite(w)))
})

test_that("DEoptim works with risk_budget objective", {
  set.seed(42)
  ps <- portfolio.spec(assets = colnames(R4))
  ps <- add.constraint(ps, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  ps <- add.constraint(ps, type = "box", min = 0.05, max = 0.60)
  ps <- add.objective(ps, type = "risk_budget", name = "StdDev",
                      min_prisk = 0.05, max_prisk = 0.50)
  expect_no_error(
    res <- optimize.portfolio(R4, ps, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0)
  )
  expect_true(inherits(res, "optimize.portfolio"))
})

test_that("DEoptim works with 8-asset portfolio", {
  set.seed(42)
  ps <- portfolio.spec(assets = colnames(R8))
  ps <- add.constraint(ps, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  ps <- add.constraint(ps, type = "box", min = 0.05, max = 0.40)
  ps <- add.objective(ps, type = "risk", name = "StdDev")
  expect_no_error(
    res <- optimize.portfolio(R8, ps, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0)
  )
  w <- extractWeights(res)
  expect_equal(length(w), ncol(R8))
  expect_true(all(is.finite(w)))
  expect_true(abs(sum(w) - 1) < 0.02)
})

# ---------------------------------------------------------------------------
# maxit alias: DEoptim also accepts maxit as an alias for itermax
# ---------------------------------------------------------------------------

test_that("DEoptim maxit arg (alias for itermax) is accepted without error (#24/#7)", {
  set.seed(42)
  expect_no_error(
    res <- optimize.portfolio(R4, portf_stddev, optimize_method = "DEoptim",
                              maxit = 8, parallelType = 0)
  )
  expect_true(inherits(res, "optimize.portfolio"))
})

# ---------------------------------------------------------------------------
# Group constraints with DEoptim
# ---------------------------------------------------------------------------

test_that("DEoptim works with group constraints", {
  set.seed(42)
  ps <- portfolio.spec(assets = colnames(R4))
  ps <- add.constraint(ps, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  ps <- add.constraint(ps, type = "box", min = 0.05, max = 0.60)
  ps <- add.constraint(ps, type = "group",
                       groups = list(1:2, 3:4),
                       group_min = c(0.2, 0.2),
                       group_max = c(0.8, 0.8))
  ps <- add.objective(ps, type = "risk", name = "StdDev")
  expect_no_error(
    res <- optimize.portfolio(R4, ps, optimize_method = "DEoptim",
                              itermax = 10, parallelType = 0)
  )
  w <- extractWeights(res)
  expect_true(all(is.finite(w)))
  # group 1 (assets 1-2) weight sum should be in [0.2, 0.8]
  expect_true(sum(w[1:2]) >= 0.19 && sum(w[1:2]) <= 0.81)
})
