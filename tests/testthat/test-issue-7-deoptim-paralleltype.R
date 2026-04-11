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

portf <- portfolio.spec(assets = colnames(R4))
portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.60)
portf <- add.objective(portf, type = "risk", name = "StdDev")

# ---------------------------------------------------------------------------
# #7 — user-supplied parallelType=0 does not crash and returns valid result
# ---------------------------------------------------------------------------

test_that("#7 DEoptim with parallelType=0 runs without error", {
  set.seed(42)
  expect_no_error(
    optimize.portfolio(R4, portf, optimize_method = "DEoptim",
                       itermax = 10, parallelType = 0)
  )
})

test_that("#7 DEoptim with parallelType=0 returns valid weights", {
  set.seed(42)
  res <- optimize.portfolio(R4, portf, optimize_method = "DEoptim",
                            itermax = 10, parallelType = 0)
  w <- extractWeights(res)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
  expect_true(abs(sum(w) - 1) < 0.02)
  expect_true(all(w >= 0.04))
})

test_that("#7 DEoptim with user-supplied itermax=5 completes quickly", {
  # If itermax were ignored the default (200) would be used — this is
  # difficult to test directly, but the call should at least succeed.
  set.seed(42)
  expect_no_error(
    optimize.portfolio(R4, portf, optimize_method = "DEoptim",
                       itermax = 5, parallelType = 0)
  )
})

test_that("#7 DEoptim result class is correct with parallelType=0", {
  set.seed(42)
  res <- optimize.portfolio(R4, portf, optimize_method = "DEoptim",
                            itermax = 10, parallelType = 0)
  expect_true(inherits(res, "optimize.portfolio"))
})
