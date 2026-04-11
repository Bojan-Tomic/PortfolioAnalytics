###############################################################################
# tests/testthat/test-parallel-solver.R
#
# Tests for optimize.portfolio.parallel() and its S3 methods:
#   summary.optimize.portfolio.parallel
#   print.optimize.portfolio.parallel
#
# Source files covered:
#   - R/optimize.portfolio.R  (optimize.portfolio.parallel)
#   - R/generics.R            (summary / print methods)
#
# Shared fixtures from helper-portfolioanalytics.R — do NOT redefine:
#   edhec4          — edhec[, 1:4], 4-asset return series
#   .portf_stoch4   — portfolio.spec with weight_sum(0.99,1.01),
#                     box(0.05,0.60), StdDev objective
#   opt_parallel    — result of optimize.portfolio.parallel() run with
#                     nodes=2, search_size=200; NULL when doParallel /
#                     foreach are unavailable or the run errored.
#
# Every test_that block is guarded with skip_if(is.null(opt_parallel)) so
# the suite degrades gracefully on machines without a parallel back-end.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("doParallel")
skip_if_not_installed("foreach")

library(PortfolioAnalytics)


# ===========================================================================
# Class and top-level structure
# ===========================================================================

test_that("opt_parallel has class optimize.portfolio.parallel", {
  skip_if(is.null(opt_parallel))
  expect_s3_class(opt_parallel, "optimize.portfolio.parallel")
})

test_that("opt_parallel$optimizations is a list", {
  skip_if(is.null(opt_parallel))
  expect_true(is.list(opt_parallel$optimizations),
    label = "opt_parallel$optimizations is a list"
  )
})

test_that("opt_parallel$optimizations has length 2 matching nodes argument", {
  skip_if(is.null(opt_parallel))
  expect_length(opt_parallel$optimizations, 2L)
})

test_that("each element of opt_parallel$optimizations inherits optimize.portfolio", {
  skip_if(is.null(opt_parallel))
  for (i in seq_along(opt_parallel$optimizations)) {
    expect_s3_class(
      opt_parallel$optimizations[[i]],
      "optimize.portfolio",
      exact = FALSE
    )
  }
})

test_that("opt_parallel$elapsed_time is a difftime or numeric duration value", {
  skip_if(is.null(opt_parallel))
  et <- opt_parallel$elapsed_time
  # elapsed_t is produced by end_t - start_t where both are Sys.time() POSIXct
  # values, so the difference is a difftime object.
  expect_true(
    inherits(et, "difftime") || is.numeric(et),
    label = "elapsed_time is a difftime or numeric"
  )
  expect_true(as.numeric(et) >= 0, label = "elapsed_time is non-negative")
})


# ===========================================================================
# summary.optimize.portfolio.parallel
# ===========================================================================

test_that("summary returns class summary.optimize.portfolio.parallel", {
  skip_if(is.null(opt_parallel))
  s <- summary(opt_parallel)
  expect_s3_class(s, "summary.optimize.portfolio.parallel")
})

test_that("summary n_optimizations equals 2 matching nodes argument", {
  skip_if(is.null(opt_parallel))
  s <- summary(opt_parallel)
  expect_equal(s$n_optimizations, 2L)
})

test_that("summary stats slot is a matrix or data.frame", {
  skip_if(is.null(opt_parallel))
  s <- summary(opt_parallel)
  expect_true(
    is.matrix(s$stats) || is.data.frame(s$stats),
    label = "stats is a matrix or data.frame"
  )
})

test_that("summary stats slot has 2 rows one per node optimization", {
  skip_if(is.null(opt_parallel))
  s <- summary(opt_parallel)
  expect_equal(nrow(s$stats), 2L)
})

test_that("summary obj_val slot is a numeric vector of length 2", {
  skip_if(is.null(opt_parallel))
  s <- summary(opt_parallel)
  expect_true(is.numeric(s$obj_val),
    label = "obj_val is numeric"
  )
  expect_length(s$obj_val, 2L)
})

test_that("summary call slot is preserved from the original optimize.portfolio.parallel call", {
  skip_if(is.null(opt_parallel))
  s <- summary(opt_parallel)
  expect_false(is.null(s$call),
    label = "summary$call is not NULL"
  )
})

test_that("summary does not error when called repeatedly on opt_parallel", {
  skip_if(is.null(opt_parallel))
  expect_no_error(summary(opt_parallel))
  expect_no_error(summary(opt_parallel))
})


# ===========================================================================
# print.optimize.portfolio.parallel
# ===========================================================================

test_that("print output contains the string PortfolioAnalytics", {
  skip_if(is.null(opt_parallel))
  expect_output(print(opt_parallel), "PortfolioAnalytics")
})

test_that("print does not error on opt_parallel", {
  skip_if(is.null(opt_parallel))
  expect_no_error(print(opt_parallel))
})
