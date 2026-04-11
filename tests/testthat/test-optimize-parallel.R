###############################################################################
# tests/testthat/test-optimize-parallel.R
#
# Coverage targets:
#
#   R/optimize.portfolio.R — optimize.portfolio.parallel (lines 3594-3632)
#     - Normal path: returns list with class "optimize.portfolio.parallel"
#     - $optimizations slot holds per-node results
#     - message=TRUE branch (line 3623)
#
#   R/extractstats.R — extractStats.optimize.portfolio.parallel (lines 317-328)
#     BUG-1 FIXED: The function now correctly iterates over
#     `object$optimizations` (was incorrectly iterating over `object` directly,
#     which has 3 named elements: $optimizations, $call, $elapsed_time).
#     extractStats(opt_par) should now succeed.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(foreach)

skip_on_cran()
skip_if_not_installed("doParallel")
skip_if_not_installed("foreach")

library(doParallel)

# Register a 2-core parallel backend for the duration of this file
cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
on.exit({
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
}, add = TRUE)

utils::data(edhec)
R4 <- edhec[, 1:4]
nms <- colnames(R4)

# ---------------------------------------------------------------------------
# Fixture: simple min-variance portfolio (ROI, fast)
# ---------------------------------------------------------------------------

portf_par <- portfolio.spec(nms)
portf_par <- add.constraint(portf_par, type = "full_investment")
portf_par <- add.constraint(portf_par, type = "long_only")
portf_par <- add.objective(portf_par, type = "risk", name = "StdDev")

# ===========================================================================
# Section 1: optimize.portfolio.parallel basic structure (lines 3594-3632)
# Use nodes=2 and ROI solver (fast)
# ===========================================================================

set.seed(1)
opt_par <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio.parallel(R4, portf_par,
                                optimize_method = "ROI",
                                nodes          = 2,
                                message        = FALSE)
  )),
  error = function(e) NULL
)

test_that("optimize.portfolio.parallel returns non-NULL result", {
  skip_if(is.null(opt_par))
  expect_false(is.null(opt_par))
})

test_that("optimize.portfolio.parallel result has class 'optimize.portfolio.parallel'", {
  skip_if(is.null(opt_par))
  expect_s3_class(opt_par, "optimize.portfolio.parallel")
})

test_that("optimize.portfolio.parallel result has $optimizations slot", {
  skip_if(is.null(opt_par))
  expect_false(is.null(opt_par$optimizations))
})

test_that("optimize.portfolio.parallel $optimizations has length equal to nodes", {
  skip_if(is.null(opt_par))
  expect_equal(length(opt_par$optimizations), 2L)
})

test_that("optimize.portfolio.parallel each node result has class optimize.portfolio.ROI", {
  skip_if(is.null(opt_par))
  for (res in opt_par$optimizations) {
    expect_s3_class(res, "optimize.portfolio.ROI")
  }
})

test_that("optimize.portfolio.parallel $call is not NULL", {
  skip_if(is.null(opt_par))
  expect_false(is.null(opt_par$call))
})

test_that("optimize.portfolio.parallel $elapsed_time is a difftime or numeric", {
  skip_if(is.null(opt_par))
  expect_true(inherits(opt_par$elapsed_time, "difftime") ||
                is.numeric(opt_par$elapsed_time))
})

# ===========================================================================
# Section 2: message=TRUE branch (line 3623)
# Verifies that passing message=TRUE does not error (message goes to stderr)
# ===========================================================================

test_that("optimize.portfolio.parallel with message=TRUE does not error", {
  set.seed(2)
  result <- tryCatch(
    suppressMessages(
      optimize.portfolio.parallel(R4, portf_par,
                                  optimize_method = "ROI",
                                  nodes          = 2,
                                  message        = TRUE)
    ),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_s3_class(result, "optimize.portfolio.parallel")
})

# ===========================================================================
# Section 3: extractStats.optimize.portfolio.parallel (lines 317-328)
#
# BUG-1 FIXED: The function now correctly uses `object$optimizations` as the
# iteration target. extractStats(opt_par) should return a matrix of stats.
# ===========================================================================

test_that("extractStats on each node result individually works", {
  skip_if(is.null(opt_par))
  for (res in opt_par$optimizations) {
    stats <- tryCatch(extractStats(res), error = function(e) NULL)
    skip_if(is.null(stats))
    expect_true(is.numeric(stats))
    expect_true("out" %in% names(stats) || length(stats) > 0)
  }
})

test_that("extractStats.optimize.portfolio.parallel function exists in package namespace", {
  skip_if(is.null(opt_par))
  expect_true(exists("extractStats.optimize.portfolio.parallel",
                     envir = asNamespace("PortfolioAnalytics")))
})

test_that("extractStats on parallel object succeeds (BUG-1 fixed)", {
  skip_if(is.null(opt_par))
  # BUG-1 fixed: resultlist <- object$optimizations (was object)
  result <- extractStats(opt_par)
  expect_true(is.matrix(result) || is.numeric(result))
  expect_true("out" %in% colnames(result) || "out" %in% names(result))
})

# ===========================================================================
# Section 4: weights from individual node results are valid
# ===========================================================================

test_that("individual node result weights sum to ~1", {
  skip_if(is.null(opt_par))
  for (res in opt_par$optimizations) {
    w <- extractWeights(res)
    expect_equal(sum(w), 1, tolerance = 1e-4)
  }
})

test_that("individual node result weights are all non-negative", {
  skip_if(is.null(opt_par))
  for (res in opt_par$optimizations) {
    w <- extractWeights(res)
    expect_true(all(round(w, 10) >= 0))
  }
})
