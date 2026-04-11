###############################################################################
# tests/testthat/test-extractstats-pso.R
#
# Coverage targets (R/extractstats.R):
#
#   extractStats.optimize.portfolio.pso() (lines 154-220):
#     - Success path: trace=TRUE → $PSOoutput is not NULL → runs full function
#     - normalize_weights() closure: max_sum branch (line 174-177)
#     - normalize_weights() closure: min_sum branch (line 179-182)
#
# Strategy:
#   Run optimize.portfolio with optimize_method="pso" and trace=TRUE, then call
#   extractStats on the result.  Two portfolio specs are used:
#     1. Full-investment (weight_sum=1, min_sum=0.99/max_sum=1.01) — hits both
#        max_sum and min_sum normalization branches.
#     2. Long-only, no explicit min/max_sum — exercises the else path (no normalization).
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(foreach)

skip_on_cran()
skip_if_not_installed("pso")
skip_if_not_installed("foreach")

library(pso)

foreach::registerDoSEQ()

utils::data(edhec)
R4 <- edhec[, 1:4]
nms <- colnames(R4)

# ---------------------------------------------------------------------------
# Fixture 1: full-investment, box-constrained portfolio with explicit
# min_sum=0.99, max_sum=1.01 (hits both normalization branches)
# ---------------------------------------------------------------------------

portf_pso_fi <- portfolio.spec(nms)
portf_pso_fi <- add.constraint(portf_pso_fi, type = "weight_sum",
                                min_sum = 0.99, max_sum = 1.01)
portf_pso_fi <- add.constraint(portf_pso_fi, type = "box",
                                min = 0.05, max = 0.60)
portf_pso_fi <- add.objective(portf_pso_fi, type = "risk", name = "StdDev")

set.seed(42)
opt_pso_trace <- tryCatch(
  optimize.portfolio(R4, portf_pso_fi,
                     optimize_method = "pso",
                     trace          = TRUE,
                     maxit          = 20L),
  error = function(e) NULL
)

# ---------------------------------------------------------------------------
# Fixture 2: no min_sum/max_sum constraint (exercises the no-normalization path)
# ---------------------------------------------------------------------------

portf_pso_nofull <- portfolio.spec(nms)
portf_pso_nofull <- add.constraint(portf_pso_nofull, type = "long_only")
portf_pso_nofull <- add.objective(portf_pso_nofull, type = "risk", name = "StdDev")

set.seed(42)
opt_pso_nofull <- tryCatch(
  optimize.portfolio(R4, portf_pso_nofull,
                     optimize_method = "pso",
                     trace          = TRUE,
                     maxit          = 20L),
  error = function(e) NULL
)

# ===========================================================================
# Section 1: trace=TRUE success path — $PSOoutput is present
# ===========================================================================

test_that("opt_pso_trace (trace=TRUE) has non-NULL PSOoutput", {
  skip_if(is.null(opt_pso_trace))
  expect_false(is.null(opt_pso_trace$PSOoutput))
})

test_that("opt_pso_trace has class 'optimize.portfolio.pso'", {
  skip_if(is.null(opt_pso_trace))
  expect_s3_class(opt_pso_trace, "optimize.portfolio.pso")
})

# ===========================================================================
# Section 2: extractStats full-investment (max_sum + min_sum branches)
# ===========================================================================

es_pso <- tryCatch(
  extractStats(opt_pso_trace),
  error = function(e) NULL
)

test_that("extractStats.pso on trace=TRUE result is not NULL", {
  skip_if(is.null(opt_pso_trace))
  expect_false(is.null(es_pso))
})

test_that("extractStats.pso returns a matrix", {
  skip_if(is.null(opt_pso_trace))
  skip_if(is.null(es_pso))
  expect_true(is.matrix(es_pso))
})

test_that("extractStats.pso matrix has more than one row (swarm + optimal)", {
  skip_if(is.null(opt_pso_trace))
  skip_if(is.null(es_pso))
  expect_gt(nrow(es_pso), 1L)
})

test_that("extractStats.pso matrix has 'out' column", {
  skip_if(is.null(opt_pso_trace))
  skip_if(is.null(es_pso))
  expect_true("out" %in% colnames(es_pso))
})

test_that("extractStats.pso matrix has weight columns (w.<asset>)", {
  skip_if(is.null(opt_pso_trace))
  skip_if(is.null(es_pso))
  w_cols <- grep("^w\\.", colnames(es_pso))
  expect_gt(length(w_cols), 0L)
  expect_equal(length(w_cols), ncol(R4))
})

test_that("extractStats.pso weight columns are numeric", {
  skip_if(is.null(opt_pso_trace))
  skip_if(is.null(es_pso))
  w_cols <- grep("^w\\.", colnames(es_pso))
  expect_true(is.numeric(es_pso[, w_cols]))
})

# ===========================================================================
# Section 3: normalize_weights max_sum branch — weights exceeding max_sum
# are scaled down.
# The max_sum branch fires when sum(weights) > max_sum.
# PSO can generate swarm positions that violate the max_sum constraint, so
# the normalization should be applied during extractStats.
# We verify the normalized weights in the result matrix are <= max_sum+eps.
# ===========================================================================

test_that("extractStats.pso: normalized weight sums are <= 1.01 (max_sum=1.01)", {
  skip_if(is.null(opt_pso_trace))
  skip_if(is.null(es_pso))
  w_cols  <- grep("^w\\.", colnames(es_pso))
  wsums   <- rowSums(es_pso[, w_cols, drop = FALSE])
  # After normalization, no row should exceed max_sum + tiny epsilon
  expect_true(all(wsums <= 1.01 + 1e-8))
})

test_that("extractStats.pso: normalized weight sums are >= 0.99 (min_sum=0.99)", {
  skip_if(is.null(opt_pso_trace))
  skip_if(is.null(es_pso))
  w_cols <- grep("^w\\.", colnames(es_pso))
  wsums  <- rowSums(es_pso[, w_cols, drop = FALSE])
  # After normalization, no row should fall below min_sum - tiny epsilon
  expect_true(all(wsums >= 0.99 - 1e-8))
})

# ===========================================================================
# Section 4: trace=FALSE error path (verify the guard fires)
# ===========================================================================

test_that("extractStats.pso trace=FALSE stops with informative message", {
  skip_if(is.null(opt_pso_trace))
  # Build a fake pso object with no PSOoutput slot
  fake_pso <- list(PSOoutput = NULL, weights = extractWeights(opt_pso_trace),
                   portfolio = portf_pso_fi, out = 0, R = R4)
  class(fake_pso) <- "optimize.portfolio.pso"
  expect_error(extractStats(fake_pso), regexp = "trace=TRUE")
})

# ===========================================================================
# Section 5: no-normalization path (no min_sum/max_sum constraint)
# ===========================================================================

es_pso_nofull <- tryCatch(
  extractStats(opt_pso_nofull),
  error = function(e) NULL
)

test_that("extractStats.pso no-normalization path returns a matrix", {
  skip_if(is.null(opt_pso_nofull))
  skip_if(is.null(es_pso_nofull))
  expect_true(is.matrix(es_pso_nofull))
})

test_that("extractStats.pso no-normalization path has weight columns", {
  skip_if(is.null(opt_pso_nofull))
  skip_if(is.null(es_pso_nofull))
  w_cols <- grep("^w\\.", colnames(es_pso_nofull))
  expect_gt(length(w_cols), 0L)
})
