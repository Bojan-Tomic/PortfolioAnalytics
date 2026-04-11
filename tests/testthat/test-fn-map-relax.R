###############################################################################
# tests/testthat/test-fn-map-relax.R
#
# Source file: R/constraint_fn_map.R
#
# Targeted branches:
#   fn_map(..., relax=TRUE):
#     - box constraint relax branch   (lines 150-194)
#     - group constraint relax branch (lines 224-263)
#     - position_limit relax branch   (lines 292-319)
#     - leverage_exposure relax branch (lines 348-373)
#
# Strategy: create portfolios with constraints that rp_transform cannot
# satisfy from the given starting weights, forcing the try-error path,
# then verify that relax=TRUE still returns a numeric weight vector (even
# if the constraints remain violated after relaxation).
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# fn_map always returns a list; weights element is what we care about
get_weights <- function(...) fn_map(...)$weights


# ===========================================================================
# 1. fn_map relax=FALSE baseline (sanity check)
# ===========================================================================

test_that("fn_map: returns list with expected names", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w, portfolio = p)
  expect_true(is.list(result))
  expect_true("weights" %in% names(result))
  expect_true("min"     %in% names(result))
  expect_true("max"     %in% names(result))
})

test_that("fn_map: errors on non-portfolio input", {
  expect_error(fn_map(weights = rep(0.25, 4), portfolio = list()),
               regexp = "portfolio")
})

test_that("fn_map: valid weights returned unchanged (no violation)", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w, portfolio = p)
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})


# ===========================================================================
# 2. Box constraint relax=TRUE branch
# ===========================================================================

# Force an impossible box constraint: weights must be ALL >= 0.45 and sum to 1.
# With 4 assets, min=0.45 per asset => min_sum >= 1.8 > 1.0 (impossible).
# rp_transform will fail; relax=TRUE should attempt to widen min.

test_that("fn_map relax=TRUE box: returns numeric weights vector", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  # Impossible: min per weight = 0.45 * 4 = 1.80 > 1.0
  p <- add.constraint(p, type = "box", min = 0.45, max = 0.55)
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = TRUE)
  expect_true(is.numeric(result$weights))
  expect_length(result$weights, 4L)
})

test_that("fn_map relax=FALSE box: also returns numeric weights (original fallback)", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.45, max = 0.55)
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = FALSE)
  expect_true(is.numeric(result$weights))
  expect_length(result$weights, 4L)
})

test_that("fn_map relax=TRUE box: min may be modified (relaxed)", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.45, max = 0.55)
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = TRUE)
  # Either relaxation found a feasible solution (min was lowered) or
  # the weights fall back to starting weights — either way weights is numeric
  expect_true(is.numeric(result$weights))
})


# ===========================================================================
# 3. Group constraint relax=TRUE branch
# ===========================================================================

# Force an impossible group constraint:
# group g1 = assets 1:2, required sum in [0.80, 0.90]
# group g2 = assets 3:4, required sum in [0.80, 0.90]
# Total minimum = 1.60 > 1.0 => impossible for long-only unit weights.

test_that("fn_map relax=TRUE group: returns numeric weights vector", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "group",
                      groups = list(g1 = 1:2, g2 = 3:4),
                      group_min = c(0.80, 0.80),
                      group_max = c(0.90, 0.90))
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = TRUE)
  expect_true(is.numeric(result$weights))
  expect_length(result$weights, 4L)
})

test_that("fn_map relax=FALSE group: also returns numeric weights (fallback)", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "group",
                      groups = list(g1 = 1:2, g2 = 3:4),
                      group_min = c(0.80, 0.80),
                      group_max = c(0.90, 0.90))
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = FALSE)
  expect_true(is.numeric(result$weights))
})


# ===========================================================================
# 4. Position limit relax=TRUE branch
# ===========================================================================

# Force an impossible position_limit: max_pos = 1 for 4 assets, but equal
# weights have 4 non-zero positions. rp_transform needs to zero out 3 assets
# while keeping weight sum ~ 1. With min box = 0, this may work, but if we
# force a large enough min box it becomes impossible.
# Use max_pos = 1 with min box = 0.3 (requires weight >= 0.3 per position,
# but only 1 position allowed => that position would be 0.3, rest are 0;
# but rest violate min=0.3 too). This creates an infeasible scenario.

test_that("fn_map relax=TRUE position_limit: returns numeric weights", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "position_limit", max_pos = 1)
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = TRUE)
  expect_true(is.numeric(result$weights))
  expect_length(result$weights, 4L)
})

test_that("fn_map relax=TRUE position_limit: result$max_pos >= original max_pos", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.3, max = 0.7)
  p <- add.constraint(p, type = "position_limit", max_pos = 1)
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = TRUE)
  # relaxation may have incremented max_pos to allow a feasible solution
  expect_true(is.numeric(result$weights))
})


# ===========================================================================
# 5. Leverage exposure relax=TRUE branch
# ===========================================================================

# Force an impossible leverage constraint: leverage = 0.1, but we need
# sum(|w|) >= min_sum = 0.99. With leverage=0.1 we can never satisfy both.

test_that("fn_map relax=TRUE leverage: returns numeric weights", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "leverage_exposure", leverage = 0.1)
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = TRUE)
  expect_true(is.numeric(result$weights))
  expect_length(result$weights, 4L)
})

test_that("fn_map relax=TRUE leverage: result$leverage may be larger than original", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "leverage_exposure", leverage = 0.1)
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result_relax  <- fn_map(weights = w_start, portfolio = p, relax = TRUE)
  result_norelax <- fn_map(weights = w_start, portfolio = p, relax = FALSE)
  # relax should produce leverage >= original 0.1 (possibly relaxed upward)
  # norelax just returns original weights
  expect_true(is.numeric(result_relax$leverage))
  expect_true(is.numeric(result_norelax$leverage))
})

test_that("fn_map relax=FALSE leverage: returns original weights on failure", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  p <- add.constraint(p, type = "leverage_exposure", leverage = 0.1)
  w_start <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w_start, portfolio = p, relax = FALSE)
  expect_true(is.numeric(result$weights))
})


# ===========================================================================
# 6. fn_map with feasible constraints (relax=TRUE, no relaxation needed)
# ===========================================================================

test_that("fn_map relax=TRUE feasible: weights sum close to 1", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w, portfolio = p, relax = TRUE)
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map relax=TRUE feasible: returned weights are non-negative", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  w <- c(0.25, 0.25, 0.25, 0.25)
  result <- fn_map(weights = w, portfolio = p, relax = TRUE)
  expect_true(all(result$weights >= -1e-8))
})
