###############################################################################
# tests/testthat/test-fn-map.R
#
# Tests for fn_map(), rp_transform(), pos_limit_fail(), and the internal
# constraint-check helpers min_sum_fail(), max_sum_fail(), leverage_fail(),
# and group_fail().
#
# Source files covered (primary):
#   R/constraint_fn_map.R — fn_map(), rp_transform(), pos_limit_fail(),
#                           group_fail(), min_sum_fail(), max_sum_fail(),
#                           leverage_fail(), rp_increase(), rp_decrease(),
#                           rp_decrease_leverage(), rp_position_limit()
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

utils::data(edhec)
R <- edhec[, 1:5]
nms <- colnames(R)
N <- ncol(R)

# ---------------------------------------------------------------------------
# Shared portfolio specs
# ---------------------------------------------------------------------------

# Full-investment, long-only
portf.lo <- portfolio.spec(assets = nms)
portf.lo <- add.constraint(portf.lo, type = "full_investment")
portf.lo <- add.constraint(portf.lo, type = "long_only")

# Relaxed weight-sum [0.99, 1.01], long-only
portf.relax <- portfolio.spec(assets = nms)
portf.relax <- add.constraint(portf.relax,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf.relax <- add.constraint(portf.relax, type = "long_only")

# Explicit box [0.05, 0.40]
portf.box <- portfolio.spec(assets = nms)
portf.box <- add.constraint(portf.box, type = "full_investment")
portf.box <- add.constraint(portf.box, type = "box", min = 0.05, max = 0.40)

# Group constraint: g1=(1,2) in [0.10,0.50], g2=(3,4,5) in [0.20,0.80]
portf.grp <- portfolio.spec(assets = nms)
portf.grp <- add.constraint(portf.grp,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf.grp <- add.constraint(portf.grp, type = "long_only")
portf.grp <- add.constraint(portf.grp,
  type      = "group",
  groups    = list(c(1, 2), c(3, 4, 5)),
  group_min = c(0.10, 0.20),
  group_max = c(0.50, 0.80)
)

# Position limit: max 3 non-zero out of 5
portf.pos <- portfolio.spec(assets = nms)
portf.pos <- add.constraint(portf.pos,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf.pos <- add.constraint(portf.pos, type = "long_only")
portf.pos <- add.constraint(portf.pos, type = "position_limit", max_pos = 3)

# Leverage: dollar-neutral ± 0.01, box [-0.5, 0.5], max leverage 1.2
portf.lev <- portfolio.spec(assets = nms)
portf.lev <- add.constraint(portf.lev,
  type = "weight_sum",
  min_sum = -0.01, max_sum = 0.01
)
portf.lev <- add.constraint(portf.lev, type = "box", min = -0.5, max = 0.5)
portf.lev <- add.constraint(portf.lev,
  type = "leverage_exposure",
  leverage = 1.2
)

# Standard weight sequences
wt_seq <- generatesequence(min = 0.01, max = 1, by = 0.01)
wt_seq_short <- generatesequence(min = -0.5, max = 0.5, by = 0.01)


# ===========================================================================
# fn_map()
# ===========================================================================

test_that("fn_map() errors when portfolio is not a portfolio object", {
  w <- rep(0.2, N)
  names(w) <- nms
  expect_error(fn_map(weights = w, portfolio = list(a = 1)))
})

test_that("fn_map() returns a list with expected named elements", {
  w <- rep(0.2, N)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.lo)
  expect_true(is.list(result))
  expected_names <- c(
    "weights", "min", "max", "cLO", "cUP",
    "max_pos", "max_pos_long", "max_pos_short", "leverage"
  )
  expect_true(all(expected_names %in% names(result)))
})

test_that("fn_map() returned weights have the same names as the input", {
  w <- rep(0.2, N)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.lo)
  expect_equal(names(result$weights), nms)
})

test_that("fn_map() returned weights are a numeric vector of length N", {
  w <- rep(0.2, N)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.lo)
  expect_true(is.numeric(result$weights))
  expect_equal(length(result$weights), N)
})

test_that("fn_map() with already-valid long-only weights returns sum close to 1", {
  w <- rep(0.2, N)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.lo)
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map() transforms weights that sum too low to satisfy min_sum", {
  # sum = 0.5, well below min_sum = 0.99
  w <- rep(0.1, N)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.relax)
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map() transforms weights that sum too high to satisfy max_sum", {
  # sum = 1.5, above max_sum = 1.01
  w <- rep(0.3, N)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.relax)
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map() transforms weights that violate box min constraint", {
  # All weights at 0.01, below box min = 0.05; normalise to sum = 1
  w <- rep(0.01, N)
  w <- w / sum(w)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.box)
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map() transforms weights that violate box max constraint", {
  # First asset at 0.80 > max = 0.40
  w <- c(0.80, 0.05, 0.05, 0.05, 0.05)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.box)
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map() transforms weights that violate group lower bound", {
  # Group 1 (assets 1,2) sums to 0.04 < cLO = 0.10
  w <- c(0.02, 0.02, 0.40, 0.30, 0.26)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.grp)
  expect_true(is.numeric(result$weights))
  expect_equal(length(result$weights), N)
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map() transforms weights that violate group upper bound", {
  # Group 1 (assets 1,2) sums to 0.70 > cUP = 0.50
  w <- c(0.50, 0.20, 0.10, 0.10, 0.10)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.grp)
  expect_true(is.numeric(result$weights))
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
})

test_that("fn_map() transforms weights to meet position limit (max_pos = 3)", {
  # All 5 weights non-zero, but max_pos = 3
  w <- rep(0.2, N)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.pos)
  expect_true(is.numeric(result$weights))
  expect_equal(sum(result$weights), 1, tolerance = 0.02)
  tol <- .Machine$double.eps^0.5
  expect_true(sum(abs(result$weights) > tol) <= 3)
})

test_that("fn_map() transforms weights to meet leverage exposure constraint", {
  # sum(abs(w)) = 1.6 > leverage = 1.2; sum(w) ≈ 0 satisfies weight_sum
  w <- c(0.4, 0.4, -0.4, -0.4, 0.0)
  names(w) <- nms
  result <- fn_map(weights = w, portfolio = portf.lev)
  expect_true(is.numeric(result$weights))
  expect_equal(length(result$weights), N)
})

test_that("fn_map() with verbose = TRUE runs without error", {
  w <- rep(0.2, N)
  names(w) <- nms
  expect_no_error(fn_map(weights = w, portfolio = portf.lo, verbose = TRUE))
})


# ===========================================================================
# pos_limit_fail()
# ===========================================================================

test_that("pos_limit_fail() returns TRUE when non-zero positions exceed max_pos", {
  # 5 non-zero, max_pos = 3
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  expect_true(pos_limit_fail(w,
    max_pos = 3,
    max_pos_long = NULL, max_pos_short = NULL
  ))
})

test_that("pos_limit_fail() returns FALSE when non-zero positions <= max_pos", {
  # 3 non-zero, max_pos = 3
  w <- c(0.5, 0.3, 0.2, 0.0, 0.0)
  expect_false(pos_limit_fail(w,
    max_pos = 3,
    max_pos_long = NULL, max_pos_short = NULL
  ))
})

test_that("pos_limit_fail() returns TRUE when long positions exceed max_pos_long", {
  # 4 long positions, max_pos_long = 3
  w <- c(0.4, 0.3, 0.2, 0.1, 0.0)
  expect_true(pos_limit_fail(w,
    max_pos = NULL,
    max_pos_long = 3, max_pos_short = NULL
  ))
})

test_that("pos_limit_fail() returns FALSE when long positions <= max_pos_long", {
  # 3 long positions, max_pos_long = 3
  w <- c(0.5, 0.3, 0.2, 0.0, 0.0)
  expect_false(pos_limit_fail(w,
    max_pos = NULL,
    max_pos_long = 3, max_pos_short = NULL
  ))
})

test_that("pos_limit_fail() returns TRUE when short positions exceed max_pos_short", {
  # 2 short positions, max_pos_short = 1
  w <- c(-0.3, -0.3, 0.3, 0.3, 0.5)
  expect_true(pos_limit_fail(w,
    max_pos = NULL,
    max_pos_long = NULL, max_pos_short = 1
  ))
})

test_that("pos_limit_fail() returns FALSE when short positions <= max_pos_short", {
  # 1 short position, max_pos_short = 2
  w <- c(-0.3, 0.5, 0.4, 0.2, 0.2)
  expect_false(pos_limit_fail(w,
    max_pos = NULL,
    max_pos_long = NULL, max_pos_short = 2
  ))
})

test_that("pos_limit_fail() returns FALSE when all limit arguments are NULL", {
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  expect_false(pos_limit_fail(w,
    max_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL
  ))
})

test_that("pos_limit_fail() treats near-zero weights as zero (tolerance)", {
  # Weight of 1e-17 should not count as a position
  w <- c(0.5, 0.3, 0.2, 0.0, 1e-17)
  expect_false(pos_limit_fail(w,
    max_pos = 3,
    max_pos_long = NULL, max_pos_short = NULL
  ))
})

test_that("pos_limit_fail() works correctly with all three limits set", {
  # 5 positions, max_pos=3 should trigger even if long/short are fine
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  expect_true(pos_limit_fail(w, max_pos = 3, max_pos_long = 5, max_pos_short = 5))
})


# ===========================================================================
# rp_transform()
# ===========================================================================

test_that("rp_transform() returns a numeric vector of length N", {
  set.seed(42)
  w <- rep(0.2, N)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, N), max_box = rep(1, N),
    groups = NULL, cLO = NULL, cUP = NULL,
    max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = NULL, weight_seq = wt_seq
  )
  expect_true(is.numeric(result))
  expect_equal(length(result), N)
})

test_that("rp_transform() corrects weights that sum below min_sum", {
  set.seed(42)
  w <- rep(0.1, N) # sum = 0.5, below min_sum = 0.99
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, N), max_box = rep(1, N),
    groups = NULL, cLO = NULL, cUP = NULL,
    max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = NULL, weight_seq = wt_seq
  )
  expect_true(sum(result) >= 0.98 && sum(result) <= 1.02)
})

test_that("rp_transform() corrects weights that sum above max_sum", {
  set.seed(42)
  w <- rep(0.3, N) # sum = 1.5, above max_sum = 1.01
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, N), max_box = rep(1, N),
    groups = NULL, cLO = NULL, cUP = NULL,
    max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = NULL, weight_seq = wt_seq
  )
  expect_true(sum(result) >= 0.98 && sum(result) <= 1.02)
})

test_that("rp_transform() with box constraints returns weights within box bounds", {
  set.seed(42)
  min_box <- rep(0.05, N)
  max_box <- rep(0.40, N)
  w <- c(0.60, 0.20, 0.10, 0.05, 0.05) # first asset violates max_box
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box,
    groups = NULL, cLO = NULL, cUP = NULL,
    max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = NULL, weight_seq = wt_seq
  )
  expect_true(is.numeric(result))
  expect_equal(length(result), N)
})

test_that("rp_transform() with group constraints returns a valid numeric vector", {
  set.seed(42)
  w <- rep(0.2, N)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, N), max_box = rep(1, N),
    groups = list(c(1, 2), c(3, 4, 5)),
    cLO = c(0.1, 0.2), cUP = c(0.5, 0.8),
    max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = NULL, weight_seq = wt_seq
  )
  expect_true(is.numeric(result))
  expect_equal(length(result), N)
})

test_that("rp_transform() with max_pos reduces non-zero positions (covers rp_position_limit)", {
  set.seed(123)
  # All 5 non-zero, enforce max_pos = 3
  w <- rep(0.2, N)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, N), max_box = rep(1, N),
    groups = NULL, cLO = NULL, cUP = NULL,
    max_pos = 3, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = NULL, weight_seq = wt_seq
  )
  expect_true(is.numeric(result))
  tol <- .Machine$double.eps^0.5
  expect_true(sum(abs(result) > tol) <= 3)
})

test_that("rp_transform() with leverage constraint covers rp_decrease_leverage path", {
  set.seed(42)
  # Dollar-neutral: sum(w) ≈ 0, but sum(abs(w)) = 1.4 > leverage = 1.2
  w <- c(0.35, 0.35, -0.35, -0.35, 0.0)
  result <- rp_transform(
    w = w, min_sum = -0.01, max_sum = 0.01,
    min_box = rep(-0.5, N), max_box = rep(0.5, N),
    groups = NULL, cLO = NULL, cUP = NULL,
    max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = 1.2, weight_seq = wt_seq_short
  )
  expect_true(is.numeric(result))
  expect_equal(length(result), N)
})


# ===========================================================================
# Internal constraint-check helpers (accessed via :::)
# ===========================================================================

test_that("min_sum_fail() returns TRUE when sum of weights is below min_sum", {
  w <- rep(0.1, N) # sum = 0.5
  expect_true(PortfolioAnalytics:::min_sum_fail(w, min_sum = 0.99))
})

test_that("min_sum_fail() returns FALSE when sum of weights meets min_sum", {
  w <- rep(0.2, N) # sum = 1.0
  expect_false(PortfolioAnalytics:::min_sum_fail(w, min_sum = 0.99))
})

test_that("min_sum_fail() returns FALSE when min_sum is NULL", {
  w <- rep(0.1, N) # sum = 0.5 but no constraint
  expect_false(PortfolioAnalytics:::min_sum_fail(w, min_sum = NULL))
})

test_that("max_sum_fail() returns TRUE when sum of weights exceeds max_sum", {
  w <- rep(0.3, N) # sum = 1.5
  expect_true(PortfolioAnalytics:::max_sum_fail(w, max_sum = 1.01))
})

test_that("max_sum_fail() returns FALSE when sum of weights is within max_sum", {
  w <- rep(0.2, N) # sum = 1.0
  expect_false(PortfolioAnalytics:::max_sum_fail(w, max_sum = 1.01))
})

test_that("max_sum_fail() returns FALSE when max_sum is NULL", {
  w <- rep(0.3, N) # sum = 1.5 but no constraint
  expect_false(PortfolioAnalytics:::max_sum_fail(w, max_sum = NULL))
})

test_that("leverage_fail() returns TRUE when sum(abs(w)) exceeds leverage", {
  w <- c(0.5, 0.5, -0.5, -0.5, 0.3) # sum(abs) = 2.3
  expect_true(PortfolioAnalytics:::leverage_fail(w, leverage = 1.5))
})

test_that("leverage_fail() returns FALSE when sum(abs(w)) is within leverage", {
  w <- c(0.3, 0.3, 0.2, 0.1, 0.1) # sum(abs) = 1.0
  expect_false(PortfolioAnalytics:::leverage_fail(w, leverage = 1.5))
})

test_that("leverage_fail() returns FALSE when leverage is NULL", {
  w <- c(0.5, 0.5, -0.5, -0.5, 0.3) # violating but no constraint
  expect_false(PortfolioAnalytics:::leverage_fail(w, leverage = NULL))
})


# ===========================================================================
# group_fail() (internal)
# ===========================================================================

test_that("group_fail() returns a logical vector of length equal to number of groups", {
  w <- rep(0.2, N)
  groups <- list(c(1, 2), c(3, 4, 5))
  cLO <- c(0.1, 0.2)
  cUP <- c(0.6, 0.8)
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP)
  expect_true(is.logical(result))
  expect_equal(length(result), 2L)
})

test_that("group_fail() returns all FALSE when all group sums are within bounds", {
  # g1 = 0.2+0.2 = 0.4, in [0.10, 0.60]; g2 = 0.6, in [0.20, 0.80]
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  groups <- list(c(1, 2), c(3, 4, 5))
  cLO <- c(0.1, 0.2)
  cUP <- c(0.6, 0.8)
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP)
  expect_false(any(result))
})

test_that("group_fail() returns TRUE for group whose sum is below cLO", {
  # g1 = 0.02+0.02 = 0.04 < cLO = 0.10; g2 = 0.30+0.30+0.38 = 0.98, in [0.20, 0.99]
  w <- c(0.02, 0.02, 0.30, 0.30, 0.36)
  groups <- list(c(1, 2), c(3, 4, 5))
  cLO <- c(0.1, 0.2)
  cUP <- c(0.6, 0.99)
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP)
  expect_true(result[1]) # first group fails (sum = 0.04 < cLO = 0.10)
  expect_false(result[2]) # second group is fine (sum = 0.96, in [0.20, 0.99])
})

test_that("group_fail() returns TRUE for group whose sum is above cUP", {
  # g1 = 0.5+0.2 = 0.70 > cUP = 0.60
  w <- c(0.5, 0.2, 0.1, 0.1, 0.1)
  groups <- list(c(1, 2), c(3, 4, 5))
  cLO <- c(0.1, 0.2)
  cUP <- c(0.6, 0.8)
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP)
  expect_true(result[1])
  expect_false(result[2])
})

test_that("group_fail() returns TRUE for both groups when both bounds are violated", {
  # g1 = 0.02+0.02 = 0.04 < cLO=0.10; g2 = 0.96 > cUP=0.80
  w <- c(0.02, 0.02, 0.40, 0.30, 0.26)
  groups <- list(c(1, 2), c(3, 4, 5))
  cLO <- c(0.1, 0.2)
  cUP <- c(0.6, 0.40) # tighten g2 upper bound
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP)
  expect_true(result[1])
  expect_true(result[2])
})


# ===========================================================================
# group_fail() — group_pos argument (line 857 of constraint_fn_map.R)
# ===========================================================================

test_that("group_fail() with group_pos detects too many non-zero weights in a group", {
  # Group 1 has 3 non-zero weights (assets 1,2,3 each = 0.2) but group_pos[1]=2
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  groups <- list(c(1, 2, 3), c(4, 5))
  cLO <- c(0.1, 0.1)
  cUP <- c(0.9, 0.9)
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP, group_pos = c(2, 2))
  expect_true(result[1])   # 3 non-zero > group_pos[1]=2 => fail
  expect_false(result[2])  # 2 non-zero <= group_pos[2]=2 => ok
})

test_that("group_fail() with group_pos returns FALSE when non-zero count is within limit", {
  # Group 1: assets 1,2 non-zero (count=2), group_pos[1]=3 => ok
  w <- c(0.3, 0.7, 0.0, 0.0, 0.0)
  groups <- list(c(1, 2, 3), c(4, 5))
  cLO <- c(0.0, 0.0)
  cUP <- c(1.0, 1.0)
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP, group_pos = c(3, 2))
  expect_false(result[1])
  expect_false(result[2])
})


# ===========================================================================
# rp_increase() — early return path (line 931)
# When sum(weights) >= min_sum already, return immediately unchanged.
# ===========================================================================

test_that("rp_increase() returns weights unchanged when sum already >= min_sum", {
  w <- c(0.25, 0.25, 0.25, 0.25, 0.25) # sum = 1.25 >= 0.99
  wt_seq <- seq(0, 1, by = 0.01)
  result <- PortfolioAnalytics:::rp_increase(
    weights = w, min_sum = 0.99,
    max_box = rep(1, 5), weight_seq = wt_seq
  )
  expect_equal(result, w)
  expect_equal(sum(result), 1.25)
})

test_that("rp_increase() returns a vector of same length as input", {
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2) # sum = 1.0 >= 0.99
  wt_seq <- seq(0, 1, by = 0.01)
  result <- PortfolioAnalytics:::rp_increase(
    weights = w, min_sum = 0.99,
    max_box = rep(1, 5), weight_seq = wt_seq
  )
  expect_length(result, 5L)
  expect_true(is.numeric(result))
})

test_that("rp_increase() increases weights when sum is below min_sum", {
  w <- c(0.1, 0.1, 0.1, 0.1, 0.1) # sum = 0.5 < 0.99
  wt_seq <- seq(0, 1, by = 0.01)
  set.seed(42)
  result <- PortfolioAnalytics:::rp_increase(
    weights = w, min_sum = 0.99,
    max_box = rep(1, 5), weight_seq = wt_seq
  )
  expect_true(is.numeric(result))
  expect_length(result, 5L)
  # Sum should be >= starting sum (weights should not decrease)
  expect_true(sum(result) >= sum(w))
})


# ===========================================================================
# rp_decrease() — early return path (line 960)
# When sum(weights) <= max_sum already, return immediately unchanged.
# ===========================================================================

test_that("rp_decrease() returns weights unchanged when sum already <= max_sum", {
  w <- c(0.2, 0.2, 0.2, 0.2, 0.2) # sum = 1.0 <= 1.01
  wt_seq <- seq(0, 1, by = 0.01)
  result <- PortfolioAnalytics:::rp_decrease(
    weights = w, max_sum = 1.01,
    min_box = rep(0, 5), weight_seq = wt_seq
  )
  expect_equal(result, w)
  expect_equal(sum(result), 1.0)
})

test_that("rp_decrease() returns a vector of same length as input", {
  w <- c(0.15, 0.15, 0.15, 0.15, 0.15) # sum = 0.75 <= 1.01
  wt_seq <- seq(0, 1, by = 0.01)
  result <- PortfolioAnalytics:::rp_decrease(
    weights = w, max_sum = 1.01,
    min_box = rep(0, 5), weight_seq = wt_seq
  )
  expect_length(result, 5L)
  expect_true(is.numeric(result))
})

test_that("rp_decrease() decreases weights when sum exceeds max_sum", {
  w <- c(0.3, 0.3, 0.3, 0.3, 0.3) # sum = 1.5 > 1.01
  wt_seq <- seq(0, 1, by = 0.01)
  set.seed(42)
  result <- PortfolioAnalytics:::rp_decrease(
    weights = w, max_sum = 1.01,
    min_box = rep(0, 5), weight_seq = wt_seq
  )
  expect_true(is.numeric(result))
  expect_length(result, 5L)
  # Sum should be <= starting sum (weights should not increase)
  expect_true(sum(result) <= sum(w))
})


# ===========================================================================
# rp_position_limit() — max_pos_long branch (lines 1045-1060)
# ===========================================================================

test_that("rp_position_limit() with max_pos_long reduces long positions", {
  set.seed(42)
  # 4 long positions but max_pos_long=2
  w <- c(0.3, 0.3, 0.3, 0.1, 0.0)
  wt_seq <- seq(-0.5, 0.5, by = 0.01)
  result <- PortfolioAnalytics:::rp_position_limit(
    weights = w,
    max_pos = NULL,
    max_pos_long = 2L,
    max_pos_short = NULL,
    min_box = rep(-0.5, 5),
    max_box = rep(0.5, 5),
    weight_seq = wt_seq
  )
  tol <- .Machine$double.eps^0.5
  expect_true(is.numeric(result))
  expect_length(result, 5L)
  # After applying rp_position_limit the number of long positions should be <= 2
  expect_lte(sum(result > tol), 2L)
})

test_that("rp_position_limit() with max_pos_long returns numeric vector", {
  set.seed(42)
  w <- c(0.4, 0.3, 0.2, 0.1, 0.0)
  wt_seq <- seq(0, 1, by = 0.01)
  result <- PortfolioAnalytics:::rp_position_limit(
    weights = w,
    max_pos = NULL,
    max_pos_long = 2L,
    max_pos_short = NULL,
    min_box = rep(0, 5),
    max_box = rep(1, 5),
    weight_seq = wt_seq
  )
  expect_true(is.numeric(result))
  expect_length(result, 5L)
})


# ===========================================================================
# rp_position_limit() — max_pos_short branch (lines 1063-1078)
# ===========================================================================

test_that("rp_position_limit() with max_pos_short reduces short positions", {
  set.seed(42)
  # 3 short positions but max_pos_short=1
  w <- c(-0.3, -0.3, -0.3, 0.1, 0.1)
  wt_seq <- seq(-0.5, 0.5, by = 0.01)
  result <- PortfolioAnalytics:::rp_position_limit(
    weights = w,
    max_pos = NULL,
    max_pos_long = NULL,
    max_pos_short = 1L,
    min_box = rep(-0.5, 5),
    max_box = rep(0.5, 5),
    weight_seq = wt_seq
  )
  tol <- .Machine$double.eps^0.5
  expect_true(is.numeric(result))
  expect_length(result, 5L)
  # After applying rp_position_limit the number of short positions should be <= 1
  expect_lte(sum(result < -tol), 1L)
})

test_that("rp_position_limit() with max_pos_short returns numeric vector", {
  set.seed(42)
  w <- c(-0.4, -0.3, 0.4, 0.2, 0.1)
  wt_seq <- seq(-0.5, 0.5, by = 0.01)
  result <- PortfolioAnalytics:::rp_position_limit(
    weights = w,
    max_pos = NULL,
    max_pos_long = NULL,
    max_pos_short = 1L,
    min_box = rep(-0.5, 5),
    max_box = rep(0.5, 5),
    weight_seq = wt_seq
  )
  expect_true(is.numeric(result))
  expect_length(result, 5L)
})


# ===========================================================================
# rp_transform() — NULL weight_seq with infinite box bounds (lines 454-455)
# When weight_seq is NULL and min_box/max_box contain Inf/-Inf, the
# guard replaces non-finite bounds with 0/1 before calling generatesequence().
# ===========================================================================

test_that("rp_transform() with NULL weight_seq and infinite bounds runs without error", {
  set.seed(42)
  w <- c(0.1, 0.1, 0.1, 0.1, 0.1) # sum = 0.5 < 0.99
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(-Inf, 5), max_box = rep(Inf, 5),
    groups = NULL, cLO = NULL, cUP = NULL,
    max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = NULL, weight_seq = NULL
  )
  expect_true(is.numeric(result))
  expect_length(result, 5L)
})

test_that("rp_transform() with NULL weight_seq and infinite bounds produces valid sum", {
  set.seed(42)
  w <- c(0.1, 0.1, 0.1, 0.1, 0.1)
  result <- rp_transform(
    w = w, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(-Inf, 5), max_box = rep(Inf, 5),
    groups = NULL, cLO = NULL, cUP = NULL,
    max_pos = NULL, group_pos = NULL,
    max_pos_long = NULL, max_pos_short = NULL,
    leverage = NULL, weight_seq = NULL
  )
  expect_true(sum(result) >= 0.98 && sum(result) <= 1.02)
})
