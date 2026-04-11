###############################################################################
# tests/testthat/test-meucci-ranking.R
#
# Source files covered:
#   R/meucci_moments.R — meucci.moments()
#   R/meucci_ranking.R — meucci.ranking()
#   R/EntropyProg.R    — EntropyProg()
#   R/ac_ranking.R     — ac.ranking(), centroid.complete.mc(),
#                        centroid.sectors(), centroid.sign(),
#                        centroid.buckets()
#
# Tests the Fully Flexible Views / Almgren-Chriss ranking framework:
#   - meucci.moments() with a flat prior reproduces the column means
#   - meucci.ranking() shifts the posterior moments away from the flat prior
#   - EntropyProg() converges and returns probabilities that sum to 1
#   - ac.ranking() scales the centroid vector to [-0.05, 0.05] and respects
#     the rank order (highest-ranked asset gets the largest value)
#   - centroid helpers (complete.mc, sectors, sign) return vectors of the
#     correct length and assign centroids in the correct rank direction
#
# IMPORTANT: centroid.complete.mc, centroid.sectors, and centroid.sign all
# rely on Monte Carlo simulation.  set.seed() is called *inside* each
# test_that() block that invokes one of these functions so the result is
# reproducible but isolated.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("nloptr")

library(nloptr)

# ---------------------------------------------------------------------------
# Shared fixtures (file scope)
# ---------------------------------------------------------------------------

R   <- edhec4     # 4-asset subset — keeps EntropyProg cheap to run
nms <- funds4
N   <- ncol(R)    # 4
m   <- nrow(R)    # number of scenarios / observations

# Flat (uniform) prior: all scenarios equally likely
p_prior <- rep(1 / m, m)

# Ranking view: E[R_1] < E[R_2] < E[R_3] < E[R_4]
# i.e. asset 1 has the lowest expected return, asset 4 the highest
order_view <- c(1L, 2L, 3L, 4L)

# Posterior moments using a flat (equal-weight) prior
# With uniform weights this should reproduce the sample column means
moments_flat <- meucci.moments(R, p_prior)

# Posterior moments after applying the ranking view via EntropyProg
# (meucci.ranking calls EntropyProg internally)
moments_ranking <- meucci.ranking(R, p_prior, order_view)

# ac.ranking is a deterministic analytical approximation — no seed required
ac_out <- ac.ranking(R, order_view)

# Small EntropyProg setup — J = 20 scenarios, equality constraint only
# The single constraint is that the revised probabilities must sum to 1
ep_J      <- 20L
p_small   <- rep(1 / ep_J, ep_J)
ep_Aeq    <- matrix(rep(1, ep_J), ncol = ep_J)   # 1 × J matrix: sum(p) = 1
ep_beq    <- matrix(1, 1)
ep_result <- EntropyProg(p = p_small, Aeq = ep_Aeq, beq = ep_beq)

# ---------------------------------------------------------------------------
# meucci.moments — flat prior
# ---------------------------------------------------------------------------

test_that("meucci.moments: mu has length N (4)", {
  expect_equal(length(moments_flat$mu), N)
})

test_that("meucci.moments: sigma is an N x N matrix", {
  expect_true(is.matrix(moments_flat$sigma))
  expect_equal(dim(moments_flat$sigma), c(N, N))
})

test_that("meucci.moments with uniform prior: mu equals colMeans(R) within 1e-10", {
  # Weighted mean with equal weights 1/m is algebraically identical to colMeans
  expect_equal(as.numeric(moments_flat$mu),
               as.numeric(colMeans(R)),
               tolerance = 1e-10)
})

test_that("meucci.moments: sigma is symmetric within 1e-10", {
  max_asym <- max(abs(moments_flat$sigma - t(moments_flat$sigma)))
  expect_true(max_asym < 1e-10)
})

# ---------------------------------------------------------------------------
# meucci.ranking — posterior moments after the ranking view
# ---------------------------------------------------------------------------

test_that("meucci.ranking returns a list containing mu and sigma", {
  expect_true(is.list(moments_ranking))
  expect_true("mu"    %in% names(moments_ranking))
  expect_true("sigma" %in% names(moments_ranking))
})

test_that("meucci.ranking: mu has length N (4)", {
  expect_equal(length(moments_ranking$mu), N)
})

test_that("meucci.ranking: sigma is an N x N matrix", {
  expect_true(is.matrix(moments_ranking$sigma))
  expect_equal(dim(moments_ranking$sigma), c(N, N))
})

test_that("meucci.ranking: sigma is symmetric within 1e-10", {
  max_asym <- max(abs(moments_ranking$sigma - t(moments_ranking$sigma)))
  expect_true(max_asym < 1e-10)
})

test_that("meucci.ranking: posterior mu differs from the flat-prior mu", {
  # Applying a non-trivial ranking view must change the mean vector
  expect_false(isTRUE(all.equal(as.numeric(moments_ranking$mu),
                                as.numeric(moments_flat$mu))))
})

test_that("meucci.ranking: posterior mu respects the rank ordering", {
  # order_view = c(1,2,3,4): E[R_1] <= E[R_2] <= E[R_3] <= E[R_4]
  # Allow a tiny numerical slack from the optimiser
  mu <- as.numeric(moments_ranking$mu)
  expect_true(all(diff(mu[order_view]) >= -1e-6))
})

test_that("meucci.ranking: sigma is positive semi-definite (eigenvalues >= 0)", {
  ev <- eigen(moments_ranking$sigma, symmetric = TRUE, only.values = TRUE)$values
  expect_true(all(ev >= -1e-10))
})

# ---------------------------------------------------------------------------
# EntropyProg — equality-only: probabilities sum to 1
# ---------------------------------------------------------------------------

test_that("EntropyProg returns a list with p_ and optimizationPerformance", {
  expect_true(is.list(ep_result))
  expect_true("p_"                      %in% names(ep_result))
  expect_true("optimizationPerformance" %in% names(ep_result))
})

test_that("EntropyProg: p_ is numeric with length J (20)", {
  expect_true(is.numeric(ep_result$p_))
  expect_equal(length(ep_result$p_), ep_J)
})

test_that("EntropyProg: revised probabilities sum to 1 within 0.001", {
  expect_equal(sum(ep_result$p_), 1, tolerance = 0.001)
})

test_that("EntropyProg: optimizationPerformance$converged is TRUE", {
  expect_true(ep_result$optimizationPerformance$converged)
})

test_that("EntropyProg: all revised probabilities are non-negative", {
  expect_true(all(ep_result$p_ >= 0))
})

# ---------------------------------------------------------------------------
# ac.ranking — analytical centroid, scaled to [-0.05, 0.05]
# ---------------------------------------------------------------------------

test_that("ac.ranking returns a numeric vector of length N (4)", {
  expect_true(is.numeric(ac_out))
  expect_equal(length(ac_out), N)
})

test_that("ac.ranking: all values lie in the range [-0.05, 0.05]", {
  # scale_range() always maps extremes to exactly ±0.05
  expect_true(min(ac_out) >= -0.05 - 1e-10)
  expect_true(max(ac_out) <=  0.05 + 1e-10)
})

test_that("ac.ranking: highest-ranked asset (index 4) receives the largest centroid", {
  # order_view[4] = 4 is the asset with the highest expected return
  expect_equal(which.max(ac_out), 4L)
})

test_that("ac.ranking: lowest-ranked asset (index 1) receives the smallest centroid", {
  # order_view[1] = 1 is the asset with the lowest expected return
  expect_equal(which.min(ac_out), 1L)
})

test_that("ac.ranking: centroid values strictly increase with rank", {
  # For order_view = c(1,2,3,4) we expect ac_out[1] < ac_out[2] < ac_out[3] < ac_out[4]
  expect_true(all(diff(ac_out[order_view]) > 0))
})

test_that("ac.ranking errors when length(order) != ncol(R)", {
  expect_error(ac.ranking(R, 1:3))
})

# ---------------------------------------------------------------------------
# centroid.complete.mc — Monte Carlo, seed set inside each block
# ---------------------------------------------------------------------------

test_that("centroid.complete.mc returns a numeric vector of correct length", {
  set.seed(1)
  c_out <- centroid.complete.mc(c(2L, 1L, 3L, 4L))
  expect_true(is.numeric(c_out))
  expect_equal(length(c_out), 4L)
})

test_that("centroid.complete.mc: highest-ranked asset gets the largest centroid", {
  # order = c(2,1,3,4) means R_2 < R_1 < R_3 < R_4; asset 4 has rank 4
  set.seed(1)
  c_out <- centroid.complete.mc(c(2L, 1L, 3L, 4L))
  expect_equal(which.max(c_out), 4L)
})

test_that("centroid.complete.mc: lowest-ranked asset gets the smallest centroid", {
  # order = c(2,1,3,4): asset 2 has rank 1 (lowest)
  set.seed(1)
  c_out <- centroid.complete.mc(c(2L, 1L, 3L, 4L))
  expect_equal(which.min(c_out), 2L)
})

test_that("centroid.complete.mc: centroid values are non-increasing along rev(order)", {
  # colMeans of sorted-descending draws are themselves decreasing;
  # out[rev(order)] captures them in that decreasing order
  set.seed(42)
  order_cc <- c(2L, 1L, 3L, 4L)
  c_out    <- centroid.complete.mc(order_cc)
  expect_true(all(diff(c_out[rev(order_cc)]) <= 0))
})

# ---------------------------------------------------------------------------
# centroid.sectors — Monte Carlo, seed set inside each block
# ---------------------------------------------------------------------------

test_that("centroid.sectors returns a numeric vector of length 5", {
  # Sector 1 view: R_2 < R_1 < R_3  (3 assets, indices 1-3)
  # Sector 2 view: R_5 < R_4        (2 assets, indices 4-5)
  set.seed(1)
  cs <- centroid.sectors(list(c(2L, 1L, 3L), c(5L, 4L)))
  expect_true(is.numeric(cs))
  expect_equal(length(cs), 5L)
})

test_that("centroid.sectors: highest-ranked asset in each sector gets the largest value", {
  # Sector 1: R_3 is the top-ranked asset  → cs[3] > cs[1] > cs[2]
  # Sector 2: R_4 is the top-ranked asset  → cs[4] > cs[5]
  set.seed(1)
  cs <- centroid.sectors(list(c(2L, 1L, 3L), c(5L, 4L)))
  expect_true(cs[3] > cs[1])
  expect_true(cs[1] > cs[2])
  expect_true(cs[4] > cs[5])
})

test_that("centroid.sectors errors when sectors argument is not a list", {
  expect_error(centroid.sectors(c(1L, 2L, 3L)))
})

# ---------------------------------------------------------------------------
# centroid.sign — Monte Carlo, seed set inside each block
# ---------------------------------------------------------------------------

test_that("centroid.sign returns a numeric vector of length 4", {
  # positive = assets 3,4 (positive expected return, ascending order)
  # negative = assets 1,2 (negative expected return, ascending order)
  set.seed(1)
  csign <- centroid.sign(positive = c(3L, 4L), negative = c(1L, 2L))
  expect_true(is.numeric(csign))
  expect_equal(length(csign), 4L)
})

test_that("centroid.sign: positive assets (3 and 4) receive positive centroid values", {
  set.seed(1)
  csign <- centroid.sign(positive = c(3L, 4L), negative = c(1L, 2L))
  expect_true(all(csign[c(3L, 4L)] > 0))
})

test_that("centroid.sign: negative assets (1 and 2) receive negative centroid values", {
  set.seed(1)
  csign <- centroid.sign(positive = c(3L, 4L), negative = c(1L, 2L))
  expect_true(all(csign[c(1L, 2L)] < 0))
})

# ---------------------------------------------------------------------------
# Integration: moments from meucci.ranking are dimension-compatible with
# a portfolio specification for downstream optimization use
# ---------------------------------------------------------------------------

test_that("meucci.ranking moments have correct dimensions for optimization use", {
  portf_ret <- make_maxret_portf(R)
  # moments_ranking was computed at file scope; verify it matches the portfolio
  expect_equal(length(moments_ranking$mu),  N)
  expect_equal(dim(moments_ranking$sigma),  c(N, N))
  expect_equal(N, length(portf_ret$assets))
})

test_that("meucci.ranking sigma diagonal entries are non-negative (valid variances)", {
  expect_true(all(diag(moments_ranking$sigma) >= 0))
})
