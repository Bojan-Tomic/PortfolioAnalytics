###############################################################################
# tests/testthat/test-extract-risk.R
#
# Source files hit: R/extractrisk.R
#
# Functions covered:
#   extract_risk()   — basic call, moment_setting, alpha > 0.5 auto-flip,
#                      explicit mu/sigma path
#
# Requires CVXR and the CLARABEL solver backend.  All tests are guarded with
# skip_if_not_installed() so the suite degrades gracefully when either
# package is absent.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("CVXR")
skip_if_not_installed("CLARABEL")

library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

set.seed(42)
N  <- 4L
TT <- 60L
R  <- matrix(rnorm(TT * N, mean = 5e-3, sd = 2e-2), nrow = TT, ncol = N)
colnames(R) <- paste0("A", seq_len(N))

# Equal-weight portfolio
w_eq <- rep(1 / N, N)
names(w_eq) <- colnames(R)

# ===========================================================================
# 1. Basic call — default alphas, moment_setting = NULL
# ===========================================================================

res_default <- extract_risk(R, w_eq)

test_that("extract_risk default: returns a list", {
  expect_true(is.list(res_default))
})

test_that("extract_risk default: has mean, StdDev, ES, CSM, EQS elements", {
  expect_true(!is.null(res_default$mean),   label = "mean present")
  expect_true(!is.null(res_default$StdDev), label = "StdDev present")
  expect_true(!is.null(res_default$ES),     label = "ES present")
  expect_true(!is.null(res_default$CSM),    label = "CSM present")
  expect_true(!is.null(res_default$EQS),    label = "EQS present")
})

test_that("extract_risk default: mean is a finite scalar", {
  expect_equal(length(as.numeric(res_default$mean)), 1L)
  expect_true(is.finite(as.numeric(res_default$mean)))
})

test_that("extract_risk default: StdDev is a positive finite scalar", {
  sd_val <- as.numeric(res_default$StdDev)
  expect_equal(length(sd_val), 1L)
  expect_true(is.finite(sd_val))
  expect_true(sd_val > 0)
})

test_that("extract_risk default: mean matches manual portfolio return", {
  manual_mean <- mean(R %*% w_eq)
  expect_equal(as.numeric(res_default$mean), manual_mean, tolerance = 1e-10)
})

test_that("extract_risk default: StdDev matches manual portfolio StdDev", {
  manual_sd <- sqrt(as.numeric(t(w_eq) %*% cov(R) %*% w_eq))
  expect_equal(as.numeric(res_default$StdDev), manual_sd, tolerance = 1e-10)
})


# ===========================================================================
# 2. moment_setting provided (custom mu and sigma)
# ===========================================================================

custom_mu    <- colMeans(R) * 2   # deliberately scaled to verify path
custom_sigma <- cov(R) * 1.5

res_custom <- extract_risk(R, w_eq,
                           moment_setting = list(mu = custom_mu,
                                                 sigma = custom_sigma))

test_that("extract_risk custom moments: mean uses moment_setting$mu", {
  expected_mean <- as.numeric(custom_mu %*% w_eq)
  expect_equal(as.numeric(res_custom$mean), expected_mean, tolerance = 1e-10)
})

test_that("extract_risk custom moments: StdDev uses moment_setting$sigma", {
  expected_sd <- sqrt(as.numeric(t(w_eq) %*% custom_sigma %*% w_eq))
  expect_equal(as.numeric(res_custom$StdDev), expected_sd, tolerance = 1e-10)
})

test_that("extract_risk custom moments: custom mean differs from sample mean", {
  expect_false(isTRUE(all.equal(
    as.numeric(res_custom$mean),
    as.numeric(res_default$mean)
  )))
})


# ===========================================================================
# 3. Alpha > 0.5 branch — auto-flip to (1 - alpha)
# ===========================================================================

# The function silently replaces alpha > 0.5 with (1 - alpha), so
# extract_risk(R, w, alpha=0.95) should give the same result as
# extract_risk(R, w, alpha=0.05).

res_high_alpha <- extract_risk(R, w_eq,
                               ES_alpha  = 0.95,
                               CSM_alpha = 0.95,
                               EQS_alpha = 0.95)

test_that("extract_risk alpha>0.5: mean identical to default (alpha-flip doesn't affect mean)", {
  expect_equal(as.numeric(res_high_alpha$mean),
               as.numeric(res_default$mean), tolerance = 1e-10)
})

test_that("extract_risk alpha>0.5: StdDev identical to default (alpha-flip doesn't affect StdDev)", {
  expect_equal(as.numeric(res_high_alpha$StdDev),
               as.numeric(res_default$StdDev), tolerance = 1e-10)
})


# ===========================================================================
# 4. Partial moment_setting (only sigma provided, mu is NULL)
# ===========================================================================

res_sigma_only <- extract_risk(R, w_eq,
                               moment_setting = list(sigma = custom_sigma))

test_that("extract_risk sigma-only moment_setting: mean uses sample mean (mu=NULL)", {
  expected_mean <- mean(R %*% w_eq)
  expect_equal(as.numeric(res_sigma_only$mean), expected_mean, tolerance = 1e-10)
})

test_that("extract_risk sigma-only moment_setting: StdDev uses custom sigma", {
  expected_sd <- sqrt(as.numeric(t(w_eq) %*% custom_sigma %*% w_eq))
  expect_equal(as.numeric(res_sigma_only$StdDev), expected_sd, tolerance = 1e-10)
})
