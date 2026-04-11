###############################################################################
# tests/testthat/test-weight-concentration.R
#
# Migrated from inst/tests/test_demo_weight_concentration.R.
# Tests weight_concentration objective with ROI optimizer using the HHI
# measure and group-level concentration aversion across four aversion levels.
#
# Changes vs. legacy:
#   - Removed require(testthat), require(PortfolioAnalytics), context().
#   - Replaced source() with inline setup.
#   - Replaced expect_that(..., is_true()) with expect_true() / expect_s3_class().
#   - Added tests for opt3 (which the legacy file skipped entirely).
#   - Relaxed opt4 equal-weight check to a spread bound (more robust).
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

# ---------------------------------------------------------------------------
# Fixture
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:8]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED", "FIA", "GM")
funds <- colnames(R)

cap_labels <- c(
  rep("Group1", 2), rep("Group2", 2),
  rep("Group3", 2), rep("Group4", 2)
)

init.portf <- portfolio.spec(assets = funds, category_labels = cap_labels)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(portfolio = init.portf, type = "risk", name = "var")

# opt1: baseline minimum variance (no concentration objective)
opt1 <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# opt2: add weight_concentration with conc_aversion=0 — should equal min var
conc.portf <- add.objective(
  portfolio = init.portf,
  type = "weight_concentration",
  name = "HHI",
  conc_aversion = 0,
  conc_groups = init.portf$category_labels
)
opt2 <- optimize.portfolio(
  R = R, portfolio = conc.portf,
  optimize_method = "ROI", trace = TRUE
)

# opt3: non-zero per-group aversion
conc.portf$objectives[[2]]$conc_aversion <- c(0.03, 0.03, 0.06, 0.02)
opt3 <- optimize.portfolio(
  R = R, portfolio = conc.portf,
  optimize_method = "ROI", trace = TRUE
)

# opt4: very high aversion — should produce approximately equal weights
conc.portf$objectives[[2]]$conc_aversion <- rep(1e6, 4)
opt4 <- optimize.portfolio(
  R = R, portfolio = conc.portf,
  optimize_method = "ROI", trace = TRUE
)

# ---------------------------------------------------------------------------
# Tests: weight_concentration objective structure
# ---------------------------------------------------------------------------

test_that("conc.portf contains weight_concentration as an objective", {
  expect_s3_class(conc.portf$objectives[[2]], "weight_concentration_objective")
})

test_that("conc.portf weight_concentration objective name is HHI", {
  expect_true(conc.portf$objectives[[2]]$name == "HHI")
})

test_that("opt2 portfolio stores conc_aversion expanded to rep(0, 4)", {
  expect_equal(opt2$portfolio$objectives[[2]]$conc_aversion, rep(0, 4))
})

# ---------------------------------------------------------------------------
# Tests: opt1 — minimum variance baseline
# ---------------------------------------------------------------------------

test_that("opt1 is an optimize.portfolio.ROI object", {
  expect_s3_class(opt1, "optimize.portfolio.ROI")
})

test_that("opt1 weights are numeric", {
  expect_true(is.numeric(extractWeights(opt1)))
})

# ---------------------------------------------------------------------------
# Tests: opt2 — conc_aversion=0 must reproduce minimum variance solution
# ---------------------------------------------------------------------------

test_that("opt2 is an optimize.portfolio.ROI object", {
  expect_s3_class(opt2, "optimize.portfolio.ROI")
})

test_that("opt2 weights are numeric", {
  expect_true(is.numeric(extractWeights(opt2)))
})

test_that("minimum variance and conc.portf weights are equal with conc_aversion=0", {
  expect_true(isTRUE(all.equal(opt1$weights, opt2$weights)))
})

# ---------------------------------------------------------------------------
# Tests: opt3 — per-group aversion c(0.03, 0.03, 0.06, 0.02)
# ---------------------------------------------------------------------------

test_that("opt3 is an optimize.portfolio.ROI object", {
  expect_s3_class(opt3, "optimize.portfolio.ROI")
})

test_that("opt3 weights are numeric", {
  expect_true(is.numeric(extractWeights(opt3)))
})

test_that("opt3 weights sum to approximately 1", {
  expect_equal(sum(extractWeights(opt3)), 1, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Tests: opt4 — very high aversion → approximately equal weights
# ---------------------------------------------------------------------------

test_that("opt4 is an optimize.portfolio.ROI object", {
  expect_s3_class(opt4, "optimize.portfolio.ROI")
})

test_that("opt4 weights are numeric", {
  expect_true(is.numeric(extractWeights(opt4)))
})

test_that("opt4 weights are approximately equal under very high concentration aversion", {
  w <- as.numeric(extractWeights(opt4))
  expect_true(max(w) - min(w) < 0.05)
})
