###############################################################################
# tests/testthat/test-factor-exposure.R
#
# Tests for factor exposure constraints in optimize.portfolio(), covering the
# constraint construction, ROI solvers, equivalence with group constraints,
# and optional DEoptim solver path.
#
# Source files covered:
#   R/optimize.portfolio.R  — factor exposure constraint path (ROI and DEoptim)
#   R/constraint_fn_map.R   — factor exposure constraint handling in fn_map
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("ROI.plugin.glpk")

library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

# ---------------------------------------------------------------------------
# File-scope setup — built once for the whole file
# ---------------------------------------------------------------------------

R   <- edhec4   # 4-asset data, matching the demo
nms <- funds4
N   <- 4L

# Portfolio spec object — no constraints added via add.constraint; we use
# the v2 external constraint list approach from demo/demo_factor_exposure.R
pspec <- portfolio.spec(assets = nms)

# v2 constraint objects
lev_constr <- weight_sum_constraint(min_sum = 1, max_sum = 1)
lo_constr  <- box_constraint(assets = pspec$assets, min = c(0.01, 0.02, 0.03, 0.04), max = 0.65)

# B matrix: industry factor model (3 "factors" for 4 assets)
# Asset 1+2 load on factorA, asset 3 on factorB, asset 4 on factorC.
# This is structurally identical to the group constraint below, which
# lets us confirm the two approaches produce the same solution.
B <- cbind(c(1, 1, 0, 0),
           c(0, 0, 1, 0),
           c(0, 0, 0, 1))
rownames(B) <- nms
colnames(B) <- c("factorA", "factorB", "factorC")
lower <- c(0.1, 0.1, 0.1)
upper <- c(0.4, 0.4, 0.4)

# Equivalent group constraint (groups mirror the B matrix above)
grp_constr <- group_constraint(assets = pspec$assets, groups = list(1:2, 3, 4),
                               group_min = 0.1, group_max = 0.4)

# Factor exposure constraint
exp_constr <- factor_exposure_constraint(assets = pspec$assets, B = B,
                                         lower = lower, upper = upper)

# Objectives
var_obj <- portfolio_risk_objective(name = "var")
ret_obj <- return_objective(name = "mean")

# ROI min-variance: group constraint and factor exposure constraint
opt_group_var <- optimize.portfolio(R = R, portfolio = pspec,
                                    constraints = list(lev_constr, lo_constr, grp_constr),
                                    objectives  = list(var_obj),
                                    optimize_method = "ROI")

opt_fe_var <- optimize.portfolio(R = R, portfolio = pspec,
                                 constraints = list(lev_constr, lo_constr, exp_constr),
                                 objectives  = list(var_obj),
                                 optimize_method = "ROI")

# ROI max-return: group constraint and factor exposure constraint
opt_group_ret <- optimize.portfolio(R = R, portfolio = pspec,
                                    constraints = list(lev_constr, lo_constr, grp_constr),
                                    objectives  = list(ret_obj),
                                    optimize_method = "ROI")

opt_fe_ret <- optimize.portfolio(R = R, portfolio = pspec,
                                 constraints = list(lev_constr, lo_constr, exp_constr),
                                 objectives  = list(ret_obj),
                                 optimize_method = "ROI")

# DEoptim setup — optional; only loaded when the package is available
opt_de_fe <- NULL
if (requireNamespace("DEoptim", quietly = TRUE)) {
  library(DEoptim)
  set.seed(123)
  opt_de_fe <- tryCatch(
    optimize.portfolio(R = R, portfolio = pspec,
                       constraints = list(lev_constr, lo_constr, exp_constr),
                       objectives  = list(ret_obj),
                       optimize_method = "DEoptim",
                       search_size = 1000),
    error = function(e) NULL
  )
}


# ===========================================================================
# Factor exposure constraint structure
# ===========================================================================

test_that("exp_constr has class 'factor_exposure_constraint'", {
  expect_s3_class(exp_constr, "factor_exposure_constraint")
})

test_that("exp_constr$B equals the B matrix we defined", {
  expect_equal(exp_constr$B, B)
})

test_that("exp_constr$lower equals c(0.1, 0.1, 0.1)", {
  expect_equal(exp_constr$lower, c(0.1, 0.1, 0.1))
})

test_that("exp_constr$upper equals c(0.4, 0.4, 0.4)", {
  expect_equal(exp_constr$upper, c(0.4, 0.4, 0.4))
})


# ===========================================================================
# ROI min-variance — factor exposure results
# ===========================================================================

test_that("opt_fe_var has class 'optimize.portfolio.ROI'", {
  expect_s3_class(opt_fe_var, "optimize.portfolio.ROI")
})

test_that("extractWeights(opt_fe_var) is a named numeric vector of length 4", {
  w <- extractWeights(opt_fe_var)
  expect_true(is.numeric(w))
  expect_equal(length(w), N)
  expect_false(is.null(names(w)))
})

test_that("opt_fe_var weights are non-negative", {
  w <- extractWeights(opt_fe_var)
  expect_true(all(w >= -TOL_OPT))
})

test_that("opt_fe_var weights sum to approximately 1", {
  w <- extractWeights(opt_fe_var)
  expect_equal(sum(w), 1, tolerance = TOL_OPT)
})

test_that("opt_fe_var satisfies factor exposure lower bounds", {
  w        <- extractWeights(opt_fe_var)
  exposure <- as.numeric(t(B) %*% w)
  expect_true(all(exposure >= lower - TOL_OPT))
})

test_that("opt_fe_var satisfies factor exposure upper bounds", {
  w        <- extractWeights(opt_fe_var)
  exposure <- as.numeric(t(B) %*% w)
  expect_true(all(exposure <= upper + TOL_OPT))
})


# ===========================================================================
# Group constraint vs factor exposure equivalence — min variance
# ===========================================================================

test_that("group and factor-exposure constraints yield the same min-var weights", {
  expect_true(isTRUE(all.equal(
    extractWeights(opt_group_var),
    extractWeights(opt_fe_var),
    tolerance = TOL_WSUM
  )))
})


# ===========================================================================
# ROI max-return — factor exposure results
# ===========================================================================

test_that("opt_fe_ret has class 'optimize.portfolio.ROI'", {
  expect_s3_class(opt_fe_ret, "optimize.portfolio.ROI")
})

test_that("opt_fe_ret satisfies factor exposure lower bounds", {
  w        <- extractWeights(opt_fe_ret)
  exposure <- as.numeric(t(B) %*% w)
  expect_true(all(exposure >= lower - TOL_OPT))
})

test_that("opt_fe_ret satisfies factor exposure upper bounds", {
  w        <- extractWeights(opt_fe_ret)
  exposure <- as.numeric(t(B) %*% w)
  expect_true(all(exposure <= upper + TOL_OPT))
})


# ===========================================================================
# Group constraint vs factor exposure equivalence — max return
# ===========================================================================

test_that("group and factor-exposure constraints yield the same max-ret weights", {
  expect_true(isTRUE(all.equal(
    extractWeights(opt_group_ret),
    extractWeights(opt_fe_ret),
    tolerance = TOL_WSUM
  )))
})


# ===========================================================================
# DEoptim with factor exposure (optional — may be slow)
# ===========================================================================

test_that("DEoptim with factor exposure constraint succeeds (result is not NULL)", {
  skip_if_not_installed("DEoptim")
  expect_false(is.null(opt_de_fe))
})

test_that("DEoptim result has class 'optimize.portfolio.DEoptim'", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_fe), "DEoptim optimization did not produce a result")
  expect_s3_class(opt_de_fe, "optimize.portfolio.DEoptim")
})

test_that("DEoptim extractWeights returns a numeric vector", {
  skip_if_not_installed("DEoptim")
  skip_if(is.null(opt_de_fe), "DEoptim optimization did not produce a result")
  expect_true(is.numeric(extractWeights(opt_de_fe)))
})
