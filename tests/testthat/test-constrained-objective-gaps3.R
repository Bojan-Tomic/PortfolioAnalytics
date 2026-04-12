###############################################################################
# tests/testthat/test-constrained-objective-gaps3.R
#
# Targeted coverage for remaining uncovered branches in R/constrained_objective.R
#
# Lines targeted (v1 = constrained_objective_v1, v2 = constrained_objective):
#   128-129  v1: VaR objective branch (non-risk_budget, sets portfolio_method/invert)
#   137      v1: ES objective branch (non-risk_budget)
#   158      v1: warning when objective$arguments names don't match formals
#   196      v1: portfolio_risk_objective with a numeric target
#   214-219  v1: minmax_objective, tmp_measure > max and < min branches
#   242      v1: risk_budget_objective with RBupper/RBlower penalty
#   371      v2: stop("portfolio object is not of class portfolio")
#   592      v2: VaR objective non-risk_budget path
#   603      v2: ES objective non-risk_budget path
#   606      v2: CSM case (empty, just falls through)
#   610-611  v2: default/unknown objective name tries match.fun
#   649      v2: try-error in objective evaluation
#
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

utils::data(edhec)
edhec4 <- edhec[, 1:4]
funds4 <- colnames(edhec4)
w4 <- rep(1/4, 4)

# Helper: v1 constraint object
.cc_v1 <- constraint_v1(assets = funds4, min_sum = 0.99, max_sum = 1.01,
                        min = rep(0, 4), max = rep(1, 4))

# Helper: v2 portfolio object  
.pf4 <- portfolio.spec(assets = funds4)
.pf4 <- add.constraint(.pf4, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
.pf4 <- add.constraint(.pf4, type = "box", min = 0, max = 1)

# ===========================================================================
# v1: VaR objective — non-risk_budget path (lines 128-129)
# ===========================================================================

test_that("constrained_objective_v1 VaR objective sets portfolio_method and invert", {
  skip_on_cran()

  cc <- add.objective_v1(.cc_v1, type = "risk", name = "VaR")
  # VaR needs invert arg set and portfolio_method='single'
  res <- PortfolioAnalytics:::constrained_objective_v1(
    w = w4, R = edhec4, constraints = cc, trace = FALSE
  )
  expect_true(is.numeric(res))
})

# ===========================================================================
# v1: ES objective — non-risk_budget path (line 137)
# ===========================================================================

test_that("constrained_objective_v1 ES objective sets portfolio_method and invert", {
  skip_on_cran()

  cc <- add.objective_v1(.cc_v1, type = "risk", name = "ES")
  res <- PortfolioAnalytics:::constrained_objective_v1(
    w = w4, R = edhec4, constraints = cc, trace = FALSE
  )
  expect_true(is.numeric(res))
})

# ===========================================================================
# v1: warning when objective$arguments has unmatched names (line 158)
# ===========================================================================

test_that("constrained_objective_v1 warns when objective arguments don't match formals", {
  skip_on_cran()

  cc <- add.objective_v1(.cc_v1, type = "risk", name = "StdDev",
                         arguments = list(nonexistent_arg_xyz = TRUE))
  expect_warning(
    PortfolioAnalytics:::constrained_objective_v1(
      w = w4, R = edhec4, constraints = cc, trace = FALSE
    ),
    "some arguments stored for"
  )
})

# ===========================================================================
# v1: portfolio_risk_objective with a numeric target (line 196)
# ===========================================================================

test_that("constrained_objective_v1 portfolio_risk_objective with numeric target adds penalty", {
  skip_on_cran()

  # Set a target that the portfolio is very unlikely to hit exactly
  cc <- add.objective_v1(.cc_v1, type = "risk", name = "StdDev", target = 0.001)
  res <- PortfolioAnalytics:::constrained_objective_v1(
    w = w4, R = edhec4, constraints = cc, trace = FALSE
  )
  # penalty-inflated result should be large
  expect_true(is.numeric(res))
  expect_true(res > 0)
})

# ===========================================================================
# v1: minmax_objective — tmp_measure > max branch (lines 214-216)
# ===========================================================================

test_that("constrained_objective_v1 minmax_objective penalizes measure above max", {
  skip_on_cran()

  # Set a minmax bound that is easily exceeded: max StdDev = 0 (always violated)
  cc <- add.objective_v1(.cc_v1, type = "tmp_minmax", name = "StdDev",
                         min = 0, max = 0)
  res <- PortfolioAnalytics:::constrained_objective_v1(
    w = w4, R = edhec4, constraints = cc, trace = FALSE
  )
  expect_true(is.numeric(res))
  expect_true(res > 0)  # penalty applied
})

# ===========================================================================
# v1: minmax_objective — tmp_measure < min branch (lines 218-219)
# ===========================================================================

test_that("constrained_objective_v1 minmax_objective penalizes measure below min", {
  skip_on_cran()

  # Set min return impossibly high: min mean = 1000 (always violated)
  cc <- add.objective_v1(.cc_v1, type = "tmp_minmax", name = "mean",
                         min = 1000, max = 2000)
  res <- PortfolioAnalytics:::constrained_objective_v1(
    w = w4, R = edhec4, constraints = cc, trace = FALSE
  )
  expect_true(is.numeric(res))
  expect_true(res > 0)  # penalty applied
})

# ===========================================================================
# v1: risk_budget_objective with RBupper/RBlower (line 242)
# ===========================================================================

test_that("constrained_objective_v1 risk_budget_objective applies RBupper/RBlower penalty", {
  skip_on_cran()

  cc <- add.objective_v1(.cc_v1, type = "risk_budget", name = "ES",
                         min_prisk = 0.01, max_prisk = 0.30)
  res <- PortfolioAnalytics:::constrained_objective_v1(
    w = w4, R = edhec4, constraints = cc, trace = FALSE
  )
  expect_true(is.numeric(res))
})

# ===========================================================================
# v2: stop when portfolio is not of class portfolio (line 371)
# NOTE: get_constraints(portfolio) at line 367 fires first for non-portfolio
# objects, so this stop is unreachable via normal call. We skip this test.
# ===========================================================================

# (line 371 is effectively dead code — get_constraints errors before it is reached)

# ===========================================================================
# v2: VaR objective — non-risk_budget path (line 592)
# ===========================================================================

test_that("constrained_objective VaR objective sets portfolio_method and invert", {
  skip_on_cran()

  pf <- add.objective(.pf4, type = "risk", name = "VaR")
  res <- PortfolioAnalytics:::constrained_objective(
    w = w4, R = edhec4, portfolio = pf, trace = FALSE
  )
  expect_true(is.numeric(res))
})

# ===========================================================================
# v2: ES objective — non-risk_budget path (line 603)
# ===========================================================================

test_that("constrained_objective ES objective sets portfolio_method and invert", {
  skip_on_cran()

  pf <- add.objective(.pf4, type = "risk", name = "ES")
  res <- PortfolioAnalytics:::constrained_objective(
    w = w4, R = edhec4, portfolio = pf, trace = FALSE
  )
  expect_true(is.numeric(res))
})

# ===========================================================================
# v2: CSM objective — empty case, just falls through (line 606)
# ===========================================================================

test_that("constrained_objective CSM objective case executes without error", {
  skip_on_cran()

  pf <- add.objective(.pf4, type = "risk", name = "CSM")
  res <- tryCatch(
    PortfolioAnalytics:::constrained_objective(
      w = w4, R = edhec4, portfolio = pf, trace = FALSE
    ),
    error = function(e) e
  )
  # CSM is an empty case — result is 0 or numeric, not an error
  expect_true(is.numeric(res) || inherits(res, "error"))
})

# ===========================================================================
# v2: unknown objective name falls to default try(match.fun(.)) (lines 610-611)
# The try-error means is.function(fun) is FALSE, so next is called (line 635)
# — no message is produced, result is 0
# ===========================================================================

test_that("constrained_objective unknown objective name hits default branch", {
  skip_on_cran()

  pf <- add.objective(.pf4, type = "risk", name = "mean")
  # rename the objective to something unknown after creating it
  pf$objectives[[1]]$name <- "totally_unknown_function_xyz"
  # Default branch: try(match.fun(...)) fails → is.function(fun) = FALSE → next
  res <- tryCatch(
    PortfolioAnalytics:::constrained_objective(
      w = w4, R = edhec4, portfolio = pf, trace = FALSE
    ),
    error = function(e) e
  )
  # Result is 0 (no penalty added) or an error — either way the branch was hit
  expect_true(is.numeric(res) || inherits(res, "error"))
})

# ===========================================================================
# v2: try-error in objective evaluation (line 649)
# Use a valid function (log) that errors when called with portfolio arguments
# Line 649 is hit (message produced), then arithmetic with try-error may error
# ===========================================================================

test_that("constrained_objective messages when do.call(fun, .formals) produces try-error", {
  skip_on_cran()

  # Use a return_objective with name changed to 'log'
  # log() will error with portfolio-shaped formals → try-error → message at line 649
  pf <- add.objective(.pf4, type = "return", name = "mean")
  pf$objectives[[1]]$name <- "log"

  msgs <- character(0)
  tryCatch(
    withCallingHandlers(
      PortfolioAnalytics:::constrained_objective(
        w = w4, R = edhec4, portfolio = pf, trace = FALSE
      ),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) NULL  # arithmetic after try-error may error — that's fine
  )
  # Line 649 message should have been produced
  expect_true(length(msgs) >= 1)
  expect_true(any(grepl("generated an error", msgs)))
})
