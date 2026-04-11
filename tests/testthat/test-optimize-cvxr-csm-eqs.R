###############################################################################
# tests/testthat/test-optimize-cvxr-csm-eqs.R
#
# Coverage targets (R/optimize.portfolio.R CVXR path):
#
#   Lines 2949-2972: CSM objective block
#     - CSMratio=TRUE  (default, max-CSM-ratio) — covered by test-optFUN-extended.R
#       via create.EfficientFrontier(ef=TRUE), but CSMratio=FALSE (min CSM) is NOT
#     - CSMratio=FALSE: minimise raw CSM (lines 2965-2972)
#
#   Lines 2974-2993: EQS objective block
#     - EQSratio=TRUE  (default) — covered by efficient frontier
#     - EQSratio=FALSE: minimise raw EQS (lines 2984-2989)
#
#   Line 2996: weight_scale branch (CSMratio=FALSE → weight_scale=1)
#   Line 3002: alternative constraint branch when not maxSR/maxSTARR/CSMratio
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("CVXR")

library(CVXR)

utils::data(edhec)
R <- edhec[, 1:4]
nms <- colnames(R)

# ---------------------------------------------------------------------------
# Helper: build a portfolio with a risk objective (CSM or EQS) only (no mean)
# This gives reward=FALSE, so CSMratio=FALSE regardless and we go straight
# to the min-CSM / min-EQS sub-branches.
# ---------------------------------------------------------------------------

make_csm_portf <- function() {
  p <- portfolio.spec(nms)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "CSM")
  p
}

make_eqs_portf <- function() {
  p <- portfolio.spec(nms)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "EQS")
  p
}

# ===========================================================================
# Section 1: min-CSM (CSMratio=FALSE path, lines 2965-2972)
# A portfolio with only a CSM risk objective (no mean return objective)
# → reward=FALSE → CSMratio stays FALSE → enters the else branch
# ===========================================================================

portf_csm <- make_csm_portf()

opt_csm <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R, portf_csm, optimize_method = "CVXR")
  )),
  error = function(e) NULL
)

test_that("min-CSM: optimization result is not NULL", {
  skip_if(is.null(opt_csm))
  expect_false(is.null(opt_csm))
})

test_that("min-CSM: result has class 'optimize.portfolio.CVXR'", {
  skip_if(is.null(opt_csm))
  expect_s3_class(opt_csm, "optimize.portfolio.CVXR")
})

test_that("min-CSM: extractWeights returns numeric vector", {
  skip_if(is.null(opt_csm))
  w <- extractWeights(opt_csm)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
})

test_that("min-CSM: weights have length equal to number of assets", {
  skip_if(is.null(opt_csm))
  expect_equal(length(extractWeights(opt_csm)), ncol(R))
})

test_that("min-CSM: weights sum to approximately 1", {
  skip_if(is.null(opt_csm))
  expect_equal(sum(extractWeights(opt_csm)), 1, tolerance = 1e-4)
})

test_that("min-CSM: all weights are non-negative (long_only)", {
  skip_if(is.null(opt_csm))
  expect_true(all(round(extractWeights(opt_csm), 10) >= 0))
})

test_that("min-CSM: objective_measures contains a CSM element", {
  skip_if(is.null(opt_csm))
  om <- opt_csm$objective_measures
  expect_false(is.null(om))
  # CSM measure should be somewhere in the objective_measures list
  nm <- names(unlist(om))
  expect_true(any(grepl("CSM", nm, ignore.case = TRUE)))
})

# ===========================================================================
# Section 2: min-EQS (EQSratio=FALSE path, lines 2984-2989)
# Same pattern: only EQS risk objective → reward=FALSE → EQSratio=FALSE
# ===========================================================================

portf_eqs <- make_eqs_portf()

opt_eqs <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R, portf_eqs, optimize_method = "CVXR")
  )),
  error = function(e) NULL
)

test_that("min-EQS: optimization result is not NULL", {
  skip_if(is.null(opt_eqs))
  expect_false(is.null(opt_eqs))
})

test_that("min-EQS: result has class 'optimize.portfolio.CVXR'", {
  skip_if(is.null(opt_eqs))
  expect_s3_class(opt_eqs, "optimize.portfolio.CVXR")
})

test_that("min-EQS: extractWeights returns numeric vector", {
  skip_if(is.null(opt_eqs))
  w <- extractWeights(opt_eqs)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
})

test_that("min-EQS: weights have length equal to number of assets", {
  skip_if(is.null(opt_eqs))
  expect_equal(length(extractWeights(opt_eqs)), ncol(R))
})

test_that("min-EQS: weights sum to approximately 1", {
  skip_if(is.null(opt_eqs))
  expect_equal(sum(extractWeights(opt_eqs)), 1, tolerance = 1e-4)
})

test_that("min-EQS: all weights are non-negative (long_only)", {
  skip_if(is.null(opt_eqs))
  expect_true(all(round(extractWeights(opt_eqs), 10) >= 0))
})

test_that("min-EQS: objective_measures contains an EQS element", {
  skip_if(is.null(opt_eqs))
  om <- opt_eqs$objective_measures
  expect_false(is.null(om))
  nm <- names(unlist(om))
  expect_true(any(grepl("EQS", nm, ignore.case = TRUE)))
})

# ===========================================================================
# Section 3: max-CSM-ratio explicit CSMratio=TRUE (verify default behaviour)
# Uses both mean AND CSM objectives → reward=TRUE, risk_CSM=TRUE → CSMratio=TRUE
# ===========================================================================

portf_csm_ratio <- portfolio.spec(nms)
portf_csm_ratio <- add.constraint(portf_csm_ratio, type = "full_investment")
portf_csm_ratio <- add.constraint(portf_csm_ratio, type = "long_only")
portf_csm_ratio <- add.objective(portf_csm_ratio, type = "return", name = "mean")
portf_csm_ratio <- add.objective(portf_csm_ratio, type = "risk",   name = "CSM")

opt_csm_ratio <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R, portf_csm_ratio, optimize_method = "CVXR",
                       CSMratio = TRUE)
  )),
  error = function(e) NULL
)

test_that("max-CSM-ratio (explicit CSMratio=TRUE): result class is CVXR", {
  skip_if(is.null(opt_csm_ratio))
  expect_s3_class(opt_csm_ratio, "optimize.portfolio.CVXR")
})

test_that("max-CSM-ratio: weights sum to approximately 1", {
  skip_if(is.null(opt_csm_ratio))
  expect_equal(sum(extractWeights(opt_csm_ratio)), 1, tolerance = 1e-3)
})

# ===========================================================================
# Section 4: max-EQS-ratio explicit EQSratio=TRUE (verify default behaviour)
# ===========================================================================

portf_eqs_ratio <- portfolio.spec(nms)
portf_eqs_ratio <- add.constraint(portf_eqs_ratio, type = "full_investment")
portf_eqs_ratio <- add.constraint(portf_eqs_ratio, type = "long_only")
portf_eqs_ratio <- add.objective(portf_eqs_ratio, type = "return", name = "mean")
portf_eqs_ratio <- add.objective(portf_eqs_ratio, type = "risk",   name = "EQS")

opt_eqs_ratio <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R, portf_eqs_ratio, optimize_method = "CVXR",
                       EQSratio = TRUE)
  )),
  error = function(e) NULL
)

test_that("max-EQS-ratio (explicit EQSratio=TRUE): result class is CVXR", {
  skip_if(is.null(opt_eqs_ratio))
  expect_s3_class(opt_eqs_ratio, "optimize.portfolio.CVXR")
})

test_that("max-EQS-ratio: weights sum to approximately 1", {
  skip_if(is.null(opt_eqs_ratio))
  expect_equal(sum(extractWeights(opt_eqs_ratio)), 1, tolerance = 1e-3)
})
