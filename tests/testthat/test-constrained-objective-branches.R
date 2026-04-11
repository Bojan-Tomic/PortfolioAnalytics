###############################################################################
# tests/testthat/test-constrained-objective-branches.R
#
# Source files hit: R/constrained_objective.R
#
# Targeted branches not yet covered by test-constrained-objective.R:
#
#   constrained_objective_v2 (== constrained_objective):
#     - minmax_objective: tmp_measure > objective$max  (penalty path)
#     - minmax_objective: tmp_measure < objective$min  (penalty path)
#     - risk_budget_objective: numeric target (line ~693)
#     - risk_budget_objective: min_difference=TRUE (line ~715)
#     - var objective alias (name="var") — tmp_objname substitution to "StdDev"
#
#   constrained_objective_v1:
#     - entire function exercised (v1 API)
#     - trace=TRUE list return
#     - normalize=FALSE penalty paths
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------
utils::data(edhec)
R5  <- edhec[, 1:5]
w5  <- rep(0.2, 5)


# ===========================================================================
# 1. minmax_objective — penalty branches
# ===========================================================================

# Build a portfolio with a minmax objective on mean return.
# mean return of equal-weight edhec[,1:5] is roughly 0.006 per month.
# Set max = 0.001 so tmp_measure > max is triggered for equal weights.
portf_mm_over <- portfolio.spec(assets = colnames(R5))
portf_mm_over <- add.constraint(portf_mm_over, type = "full_investment")
portf_mm_over <- add.constraint(portf_mm_over, type = "long_only")
portf_mm_over$objectives <- list(
  minmax_objective(name = "mean", min = 0.0001, max = 0.001)
)

# Set min = 0.999 so tmp_measure < min is triggered.
portf_mm_under <- portfolio.spec(assets = colnames(R5))
portf_mm_under <- add.constraint(portf_mm_under, type = "full_investment")
portf_mm_under <- add.constraint(portf_mm_under, type = "long_only")
portf_mm_under$objectives <- list(
  minmax_objective(name = "mean", min = 0.999, max = 1.0)
)

# A portfolio where mean is comfortably inside [min, max] — no penalty.
portf_mm_ok <- portfolio.spec(assets = colnames(R5))
portf_mm_ok <- add.constraint(portf_mm_ok, type = "full_investment")
portf_mm_ok <- add.constraint(portf_mm_ok, type = "long_only")
portf_mm_ok$objectives <- list(
  minmax_objective(name = "mean", min = 0.001, max = 0.02)
)

test_that("minmax_objective: tmp_measure > max incurs penalty (higher out)", {
  result_over <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_over, trace = FALSE
  )
  result_ok <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_ok, trace = FALSE
  )
  expect_true(result_over > result_ok)
})

test_that("minmax_objective: tmp_measure > max returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_over, trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("minmax_objective: tmp_measure < min incurs penalty (higher out)", {
  result_under <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_under, trace = FALSE
  )
  result_ok <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_ok, trace = FALSE
  )
  expect_true(result_under > result_ok)
})

test_that("minmax_objective: tmp_measure < min returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_under, trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("minmax_objective: in-range measure returns numeric (no penalty)", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_ok, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("minmax_objective: trace=TRUE includes mean in objective_measures", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_mm_ok, trace = TRUE
  )
  expect_true("mean" %in% names(result$objective_measures))
})


# ===========================================================================
# 2. risk_budget_objective with numeric target
# ===========================================================================

# Numeric target triggers the univariate penalty path at ~line 693.
portf_rb_tgt <- portfolio.spec(assets = colnames(R5))
portf_rb_tgt <- add.constraint(portf_rb_tgt, type = "full_investment")
portf_rb_tgt <- add.constraint(portf_rb_tgt, type = "long_only")
portf_rb_tgt <- add.objective(portf_rb_tgt,
  type = "risk_budget", name = "ES",
  arguments = list(p = 0.95),
  max_prisk = 0.5, target = 0  # impossible ES target => penalty
)

# Same but no target => only max_prisk penalty applies.
portf_rb_notgt <- portfolio.spec(assets = colnames(R5))
portf_rb_notgt <- add.constraint(portf_rb_notgt, type = "full_investment")
portf_rb_notgt <- add.constraint(portf_rb_notgt, type = "long_only")
portf_rb_notgt <- add.objective(portf_rb_notgt,
  type = "risk_budget", name = "ES",
  arguments = list(p = 0.95),
  max_prisk = 0.5
)

test_that("risk_budget with numeric target: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_tgt, trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("risk_budget with numeric target: higher out than no-target version", {
  result_tgt <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_tgt, trace = FALSE
  )
  result_notgt <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_notgt, trace = FALSE
  )
  expect_true(result_tgt > result_notgt)
})

test_that("risk_budget with numeric target: trace=TRUE includes ES in objective_measures", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_tgt, trace = TRUE
  )
  expect_true("ES" %in% names(result$objective_measures))
})


# ===========================================================================
# 3. risk_budget_objective with min_difference=TRUE
# ===========================================================================

portf_rb_md <- portfolio.spec(assets = colnames(R5))
portf_rb_md <- add.constraint(portf_rb_md, type = "full_investment")
portf_rb_md <- add.constraint(portf_rb_md, type = "long_only")
portf_rb_md <- add.objective(portf_rb_md,
  type = "risk_budget", name = "ES",
  arguments = list(p = 0.95),
  min_difference = TRUE
)

test_that("risk_budget min_difference=TRUE: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_md, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("risk_budget min_difference=TRUE: trace=TRUE includes ES", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_md, trace = TRUE
  )
  expect_true("ES" %in% names(result$objective_measures))
})

test_that("risk_budget min_difference=TRUE: concentrated weights produce higher out", {
  w_conc <- c(0.96, 0.01, 0.01, 0.01, 0.01)
  result_conc <- constrained_objective(
    w = w_conc, R = R5, portfolio = portf_rb_md, trace = FALSE
  )
  result_eq <- constrained_objective(
    w = w5, R = R5, portfolio = portf_rb_md, trace = FALSE
  )
  # concentrated portfolio has more unequal risk contributions => higher penalty
  expect_true(result_conc > result_eq)
})


# ===========================================================================
# 4. "var" objective — tmp_objname substitution to "StdDev" in trace
# ===========================================================================

portf_var <- portfolio.spec(assets = colnames(R5))
portf_var <- add.constraint(portf_var, type = "full_investment")
portf_var <- add.constraint(portf_var, type = "long_only")
portf_var <- add.objective(portf_var, type = "risk", name = "var")

test_that("var objective: trace=TRUE renames 'var' to 'StdDev' in objective_measures", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_var, trace = TRUE
  )
  expect_true("StdDev" %in% names(result$objective_measures))
  expect_false("var" %in% names(result$objective_measures))
})

test_that("var objective: constrained_objective returns positive numeric scalar", {
  result <- constrained_objective(
    w = w5, R = R5, portfolio = portf_var, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_true(result > 0)
})


# ===========================================================================
# 5. constrained_objective_v1 — v1 API
# ===========================================================================

# Build a v1_constraint object using constraint_v1() — the original v1 constructor
v1_constr <- constraint_v1(assets = colnames(R5), min = 0, max = 1,
                            min_sum = 0.99, max_sum = 1.01,
                            weight_seq = generatesequence())
v1_constr <- add.objective_v1(v1_constr, type = "risk", name = "StdDev")

test_that("constrained_objective_v1: returns numeric scalar (trace=FALSE)", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr, trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("constrained_objective_v1: result is non-negative", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr, trace = FALSE
  )
  expect_true(result >= 0)
})

test_that("constrained_objective_v1: trace=TRUE returns a list", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr, trace = TRUE
  )
  expect_true(is.list(result))
})

test_that("constrained_objective_v1: trace=TRUE list has out, weights, objective_measures", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr, trace = TRUE
  )
  expect_true(!is.null(result$out))
  expect_true(!is.null(result$weights))
  expect_true(!is.null(result$objective_measures))
})

test_that("constrained_objective_v1: normalize=FALSE returns numeric scalar", {
  result <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr,
    trace = FALSE, normalize = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("constrained_objective_v1: errors on non-constraint input", {
  expect_error(
    constrained_objective_v1(w = w5, R = R5, constraints = list()),
    regexp = "constraint"
  )
})

test_that("constrained_objective_v1: normalize=FALSE over-sum penalty works", {
  w_over <- rep(0.3, 5) # sum = 1.5 > max_sum = 1.01
  result_over <- constrained_objective_v1(
    w = w_over, R = R5, constraints = v1_constr,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_over > result_ok)
})

# ===========================================================================
# 6. constrained_objective_v1 — additional branch coverage
# ===========================================================================

test_that("constrained_objective_v1: R truncated when ncol(R) > length(w) (line 24)", {
  skip_on_cran()
  # Pass 3-asset weight vector against 5-column R — R should be silently trimmed
  w3 <- rep(1/3, 3)
  v1_c3 <- constraint_v1(assets = colnames(R5)[1:3], min = 0, max = 1,
                          min_sum = 0.99, max_sum = 1.01,
                          weight_seq = generatesequence())
  v1_c3 <- add.objective_v1(v1_c3, type = "risk", name = "StdDev")
  result <- constrained_objective_v1(w = w3, R = R5, constraints = v1_c3, trace = FALSE)
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("constrained_objective_v1: length-mismatch warning (line 42)", {
  skip_on_cran()
  # v1_constr has 5 assets; pass 4-weight vector — should warn about length mismatch
  w4 <- rep(0.25, 4)
  # Capture all warnings and check at least one matches "length"
  warns <- character(0)
  withCallingHandlers(
    constrained_objective_v1(w = w4, R = R5[, 1:4], constraints = v1_constr, trace = FALSE),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("length", warns)))
})

test_that("constrained_objective_v1: NULL objectives warning (line 106)", {
  skip_on_cran()
  v1_null_obj <- constraint_v1(assets = colnames(R5), min = 0, max = 1,
                                min_sum = 0.99, max_sum = 1.01,
                                weight_seq = generatesequence())
  # Set objectives to NULL explicitly to trigger the warning on line 106
  v1_null_obj$objectives <- NULL
  warns <- character(0)
  withCallingHandlers(
    constrained_objective_v1(w = w5, R = R5, constraints = v1_null_obj, trace = FALSE),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("objectives", warns)))
})

test_that("constrained_objective_v1: normalize=FALSE under-sum penalty (line 82)", {
  skip_on_cran()
  # sum(w) < min_sum triggers the under-sum penalty branch
  w_under <- rep(0.01, 5)  # sum = 0.05 < min_sum = 0.99
  result_under <- constrained_objective_v1(
    w = w_under, R = R5, constraints = v1_constr,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective_v1(
    w = w5, R = R5, constraints = v1_constr,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_under > result_ok)
})

# ===========================================================================
# 7. constrained_objective (v2) — additional branch coverage
# ===========================================================================

test_that("constrained_objective v2: R truncated when ncol(R) > length(w) (line 351)", {
  skip_on_cran()
  w3 <- rep(1/3, 3)
  p3 <- portfolio.spec(assets = colnames(R5)[1:3])
  p3 <- add.constraint(p3, type = "full_investment")
  p3 <- add.constraint(p3, type = "long_only")
  p3 <- add.objective(p3, type = "risk", name = "StdDev")
  result <- constrained_objective(w = w3, R = R5, portfolio = p3, trace = FALSE)
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("constrained_objective v2: stop on non-portfolio object (line 370-371)", {
  skip_on_cran()
  expect_error(
    constrained_objective(w = w5, R = R5, portfolio = list()),
    regexp = "portfolio"
  )
})

test_that("constrained_objective v2: length-mismatch warning (line 375-376)", {
  skip_on_cran()
  p4 <- portfolio.spec(assets = colnames(edhec5)[1:4])
  p4 <- add.constraint(p4, type = "full_investment")
  p4 <- add.constraint(p4, type = "long_only")
  p4 <- add.objective(p4, type = "risk", name = "StdDev")
  # Pass 5-weight vector to a 4-asset portfolio
  expect_warning(
    constrained_objective(w = w5, R = R5, portfolio = p4, trace = FALSE),
    regexp = "length"
  )
})

test_that("constrained_objective v2: NULL objectives warning (lines 562-563)", {
  skip_on_cran()
  p_noobj <- portfolio.spec(assets = colnames(R5))
  p_noobj <- add.constraint(p_noobj, type = "full_investment")
  p_noobj <- add.constraint(p_noobj, type = "long_only")
  # Set objectives to NULL explicitly to trigger the warning branch
  p_noobj$objectives <- NULL
  expect_warning(
    constrained_objective(w = w5, R = R5, portfolio = p_noobj, trace = FALSE),
    regexp = "objectives"
  )
})

test_that("constrained_objective v2: normalize=FALSE under-sum penalty (line 400-402)", {
  skip_on_cran()
  p_nrm <- portfolio.spec(assets = colnames(R5))
  p_nrm <- add.constraint(p_nrm, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_nrm <- add.constraint(p_nrm, type = "long_only")
  p_nrm <- add.objective(p_nrm, type = "risk", name = "StdDev")
  w_under <- rep(0.01, 5)  # sum = 0.05 < 0.99
  result_under <- constrained_objective(w = w_under, R = R5, portfolio = p_nrm,
                                         normalize = FALSE, trace = FALSE)
  result_ok <- constrained_objective(w = w5, R = R5, portfolio = p_nrm,
                                      normalize = FALSE, trace = FALSE)
  expect_true(result_under > result_ok)
})

test_that("constrained_objective v2: normalize=FALSE over-sum penalty (line 396-398)", {
  skip_on_cran()
  p_nrm <- portfolio.spec(assets = colnames(R5))
  p_nrm <- add.constraint(p_nrm, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_nrm <- add.constraint(p_nrm, type = "long_only")
  p_nrm <- add.objective(p_nrm, type = "risk", name = "StdDev")
  w_over <- rep(0.3, 5)  # sum = 1.5 > 1.01
  result_over <- constrained_objective(w = w_over, R = R5, portfolio = p_nrm,
                                        normalize = FALSE, trace = FALSE)
  result_ok <- constrained_objective(w = w5, R = R5, portfolio = p_nrm,
                                      normalize = FALSE, trace = FALSE)
  expect_true(result_over > result_ok)
})

# ===========================================================================
# 8. weight_concentration_objective (v2 lines 738-752)
# ===========================================================================

test_that("weight_concentration_objective: scalar conc_aversion path (line 742)", {
  skip_on_cran()
  p_wc <- portfolio.spec(assets = colnames(R5))
  p_wc <- add.constraint(p_wc, type = "full_investment")
  p_wc <- add.constraint(p_wc, type = "long_only")
  p_wc <- add.objective(p_wc, type = "weight_concentration",
                         name = "HHI", conc_aversion = 2)
  result <- constrained_objective(w = w5, R = R5, portfolio = p_wc, trace = FALSE)
  expect_true(is.numeric(result))
  # concentrated weights should give higher penalty
  w_conc <- c(0.96, 0.01, 0.01, 0.01, 0.01)
  result_conc <- constrained_objective(w = w_conc, R = R5, portfolio = p_wc, trace = FALSE)
  expect_true(result_conc > result)
})

test_that("weight_concentration_objective: group conc_aversion path (line 747-750)", {
  skip_on_cran()
  p_wc_grp <- portfolio.spec(assets = colnames(R5))
  p_wc_grp <- add.constraint(p_wc_grp, type = "full_investment")
  p_wc_grp <- add.constraint(p_wc_grp, type = "long_only")
  p_wc_grp <- add.objective(p_wc_grp, type = "weight_concentration",
                              name = "HHI",
                              conc_aversion = c(0.5, 0.5),
                              conc_groups = list(c(1, 2, 3), c(4, 5)))
  result <- constrained_objective(w = w5, R = R5, portfolio = p_wc_grp, trace = FALSE)
  expect_true(is.numeric(result))
})

# ===========================================================================
# 9. risk_budget min_concentration (v2 line 727-734)
# ===========================================================================

test_that("risk_budget min_concentration=TRUE: returns numeric (line 727-734)", {
  skip_on_cran()
  p_rb_mc <- portfolio.spec(assets = colnames(R5))
  p_rb_mc <- add.constraint(p_rb_mc, type = "full_investment")
  p_rb_mc <- add.constraint(p_rb_mc, type = "long_only")
  p_rb_mc <- add.objective(p_rb_mc, type = "risk_budget", name = "ES",
                            arguments = list(p = 0.95),
                            min_concentration = TRUE)
  result <- constrained_objective(w = w5, R = R5, portfolio = p_rb_mc, trace = FALSE)
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

# ===========================================================================
# 10. ETL/mETL objective aliases (v2 lines 599-604)
# ===========================================================================

test_that("ETL objective alias maps to ES function (lines 599-604)", {
  skip_on_cran()
  p_etl <- portfolio.spec(assets = colnames(R5))
  p_etl <- add.constraint(p_etl, type = "full_investment")
  p_etl <- add.constraint(p_etl, type = "long_only")
  p_etl <- add.objective(p_etl, type = "risk", name = "ETL")
  result <- constrained_objective(w = w5, R = R5, portfolio = p_etl, trace = FALSE)
  expect_true(is.numeric(result))
  expect_true(result > 0)
})

test_that("mETL objective alias maps to ES function (lines 599-604)", {
  skip_on_cran()
  p_metl <- portfolio.spec(assets = colnames(R5))
  p_metl <- add.constraint(p_metl, type = "full_investment")
  p_metl <- add.constraint(p_metl, type = "long_only")
  p_metl <- add.objective(p_metl, type = "risk", name = "mETL")
  result <- constrained_objective(w = w5, R = R5, portfolio = p_metl, trace = FALSE)
  expect_true(is.numeric(result))
  expect_true(result > 0)
})
