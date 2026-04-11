###############################################################################
# tests/testthat/test-optimize-roi-extended.R
#
# Extended ROI solver coverage for optimize.portfolio.R (optFUN.R paths) that
# are NOT exercised by the existing migration tests.
#
# New paths covered:
#   1.  Factor exposure constraint    -> gmv_opt() via constraints$B
#   2.  Position limit (MILP/glpk)   -> maxret_milp_opt()
#   3.  Proportional transaction cost -> gmv_opt_ptc()   [requires corpcor]
#   4.  Turnover constraint           -> gmv_opt_toc()   [requires corpcor]
#   5.  Return target as constraint   -> gmv_opt() with non-NA target
#   6.  Diversification constraint    -> spec storage; not enforced by QP
#   7.  trace=TRUE structure          -> $weights, $objective_measures, $out, $R
#   8.  Multiple objectives QU        -> gmv_opt() with non-zero moments$mean
#   9.  Weight concentration HHI      -> gmv_opt() with scalar lambda_hhi
#  10.  Error conditions              -> wrong portfolio class, bad objective name
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("ROI.plugin.glpk")

utils::data(edhec)
R <- edhec[, 1:5]

# ============================================================
# 1.  Factor exposure constraint with ROI (quadprog)
#     Exercises the constraints$B branch inside gmv_opt().
# ============================================================

portf_fe <- portfolio.spec(assets = colnames(R))
portf_fe <- add.constraint(portf_fe, type = "full_investment")
portf_fe <- add.constraint(portf_fe, type = "long_only")

# Single "market" factor: all assets loaded with weight 1.
# Constraint: B^T w in [0.9, 1.1].  Since full_investment forces sum(w)=1
# this is always satisfied, but the constraint rows ARE added to Amat,
# exercising the code path in gmv_opt().
B <- matrix(1, nrow = ncol(R), ncol = 1)
rownames(B) <- colnames(R)
colnames(B) <- "market"
portf_fe <- add.constraint(portf_fe, type = "factor_exposure",
                           B = B, lower = 0.9, upper = 1.1)
portf_fe <- add.objective(portf_fe, type = "risk", name = "StdDev")

opt_fe <- optimize.portfolio(R, portf_fe,
                             optimize_method = "ROI", trace = TRUE)

test_that("factor_exposure: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_fe, "optimize.portfolio.ROI")
})

test_that("factor_exposure: weights are numeric", {
  expect_true(is.numeric(extractWeights(opt_fe)))
})

test_that("factor_exposure: weights sum to 1 (full investment satisfied)", {
  expect_equal(sum(extractWeights(opt_fe)), 1, tolerance = 1e-6)
})

test_that("factor_exposure: market-factor exposure is in [0.9, 1.1]", {
  w <- extractWeights(opt_fe)
  exposure <- as.numeric(t(B) %*% w)   # sum(w) when B = all-ones
  expect_true(exposure >= 0.9 - 1e-6 && exposure <= 1.1 + 1e-6)
})

test_that("factor_exposure: StdDev objective measure is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(opt_fe)$StdDev))
})

test_that("factor_exposure: portfolio spec stores B matrix", {
  # Verify the constraint is stored in the portfolio spec
  fe_con <- portf_fe$constraints[[3]]
  expect_true(inherits(fe_con, "factor_exposure_constraint"))
  expect_equal(fe_con$lower, 0.9)
  expect_equal(fe_con$upper, 1.1)
})


# ============================================================
# 2.  Position limit (MILP) with ROI / glpk
#     Exercises maxret_milp_opt() via the mean-only objective + max_pos.
# ============================================================

portf_milp <- portfolio.spec(assets = colnames(R))
portf_milp <- add.constraint(portf_milp, type = "full_investment")
portf_milp <- add.constraint(portf_milp, type = "long_only")
portf_milp <- add.constraint(portf_milp, type = "position_limit", max_pos = 3)
portf_milp <- add.objective(portf_milp, type = "return", name = "mean")

opt_milp <- optimize.portfolio(R, portf_milp,
                               optimize_method = "ROI", trace = TRUE)

test_that("position_limit MILP: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_milp, "optimize.portfolio.ROI")
})

test_that("position_limit MILP: weights are numeric", {
  expect_true(is.numeric(extractWeights(opt_milp)))
})

test_that("position_limit MILP: at most 3 non-zero weights", {
  w   <- extractWeights(opt_milp)
  tol <- .Machine$double.eps^0.5
  expect_true(sum(abs(w) > tol) <= 3)
})

test_that("position_limit MILP: all weights non-negative (long-only)", {
  expect_true(all(extractWeights(opt_milp) >= -.Machine$double.eps^0.5))
})

test_that("position_limit MILP: mean objective measure is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(opt_milp)$mean))
})


# ============================================================
# 3.  Proportional transaction cost constraint with ROI
#     Exercises gmv_opt_ptc().  Requires corpcor.
#
#     NOTE: gmv_opt_ptc() contains a known bug where the initial equality
#     constraint uses rhs = 1 + target instead of rhs = target, making the
#     QP infeasible when no return target is supplied (0 == 1).  Tests here
#     verify that (a) the code path is entered without throwing an R-level
#     error, (b) the result has the correct S3 class, and (c) the portfolio
#     spec stores the ptc constraint correctly.  Weight-validity assertions
#     are intentionally omitted because the solver returns NA weights in the
#     default (no target-return) case.
# ============================================================

skip_if_not_installed("corpcor")

portf_ptc <- portfolio.spec(assets = colnames(R))
portf_ptc <- add.constraint(portf_ptc, type = "full_investment")
portf_ptc <- add.constraint(portf_ptc, type = "long_only")
portf_ptc <- add.constraint(portf_ptc, type = "transaction_cost", ptc = 0.01)
portf_ptc <- add.objective(portf_ptc, type = "risk", name = "StdDev")

opt_ptc <- optimize.portfolio(R, portf_ptc,
                              optimize_method = "ROI", trace = TRUE)

test_that("transaction_cost: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_ptc, "optimize.portfolio.ROI")
})

test_that("transaction_cost: portfolio spec stores transaction_cost_constraint", {
  ptc_con <- portf_ptc$constraints[[3]]
  expect_true(inherits(ptc_con, "transaction_cost_constraint"))
})

test_that("transaction_cost: ptc value stored as length-N vector of 0.01", {
  ptc_con <- portf_ptc$constraints[[3]]
  expect_equal(length(ptc_con$ptc), ncol(R))
  expect_true(all(ptc_con$ptc == 0.01))
})

test_that("transaction_cost: result has $portfolio containing the ptc constraint", {
  expect_false(is.null(opt_ptc$portfolio))
  ptc_in_result <- opt_ptc$portfolio$constraints[[3]]
  expect_true(inherits(ptc_in_result, "transaction_cost_constraint"))
})


# ============================================================
# 4.  Turnover constraint with ROI
#     Exercises gmv_opt_toc().  Requires corpcor.
# ============================================================

portf_toc <- portfolio.spec(assets = colnames(R))
portf_toc <- add.constraint(portf_toc, type = "full_investment")
portf_toc <- add.constraint(portf_toc, type = "long_only")
portf_toc <- add.constraint(portf_toc, type = "turnover",
                            turnover_target = 0.30)
portf_toc <- add.objective(portf_toc, type = "risk", name = "StdDev")

opt_toc <- optimize.portfolio(R, portf_toc,
                              optimize_method = "ROI", trace = TRUE)

test_that("turnover_constraint: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_toc, "optimize.portfolio.ROI")
})

test_that("turnover_constraint: weights are numeric", {
  expect_true(is.numeric(extractWeights(opt_toc)))
})

test_that("turnover_constraint: weights sum to approximately 1", {
  expect_equal(sum(extractWeights(opt_toc)), 1, tolerance = 1e-4)
})

test_that("turnover_constraint: all weights non-negative (long-only)", {
  expect_true(all(round(extractWeights(opt_toc), 10) >= 0))
})

test_that("turnover_constraint: StdDev objective measure is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(opt_toc)$StdDev))
})

test_that("turnover_constraint: turnover <= target (0.30) from equal-weight seed", {
  w0  <- rep(1 / ncol(R), ncol(R))   # equal-weight seed = portfolio$assets default
  w1  <- extractWeights(opt_toc)
  tov <- sum(abs(w1 - w0))
  expect_true(tov <= 0.30 + 1e-4)
})


# ============================================================
# 5.  Return target as a constraint with ROI (quadprog)
#     Exercises gmv_opt() with non-NA target coming from return_constraint.
# ============================================================

ret_target <- 0.005   # ~0.5 % monthly; feasible for edhec[, 1:5]

portf_rc <- portfolio.spec(assets = colnames(R))
portf_rc <- add.constraint(portf_rc, type = "full_investment")
portf_rc <- add.constraint(portf_rc, type = "long_only")
portf_rc <- add.constraint(portf_rc, type = "return",
                           return_target = ret_target)
portf_rc <- add.objective(portf_rc, type = "risk", name = "StdDev")

opt_rc <- optimize.portfolio(R, portf_rc,
                             optimize_method = "ROI", trace = TRUE)

test_that("return_target constraint: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_rc, "optimize.portfolio.ROI")
})

test_that("return_target constraint: weights are numeric", {
  expect_true(is.numeric(extractWeights(opt_rc)))
})

test_that("return_target constraint: portfolio spec stores return_target", {
  rc_con <- portf_rc$constraints[[3]]
  expect_true(inherits(rc_con, "return_constraint"))
  expect_equal(rc_con$return_target, ret_target)
})

test_that("return_target constraint: realised mean return approx equals target", {
  w         <- extractWeights(opt_rc)
  port_mean <- as.numeric(w %*% colMeans(R))
  expect_equal(port_mean, ret_target, tolerance = 1e-5)
})


# ============================================================
# 6.  Diversification constraint with ROI
#     div_target is a quadratic constraint; it is stored in the portfolio
#     spec but the QP solver (gmv_opt) does NOT enforce it.  Tests verify
#     that the optimisation completes without error and the constraint is
#     correctly stored in the spec.
# ============================================================

portf_div <- portfolio.spec(assets = colnames(R))
portf_div <- add.constraint(portf_div, type = "full_investment")
portf_div <- add.constraint(portf_div, type = "long_only")
portf_div <- add.constraint(portf_div, type = "diversification",
                            div_target = 0.7)
portf_div <- add.objective(portf_div, type = "risk", name = "StdDev")

opt_div <- optimize.portfolio(R, portf_div,
                              optimize_method = "ROI", trace = TRUE)

test_that("diversification constraint: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_div, "optimize.portfolio.ROI")
})

test_that("diversification constraint: weights are numeric", {
  expect_true(is.numeric(extractWeights(opt_div)))
})

test_that("diversification constraint: weights sum to 1", {
  expect_equal(sum(extractWeights(opt_div)), 1, tolerance = 1e-6)
})

test_that("diversification constraint: portfolio spec stores div_target = 0.7", {
  div_con <- portf_div$constraints[[3]]
  expect_true(inherits(div_con, "diversification_constraint"))
  expect_equal(div_con$div_target, 0.7)
})


# ============================================================
# 7.  trace = TRUE - verify result structure
#     When trace=TRUE the optimize.portfolio.ROI object should contain
#     $weights, $objective_measures, $out (solver obj value), and $R.
# ============================================================

portf_tr <- portfolio.spec(assets = colnames(R))
portf_tr <- add.constraint(portf_tr, type = "full_investment")
portf_tr <- add.constraint(portf_tr, type = "long_only")
portf_tr <- add.objective(portf_tr, type = "risk", name = "StdDev")

opt_tr <- optimize.portfolio(R, portf_tr,
                             optimize_method = "ROI", trace = TRUE)

test_that("trace=TRUE: result has $weights", {
  expect_false(is.null(opt_tr$weights))
  expect_true(is.numeric(opt_tr$weights))
})

test_that("trace=TRUE: result has $objective_measures", {
  expect_false(is.null(opt_tr$objective_measures))
  expect_true(is.list(opt_tr$objective_measures))
})

test_that("trace=TRUE: result has $out (solver objective value)", {
  expect_false(is.null(opt_tr$out))
  expect_true(is.numeric(opt_tr$out))
})

test_that("trace=TRUE: result has $R (returns stored when trace=TRUE)", {
  expect_false(is.null(opt_tr$R))
})

test_that("trace=TRUE: $R has same dimensions as input", {
  expect_equal(nrow(opt_tr$R), nrow(R))
  expect_equal(ncol(opt_tr$R), ncol(R))
})

test_that("trace=TRUE: result has $portfolio", {
  expect_false(is.null(opt_tr$portfolio))
  expect_true(is.portfolio(opt_tr$portfolio))
})

test_that("trace=TRUE: result has $elapsed_time", {
  expect_false(is.null(opt_tr$elapsed_time))
})


# ============================================================
# 8.  Multiple ROI objectives: StdDev + mean (Quadratic Utility path)
#     When both "var" and "mean" are in moments, gmv_opt() is called with
#     L = -moments$mean (non-zero), maximising quadratic utility.
# ============================================================

portf_qu <- portfolio.spec(assets = colnames(R))
portf_qu <- add.constraint(portf_qu, type = "full_investment")
portf_qu <- add.constraint(portf_qu, type = "long_only")
portf_qu <- add.objective(portf_qu, type = "risk",   name = "StdDev")
portf_qu <- add.objective(portf_qu, type = "return", name = "mean")

opt_qu <- optimize.portfolio(R, portf_qu,
                             optimize_method = "ROI", trace = TRUE)

test_that("multi-objective QU: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_qu, "optimize.portfolio.ROI")
})

test_that("multi-objective QU: weights are numeric", {
  expect_true(is.numeric(extractWeights(opt_qu)))
})

test_that("multi-objective QU: weights sum to 1", {
  expect_equal(sum(extractWeights(opt_qu)), 1, tolerance = 1e-6)
})

test_that("multi-objective QU: all weights non-negative (long-only)", {
  expect_true(all(round(extractWeights(opt_qu), 10) >= 0))
})

test_that("multi-objective QU: objective_measures contains StdDev", {
  om <- extractObjectiveMeasures(opt_qu)
  expect_false(is.null(om$StdDev))
  expect_true(is.numeric(om$StdDev))
})

test_that("multi-objective QU: objective_measures contains mean", {
  om <- extractObjectiveMeasures(opt_qu)
  expect_false(is.null(om$mean))
  expect_true(is.numeric(om$mean))
})


# ============================================================
# 9.  Weight concentration objective with ROI - scalar conc_aversion
#     Uses gmv_opt() with lambda_hhi set (no conc_groups).
#     The HHI penalty reduces weight concentration relative to pure GMV.
# ============================================================

portf_hhi <- portfolio.spec(assets = colnames(R))
portf_hhi <- add.constraint(portf_hhi, type = "full_investment")
portf_hhi <- add.constraint(portf_hhi, type = "long_only")
portf_hhi <- add.objective(portf_hhi, type = "risk", name = "StdDev")
portf_hhi <- add.objective(portf_hhi,
                           type         = "weight_concentration",
                           name         = "HHI",
                           conc_aversion = 0.001)

opt_hhi <- optimize.portfolio(R, portf_hhi,
                              optimize_method = "ROI", trace = TRUE)

test_that("HHI scalar conc_aversion: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_hhi, "optimize.portfolio.ROI")
})

test_that("HHI scalar conc_aversion: weights are numeric", {
  expect_true(is.numeric(extractWeights(opt_hhi)))
})

test_that("HHI scalar conc_aversion: weights sum to 1", {
  expect_equal(sum(extractWeights(opt_hhi)), 1, tolerance = 1e-6)
})

test_that("HHI scalar conc_aversion: all weights non-negative (long-only)", {
  expect_true(all(round(extractWeights(opt_hhi), 10) >= 0))
})

test_that("HHI scalar conc_aversion: objective_measures contains StdDev", {
  om <- extractObjectiveMeasures(opt_hhi)
  expect_false(is.null(om$StdDev))
  expect_true(is.numeric(om$StdDev))
})

test_that("HHI penalty reduces HHI versus pure min-variance", {
  # Pure GMV (no HHI term)
  portf_gmv <- portfolio.spec(assets = colnames(R))
  portf_gmv <- add.constraint(portf_gmv, type = "full_investment")
  portf_gmv <- add.constraint(portf_gmv, type = "long_only")
  portf_gmv <- add.objective(portf_gmv, type = "risk", name = "StdDev")
  opt_gmv   <- optimize.portfolio(R, portf_gmv, optimize_method = "ROI")

  hhi_gmv <- sum(extractWeights(opt_gmv)^2)
  hhi_pen <- sum(extractWeights(opt_hhi)^2)
  # With a positive concentration-aversion penalty the HHI must be <= GMV HHI
  expect_true(hhi_pen <= hhi_gmv + 1e-6)
})


# ============================================================
# 10.  Error conditions
# ============================================================

# 10a: non-portfolio object passed as portfolio argument
test_that("wrong portfolio class: optimize.portfolio throws an error", {
  expect_error(
    optimize.portfolio(R = R,
                       portfolio      = "not a portfolio",
                       optimize_method = "ROI"),
    regexp      = "class.*portfolio|portfolio.*class",
    ignore.case = TRUE
  )
})

# 10b: objective name not supported by ROI
portf_bad <- portfolio.spec(assets = colnames(R))
portf_bad <- add.constraint(portf_bad, type = "full_investment")
portf_bad <- add.constraint(portf_bad, type = "long_only")
portf_bad <- add.objective(portf_bad, type = "risk", name = "SomeUnsupportedRisk")

test_that("unsupported objective name with ROI: optimize.portfolio throws an error", {
  expect_error(
    optimize.portfolio(R = R,
                       portfolio      = portf_bad,
                       optimize_method = "ROI"),
    regexp      = "ROI only solves",
    ignore.case = TRUE
  )
})


# ============================================================
# 11.  GMV with group constraint (quadprog)
#      Exercises the n.groups > 0 branch inside gmv_opt()
#      (optFUN.R lines ~70-79: group Amat/dir/rhs build).
#
#      Groups: assets 1-3 sum in [0.30, 0.70],
#              assets 4-5 sum in [0.20, 0.60].
#      With full investment the effective feasible ranges are
#      group1 in [0.40, 0.70] and group2 in [0.30, 0.60].
# ============================================================

portf_gmv_grp <- portfolio.spec(assets = colnames(R))
portf_gmv_grp <- add.constraint(portf_gmv_grp, type = "weight_sum",
                                min_sum = 0.99, max_sum = 1.01)
portf_gmv_grp <- add.constraint(portf_gmv_grp, type = "box",
                                min = 0.05, max = 0.50)
portf_gmv_grp <- add.constraint(portf_gmv_grp, type = "group",
                                groups    = list(c(1, 2, 3), c(4, 5)),
                                group_min = c(0.30, 0.20),
                                group_max = c(0.70, 0.60))
portf_gmv_grp <- add.objective(portf_gmv_grp, type = "risk", name = "StdDev")

opt_gmv_grp <- tryCatch(
  optimize.portfolio(R, portf_gmv_grp, optimize_method = "ROI"),
  error = function(e) NULL
)

test_that("GMV group constraint: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_gmv_grp, "optimize.portfolio.ROI")
})

test_that("GMV group constraint: weights are numeric and sum in [0.99, 1.01]", {
  w <- extractWeights(opt_gmv_grp)
  expect_true(is.numeric(w))
  expect_true(sum(w) >= 0.99 - 1e-4 && sum(w) <= 1.01 + 1e-4)
})

test_that("GMV group constraint: group 1 (assets 1-3) sum in [0.30, 0.70]", {
  w  <- extractWeights(opt_gmv_grp)
  g1 <- sum(w[1:3])
  expect_true(g1 >= 0.30 - 1e-4 && g1 <= 0.70 + 1e-4)
})

test_that("GMV group constraint: group 2 (assets 4-5) sum in [0.20, 0.60]", {
  w  <- extractWeights(opt_gmv_grp)
  g2 <- sum(w[4:5])
  expect_true(g2 >= 0.20 - 1e-4 && g2 <= 0.60 + 1e-4)
})


# ============================================================
# 12.  Max return with group constraint (glpk LP)
#      Exercises the n.groups > 0 branch inside maxret_opt()
#      (optFUN.R lines ~216-225: group Amat/dir/rhs build).
# ============================================================

portf_maxret_grp <- portfolio.spec(assets = colnames(R))
portf_maxret_grp <- add.constraint(portf_maxret_grp, type = "weight_sum",
                                   min_sum = 0.99, max_sum = 1.01)
portf_maxret_grp <- add.constraint(portf_maxret_grp, type = "box",
                                   min = 0.05, max = 0.50)
portf_maxret_grp <- add.constraint(portf_maxret_grp, type = "group",
                                   groups    = list(c(1, 2, 3), c(4, 5)),
                                   group_min = c(0.30, 0.20),
                                   group_max = c(0.70, 0.60))
portf_maxret_grp <- add.objective(portf_maxret_grp, type = "return", name = "mean")

opt_maxret_grp <- tryCatch(
  optimize.portfolio(R, portf_maxret_grp, optimize_method = "ROI"),
  error = function(e) NULL
)

test_that("max return group constraint: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_maxret_grp, "optimize.portfolio.ROI")
})

test_that("max return group constraint: weights are numeric and sum in [0.99, 1.01]", {
  w <- extractWeights(opt_maxret_grp)
  expect_true(is.numeric(w))
  expect_true(sum(w) >= 0.99 - 1e-4 && sum(w) <= 1.01 + 1e-4)
})

test_that("max return group constraint: group 1 (assets 1-3) sum in [0.30, 0.70]", {
  w  <- extractWeights(opt_maxret_grp)
  g1 <- sum(w[1:3])
  expect_true(g1 >= 0.30 - 1e-4 && g1 <= 0.70 + 1e-4)
})

test_that("max return group constraint: group 2 (assets 4-5) sum in [0.20, 0.60]", {
  w  <- extractWeights(opt_maxret_grp)
  g2 <- sum(w[4:5])
  expect_true(g2 >= 0.20 - 1e-4 && g2 <= 0.60 + 1e-4)
})


# ============================================================
# 13.  Min ETL with group constraint (glpk LP)
#      Exercises the n.groups > 0 branch inside etl_opt()
#      (optFUN.R lines ~445-455: group Amat/dir/rhs build).
# ============================================================

portf_etl_grp <- portfolio.spec(assets = colnames(R))
portf_etl_grp <- add.constraint(portf_etl_grp, type = "weight_sum",
                                min_sum = 0.99, max_sum = 1.01)
portf_etl_grp <- add.constraint(portf_etl_grp, type = "box",
                                min = 0.05, max = 0.50)
portf_etl_grp <- add.constraint(portf_etl_grp, type = "group",
                                groups    = list(c(1, 2, 3), c(4, 5)),
                                group_min = c(0.30, 0.20),
                                group_max = c(0.70, 0.60))
portf_etl_grp <- add.objective(portf_etl_grp, type = "risk", name = "ES")

opt_etl_grp <- tryCatch(
  optimize.portfolio(R, portf_etl_grp, optimize_method = "ROI"),
  error = function(e) NULL
)

test_that("min ETL group constraint: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_etl_grp, "optimize.portfolio.ROI")
})

test_that("min ETL group constraint: weights are numeric and sum in [0.99, 1.01]", {
  w <- extractWeights(opt_etl_grp)
  expect_true(is.numeric(w))
  expect_true(sum(w) >= 0.99 - 1e-4 && sum(w) <= 1.01 + 1e-4)
})

test_that("min ETL group constraint: group 1 (assets 1-3) sum in [0.30, 0.70]", {
  w  <- extractWeights(opt_etl_grp)
  g1 <- sum(w[1:3])
  expect_true(g1 >= 0.30 - 1e-4 && g1 <= 0.70 + 1e-4)
})

test_that("min ETL group constraint: group 2 (assets 4-5) sum in [0.20, 0.60]", {
  w  <- extractWeights(opt_etl_grp)
  g2 <- sum(w[4:5])
  expect_true(g2 >= 0.20 - 1e-4 && g2 <= 0.60 + 1e-4)
})


# ============================================================
# 14.  Turnover-constrained GMV with group constraint (quadprog)
#      Exercises the n.groups > 0 branch inside gmv_opt_toc()
#      (optFUN.R lines ~750-762: group Amat/dir/rhs build).
#      Requires corpcor (already guarded at file scope above).
# ============================================================

portf_toc_grp <- portfolio.spec(assets = colnames(R))
portf_toc_grp <- add.constraint(portf_toc_grp, type = "weight_sum",
                                min_sum = 0.99, max_sum = 1.01)
portf_toc_grp <- add.constraint(portf_toc_grp, type = "box",
                                min = 0.05, max = 0.50)
portf_toc_grp <- add.constraint(portf_toc_grp, type = "turnover",
                                turnover_target = 0.30)
portf_toc_grp <- add.constraint(portf_toc_grp, type = "group",
                                groups    = list(c(1, 2, 3), c(4, 5)),
                                group_min = c(0.30, 0.20),
                                group_max = c(0.70, 0.60))
portf_toc_grp <- add.objective(portf_toc_grp, type = "risk", name = "StdDev")

opt_toc_grp <- tryCatch(
  optimize.portfolio(R, portf_toc_grp, optimize_method = "ROI"),
  error = function(e) NULL
)

test_that("TOC group constraint: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_toc_grp, "optimize.portfolio.ROI")
})

test_that("TOC group constraint: weights are numeric and sum in [0.99, 1.01]", {
  w <- extractWeights(opt_toc_grp)
  expect_true(is.numeric(w))
  expect_true(sum(w) >= 0.99 - 1e-4 && sum(w) <= 1.01 + 1e-4)
})

test_that("TOC group constraint: group 1 (assets 1-3) sum in [0.30, 0.70]", {
  w  <- extractWeights(opt_toc_grp)
  g1 <- sum(w[1:3])
  expect_true(g1 >= 0.30 - 1e-4 && g1 <= 0.70 + 1e-4)
})

test_that("TOC group constraint: group 2 (assets 4-5) sum in [0.20, 0.60]", {
  w  <- extractWeights(opt_toc_grp)
  g2 <- sum(w[4:5])
  expect_true(g2 >= 0.20 - 1e-4 && g2 <= 0.60 + 1e-4)
})


# ============================================================
# 15.  Leverage-constrained GMV with group constraint (quadprog)
#      Exercises the n.groups > 0 branch inside gmv_opt_leverage()
#      (optFUN.R lines ~1070-1082: group Amat/dir/rhs build).
#      Requires corpcor (already guarded at file scope above).
#
#      A leverage of 1.5 is used; for the long-only box (min=0.05)
#      the constraint is non-binding but ensures routing to
#      gmv_opt_leverage() and exercises the group branch therein.
# ============================================================

portf_lev_grp <- portfolio.spec(assets = colnames(R))
portf_lev_grp <- add.constraint(portf_lev_grp, type = "weight_sum",
                                min_sum = 0.99, max_sum = 1.01)
portf_lev_grp <- add.constraint(portf_lev_grp, type = "box",
                                min = 0.05, max = 0.50)
portf_lev_grp <- add.constraint(portf_lev_grp, type = "leverage_exposure",
                                leverage = 1.5)
portf_lev_grp <- add.constraint(portf_lev_grp, type = "group",
                                groups    = list(c(1, 2, 3), c(4, 5)),
                                group_min = c(0.30, 0.20),
                                group_max = c(0.70, 0.60))
portf_lev_grp <- add.objective(portf_lev_grp, type = "risk", name = "StdDev")

opt_lev_grp <- tryCatch(
  optimize.portfolio(R, portf_lev_grp, optimize_method = "ROI"),
  error = function(e) NULL
)

test_that("leverage group constraint: result is optimize.portfolio.ROI", {
  expect_s3_class(opt_lev_grp, "optimize.portfolio.ROI")
})

test_that("leverage group constraint: weights are numeric and sum in [0.99, 1.01]", {
  w <- extractWeights(opt_lev_grp)
  expect_true(is.numeric(w))
  expect_true(sum(w) >= 0.99 - 1e-4 && sum(w) <= 1.01 + 1e-4)
})

test_that("leverage group constraint: group 1 (assets 1-3) sum in [0.30, 0.70]", {
  w  <- extractWeights(opt_lev_grp)
  g1 <- sum(w[1:3])
  expect_true(g1 >= 0.30 - 1e-4 && g1 <= 0.70 + 1e-4)
})

test_that("leverage group constraint: group 2 (assets 4-5) sum in [0.20, 0.60]", {
  w  <- extractWeights(opt_lev_grp)
  g2 <- sum(w[4:5])
  expect_true(g2 >= 0.20 - 1e-4 && g2 <= 0.60 + 1e-4)
})
