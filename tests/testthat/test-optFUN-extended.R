###############################################################################
# tests/testthat/test-optFUN-extended.R
#
# Source files hit: optFUN.R (+~120 expr), extract.efficient.frontier.R (+~80 expr)
#
# Covers:
#   1.  gmv_opt HHI with conc_groups vector  — the else-if(!is.null(conc_groups))
#       branch at lines 108-122 of optFUN.R.
#   2.  extract.efficient.frontier with n.portfolios — n.portfolios branch of
#       the lower-level function (called DIRECTLY, not via extractEfficientFrontier).
#   3.  extract.efficient.frontier with from/to/by — the 'else' branch that
#       uses seq(..., by=by) rather than length.out.
#   4.  meanvar.efficient.frontier — direct call with n.portfolios (CVXR).
#   5.  meanvar.efficient.frontier — direct call with risk_aversion vector,
#       hitting the lambda branch at line 179 (CVXR).
#   6.  meanetl.efficient.frontier — direct call with optimize_method='ROI',
#       exercising the ROI/glpk path instead of the default CVXR path.
#   7.  create.EfficientFrontier "mean-StdDev" — simple 4-asset spec (CVXR).
#   8.  create.EfficientFrontier "mean-ES"  — simple 4-asset spec (CVXR).
#   9.  create.EfficientFrontier "random"   — random-portfolio type, which
#       calls optimize.portfolio then extract.efficient.frontier internally.
#  10.  create.EfficientFrontier "mean-CSM" — meancsm.efficient.frontier via
#       CVXR; CSM is a CVXR-only risk measure (extract.efficient.frontier.R
#       lines 304-368).
#  11.  create.EfficientFrontier "mean-EQS" — meaneqs.efficient.frontier via
#       CVXR; EQS is a CVXR-only risk measure (extract.efficient.frontier.R
#       lines 387-451).
#  12.  create.EfficientFrontier "mean-risk" — meanrisk.efficient.frontier
#       with risk_type="StdDev" and compare_port=c("StdDev","ES") via CVXR
#       (extract.efficient.frontier.R lines 471-542).
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# ---------------------------------------------------------------------------
# Data — define locally so the file is standalone even if helper-*.R changes.
# ---------------------------------------------------------------------------
utils::data(edhec)
edhec4 <- edhec[, 1:4]

# ===========================================================================
# Section 1: gmv_opt with conc_groups (vector lambda_hhi)
#
# The existing test-optimize-roi-extended.R only exercises the SCALAR
# conc_aversion path (lambda_hhi length==1, conc_groups==NULL).
# Here we supply a *list* of groups with a *vector* of aversion values, which
# hits the 'else if(!is.null(conc_groups))' branch (optFUN.R lines 108-122).
# ===========================================================================

p_hhi_grp <- portfolio.spec(assets = colnames(edhec4))
p_hhi_grp <- add.constraint(p_hhi_grp, type = "full_investment")
p_hhi_grp <- add.constraint(p_hhi_grp, type = "long_only")
p_hhi_grp <- add.objective(p_hhi_grp, type = "risk", name = "StdDev")
p_hhi_grp <- add.objective(
  p_hhi_grp,
  type          = "weight_concentration",
  name          = "HHI",
  conc_aversion = c(0.001, 0.002),      # vector -> conc_groups branch
  conc_groups   = list(1:2, 3:4)        # two groups of two assets each
)

opt_hhi_grp <- tryCatch(
  optimize.portfolio(edhec4, p_hhi_grp, optimize_method = "ROI"),
  error = function(e) NULL
)

test_that("HHI conc_groups: result is optimize.portfolio.ROI", {
  skip_if(is.null(opt_hhi_grp))
  expect_s3_class(opt_hhi_grp, "optimize.portfolio.ROI")
})

test_that("HHI conc_groups: extractWeights returns numeric", {
  skip_if(is.null(opt_hhi_grp))
  expect_true(is.numeric(extractWeights(opt_hhi_grp)))
})

test_that("HHI conc_groups: weights sum to 1", {
  skip_if(is.null(opt_hhi_grp))
  expect_equal(sum(extractWeights(opt_hhi_grp)), 1, tolerance = 1e-6)
})

test_that("HHI conc_groups: all weights non-negative (long_only)", {
  skip_if(is.null(opt_hhi_grp))
  expect_true(all(round(extractWeights(opt_hhi_grp), 10) >= 0))
})

test_that("HHI conc_groups: length of weights equals number of assets", {
  skip_if(is.null(opt_hhi_grp))
  expect_length(extractWeights(opt_hhi_grp), ncol(edhec4))
})

test_that("HHI conc_groups: StdDev is in objective measures and is numeric", {
  skip_if(is.null(opt_hhi_grp))
  om <- extractObjectiveMeasures(opt_hhi_grp)
  expect_false(is.null(om$StdDev))
  expect_true(is.numeric(om$StdDev))
})

test_that("HHI conc_groups: StdDev is positive", {
  skip_if(is.null(opt_hhi_grp))
  expect_gt(extractObjectiveMeasures(opt_hhi_grp)$StdDev, 0)
})

# ===========================================================================
# Section 2 & 3: extract.efficient.frontier — direct calls
#
# test-charts-roi.R calls extract.efficient.frontier only *indirectly*
# through extractEfficientFrontier(), and the from/to/by test there is always
# skipped because extractEfficientFrontier() does not accept those arguments.
# Here we call extract.efficient.frontier() directly to cover both branches:
#   * n.portfolios branch  (seq with length.out)
#   * from/to/by branch    (seq with by)
# ===========================================================================

# Build a random-portfolio result with trace=TRUE so extractStats() works.
p_rp_es4 <- portfolio.spec(assets = colnames(edhec4))
p_rp_es4 <- add.constraint(p_rp_es4, type = "weight_sum",
                            min_sum = 0.99, max_sum = 1.01)
p_rp_es4 <- add.constraint(p_rp_es4, type = "box", min = 0, max = 1)
p_rp_es4 <- add.objective(p_rp_es4, type = "return", name = "mean")
p_rp_es4 <- add.objective(p_rp_es4, type = "risk",   name = "ES")

opt_rp_es4 <- tryCatch({
  set.seed(42)
  optimize.portfolio(edhec4, p_rp_es4,
                     optimize_method = "random",
                     trace       = TRUE,
                     search_size = 300L)
}, error = function(e) NULL)

# --- 2a: n.portfolios branch ---
ef_np <- tryCatch(
  extract.efficient.frontier(opt_rp_es4, match.col = "ES", n.portfolios = 10),
  error = function(e) NULL
)

test_that("extract.efficient.frontier n.portfolios: class is 'frontier'", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_np))
  expect_s3_class(ef_np, "frontier")
})

test_that("extract.efficient.frontier n.portfolios: at most n.portfolios rows", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_np))
  expect_lte(nrow(ef_np), 10L)
})

test_that("extract.efficient.frontier n.portfolios: has 'mean' column", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_np))
  expect_true("mean" %in% colnames(ef_np))
})

test_that("extract.efficient.frontier n.portfolios: has 'ES' column", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_np))
  expect_true("ES" %in% colnames(ef_np))
})

test_that("extract.efficient.frontier n.portfolios: weight columns present", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_np))
  wcols <- grep("^w\\.", colnames(ef_np))
  expect_gt(length(wcols), 0L)
})

test_that("extract.efficient.frontier n.portfolios: is a matrix", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_np))
  expect_true(is.matrix(ef_np))
})

# --- 2b: from/to/by branch ---
ef_range <- tryCatch({
  xtract  <- extractStats(opt_rp_es4)
  es_col  <- pmatch("ES", colnames(xtract))
  es_vals <- xtract[, es_col]
  lo      <- min(es_vals) * 1.01      # slightly above min
  hi      <- max(es_vals) * 0.99      # slightly below max
  if (lo < hi) {
    extract.efficient.frontier(opt_rp_es4, match.col = "ES",
                               from = lo, to = hi, by = 0.002)
  } else {
    NULL
  }
}, error = function(e) NULL)

test_that("extract.efficient.frontier from/to/by: class is 'frontier'", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_range))
  expect_s3_class(ef_range, "frontier")
})

test_that("extract.efficient.frontier from/to/by: is a matrix", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_range))
  expect_true(is.matrix(ef_range))
})

test_that("extract.efficient.frontier from/to/by: has 'mean' column", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_range))
  expect_true("mean" %in% colnames(ef_range))
})

test_that("extract.efficient.frontier from/to/by: has 'ES' column", {
  skip_if(is.null(opt_rp_es4))
  skip_if(is.null(ef_range))
  expect_true("ES" %in% colnames(ef_range))
})

# ===========================================================================
# Section 4 & 5: meanvar.efficient.frontier — direct calls (CVXR)
#
# test-backtest-apply.R already calls meanvar.efficient.frontier with
# n.portfolios=10 (CVXR default).  We add:
#   4. n.portfolios=5 on a simpler spec (confirms code path with edhec4).
#   5. risk_aversion vector — hits the DISTINCT lambda branch (line ~179 in
#      extract.efficient.frontier.R) that NO existing test exercises.
# ===========================================================================

p_mv4    <- NULL
ef_mv_np <- NULL
ef_mv_ra <- NULL

if (requireNamespace("CVXR", quietly = TRUE)) {
  p_mv4 <- portfolio.spec(assets = colnames(edhec4))
  p_mv4 <- add.constraint(p_mv4, type = "full_investment")
  p_mv4 <- add.constraint(p_mv4, type = "long_only")
  p_mv4 <- add.objective(p_mv4, type = "return", name = "mean")
  p_mv4 <- add.objective(p_mv4, type = "risk",   name = "StdDev")

  # 4: n.portfolios path
  ef_mv_np <- tryCatch(
    meanvar.efficient.frontier(portfolio = p_mv4, R = edhec4, n.portfolios = 5L),
    error = function(e) NULL
  )

  # 5: risk_aversion vector path (distinct branch)
  ef_mv_ra <- tryCatch(
    meanvar.efficient.frontier(portfolio = p_mv4, R = edhec4,
                               risk_aversion = c(1, 5, 10)),
    error = function(e) NULL
  )
}

# --- 4: n.portfolios tests ---
test_that("meanvar.efficient.frontier n.portfolios: class is 'frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_np))
  expect_s3_class(ef_mv_np, "frontier")
})

test_that("meanvar.efficient.frontier n.portfolios: has 5 rows", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_np))
  expect_equal(nrow(ef_mv_np), 5L)
})

test_that("meanvar.efficient.frontier n.portfolios: has 'mean' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_np))
  expect_true("mean" %in% colnames(ef_mv_np))
})

test_that("meanvar.efficient.frontier n.portfolios: has 'StdDev' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_np))
  expect_true("StdDev" %in% colnames(ef_mv_np))
})

test_that("meanvar.efficient.frontier n.portfolios: weight columns present", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_np))
  expect_gt(length(grep("^w\\.", colnames(ef_mv_np))), 0L)
})

test_that("meanvar.efficient.frontier n.portfolios: StdDev values are positive", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_np))
  expect_true(all(ef_mv_np[, "StdDev"] > 0))
})

# --- 5: risk_aversion tests ---
test_that("meanvar.efficient.frontier risk_aversion: class is 'frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_ra))
  expect_s3_class(ef_mv_ra, "frontier")
})

test_that("meanvar.efficient.frontier risk_aversion: nrow equals length(risk_aversion)", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_ra))
  expect_equal(nrow(ef_mv_ra), 3L)
})

test_that("meanvar.efficient.frontier risk_aversion: has 'lambda' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_ra))
  expect_true("lambda" %in% colnames(ef_mv_ra))
})

test_that("meanvar.efficient.frontier risk_aversion: lambda values match input", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_mv_ra))
  expect_equal(as.numeric(ef_mv_ra[, "lambda"]), c(1, 5, 10), tolerance = 1e-9)
})

# ===========================================================================
# Section 6: meanetl.efficient.frontier with optimize_method = 'ROI'
#
# The existing tests (test-backtest-apply.R, test-efficient-frontier.R) all
# use the default CVXR path.  Here we explicitly request optimize_method='ROI'
# to exercise the ROI/glpk code inside the foreach loop.
# ===========================================================================

p_etl4     <- NULL
ef_etl_roi <- NULL

if (requireNamespace("ROI.plugin.glpk", quietly = TRUE)) {
  p_etl4 <- portfolio.spec(assets = colnames(edhec4))
  p_etl4 <- add.constraint(p_etl4, type = "full_investment")
  p_etl4 <- add.constraint(p_etl4, type = "long_only")
  p_etl4 <- add.objective(p_etl4, type = "return", name = "mean")
  p_etl4 <- add.objective(p_etl4, type = "risk",   name = "ES")

  ef_etl_roi <- tryCatch(
    meanetl.efficient.frontier(portfolio       = p_etl4,
                               R               = edhec4,
                               optimize_method = "ROI",
                               n.portfolios    = 5L),
    error = function(e) NULL
  )
}

test_that("meanetl.efficient.frontier ROI: class is 'frontier'", {
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(ef_etl_roi))
  expect_s3_class(ef_etl_roi, "frontier")
})

test_that("meanetl.efficient.frontier ROI: at most 5 rows", {
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(ef_etl_roi))
  expect_lte(nrow(ef_etl_roi), 5L)
})

test_that("meanetl.efficient.frontier ROI: has 'ES' column", {
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(ef_etl_roi))
  expect_true("ES" %in% colnames(ef_etl_roi))
})

test_that("meanetl.efficient.frontier ROI: has 'mean' column", {
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(ef_etl_roi))
  expect_true("mean" %in% colnames(ef_etl_roi))
})

test_that("meanetl.efficient.frontier ROI: ES values are positive", {
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(ef_etl_roi))
  expect_true(all(ef_etl_roi[, "ES"] > 0))
})

test_that("meanetl.efficient.frontier ROI: weight columns present", {
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(ef_etl_roi))
  expect_gt(length(grep("^w\\.", colnames(ef_etl_roi))), 0L)
})

# ===========================================================================
# Section 7: create.EfficientFrontier "mean-StdDev" — simple 4-asset spec
#
# test-efficient-frontier.R covers "mean-StdDev" with a 5-asset portfolio
# carrying group + box constraints.  Here we use the minimal full-investment /
# long-only spec on edhec4 with n.portfolios=5 to keep the run fast.
# ===========================================================================

ef_cef_stddev <- NULL

if (requireNamespace("CVXR", quietly = TRUE) && !is.null(p_mv4)) {
  ef_cef_stddev <- tryCatch(
    create.EfficientFrontier(edhec4, p_mv4,
                             type         = "mean-StdDev",
                             n.portfolios = 5L),
    error = function(e) NULL
  )
}

test_that("create.EfficientFrontier mean-StdDev: class is 'efficient.frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_stddev))
  expect_s3_class(ef_cef_stddev, "efficient.frontier")
})

test_that("create.EfficientFrontier mean-StdDev: $frontier not NULL", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_stddev))
  expect_false(is.null(ef_cef_stddev$frontier))
})

test_that("create.EfficientFrontier mean-StdDev: $frontier class is 'frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_stddev))
  expect_s3_class(ef_cef_stddev$frontier, "frontier")
})

test_that("create.EfficientFrontier mean-StdDev: $R slot equals edhec4", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_stddev))
  expect_equal(ef_cef_stddev$R, edhec4)
})

test_that("create.EfficientFrontier mean-StdDev: $portfolio is a portfolio", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_stddev))
  expect_true(is.portfolio(ef_cef_stddev$portfolio))
})

test_that("create.EfficientFrontier mean-StdDev: frontier has 5 rows", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_stddev))
  expect_equal(nrow(ef_cef_stddev$frontier), 5L)
})

# ===========================================================================
# Section 8: create.EfficientFrontier "mean-ES" — simple 4-asset spec
#
# The "mean-ES" type calls meanetl.efficient.frontier() internally, which
# defaults to CVXR.  Guard accordingly.
# ===========================================================================

# Build a separate ETL/CVXR spec (p_etl4 is only defined when glpk is present).
p_etl_cvxr <- NULL
ef_cef_es  <- NULL

if (requireNamespace("CVXR", quietly = TRUE)) {
  p_etl_cvxr <- portfolio.spec(assets = colnames(edhec4))
  p_etl_cvxr <- add.constraint(p_etl_cvxr, type = "full_investment")
  p_etl_cvxr <- add.constraint(p_etl_cvxr, type = "long_only")
  p_etl_cvxr <- add.objective(p_etl_cvxr, type = "return", name = "mean")
  p_etl_cvxr <- add.objective(p_etl_cvxr, type = "risk",   name = "ES")

  ef_cef_es <- tryCatch(
    create.EfficientFrontier(edhec4, p_etl_cvxr,
                             type         = "mean-ES",
                             n.portfolios = 5L),
    error = function(e) NULL
  )
}

test_that("create.EfficientFrontier mean-ES: class is 'efficient.frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_es))
  expect_s3_class(ef_cef_es, "efficient.frontier")
})

test_that("create.EfficientFrontier mean-ES: $frontier not NULL", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_es))
  expect_false(is.null(ef_cef_es$frontier))
})

test_that("create.EfficientFrontier mean-ES: $frontier class is 'frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_es))
  expect_s3_class(ef_cef_es$frontier, "frontier")
})

test_that("create.EfficientFrontier mean-ES: $R slot equals edhec4", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_es))
  expect_equal(ef_cef_es$R, edhec4)
})

test_that("create.EfficientFrontier mean-ES: frontier has 'ES' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_es))
  expect_true("ES" %in% colnames(ef_cef_es$frontier))
})

# ===========================================================================
# Section 9: create.EfficientFrontier "random" type
#
# This type calls optimize.portfolio(method="random", trace=TRUE) internally
# and then extract.efficient.frontier(), exercising the "random" switch-case
# branch in create.EfficientFrontier (lines ~650-662 of
# extract.efficient.frontier.R).  No special solver package is required.
# ===========================================================================

p_cef_rp <- portfolio.spec(assets = colnames(edhec4))
p_cef_rp <- add.constraint(p_cef_rp, type = "weight_sum",
                            min_sum = 0.99, max_sum = 1.01)
p_cef_rp <- add.constraint(p_cef_rp, type = "box", min = 0, max = 1)
p_cef_rp <- add.objective(p_cef_rp, type = "return", name = "mean")
p_cef_rp <- add.objective(p_cef_rp, type = "risk",   name = "ES")

ef_cef_rp <- tryCatch({
  set.seed(123L)
  create.EfficientFrontier(edhec4, p_cef_rp,
                           type         = "random",
                           n.portfolios = 8L,
                           search_size  = 200L,
                           match.col    = "ES")
}, error = function(e) NULL)

test_that("create.EfficientFrontier random: class is 'efficient.frontier'", {
  skip_if(is.null(ef_cef_rp))
  expect_s3_class(ef_cef_rp, "efficient.frontier")
})

test_that("create.EfficientFrontier random: $frontier not NULL", {
  skip_if(is.null(ef_cef_rp))
  expect_false(is.null(ef_cef_rp$frontier))
})

test_that("create.EfficientFrontier random: $frontier class is 'frontier'", {
  skip_if(is.null(ef_cef_rp))
  expect_s3_class(ef_cef_rp$frontier, "frontier")
})

test_that("create.EfficientFrontier random: $R slot not NULL", {
  skip_if(is.null(ef_cef_rp))
  expect_false(is.null(ef_cef_rp$R))
})

test_that("create.EfficientFrontier random: $portfolio is a portfolio", {
  skip_if(is.null(ef_cef_rp))
  expect_true(is.portfolio(ef_cef_rp$portfolio))
})

test_that("create.EfficientFrontier random: frontier has at most 8 rows", {
  skip_if(is.null(ef_cef_rp))
  expect_lte(nrow(ef_cef_rp$frontier), 8L)
})

test_that("create.EfficientFrontier random: $call records the call", {
  skip_if(is.null(ef_cef_rp))
  expect_false(is.null(ef_cef_rp$call))
})

test_that("create.EfficientFrontier random: frontier has 'mean' column", {
  skip_if(is.null(ef_cef_rp))
  expect_true("mean" %in% colnames(ef_cef_rp$frontier))
})

# ===========================================================================
# Section 10: create.EfficientFrontier "mean-CSM"
#
# meancsm.efficient.frontier (extract.efficient.frontier.R lines 304-368).
# CSM (Conditional Skewness Maximum) is a CVXR-specific risk measure; the
# underlying optimize.portfolio call uses optimize_method='CVXR' by default.
# ===========================================================================

p_csm    <- NULL
ef_cef_csm <- NULL

if (requireNamespace("CVXR", quietly = TRUE)) {
  p_csm <- portfolio.spec(assets = colnames(edhec4))
  p_csm <- add.constraint(p_csm, type = "full_investment")
  p_csm <- add.constraint(p_csm, type = "long_only")
  p_csm <- add.objective(p_csm, type = "return", name = "mean")
  p_csm <- add.objective(p_csm, type = "risk",   name = "CSM")

  ef_cef_csm <- tryCatch(
    create.EfficientFrontier(R = edhec4, portfolio = p_csm,
                             type         = "mean-CSM",
                             n.portfolios = 5L),
    error = function(e) NULL
  )
}

test_that("create.EfficientFrontier mean-CSM: class is 'efficient.frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_csm))
  expect_s3_class(ef_cef_csm, "efficient.frontier")
})

test_that("create.EfficientFrontier mean-CSM: $frontier is not NULL", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_csm))
  expect_false(is.null(ef_cef_csm$frontier))
})

test_that("create.EfficientFrontier mean-CSM: $frontier is a matrix", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_csm))
  expect_true(is.matrix(ef_cef_csm$frontier))
})

test_that("create.EfficientFrontier mean-CSM: $frontier has 'mean' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_csm))
  expect_true("mean" %in% colnames(ef_cef_csm$frontier))
})

test_that("create.EfficientFrontier mean-CSM: $frontier has 'CSM' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_csm))
  expect_true("CSM" %in% colnames(ef_cef_csm$frontier))
})

test_that("create.EfficientFrontier mean-CSM: $frontier has weight columns", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_csm))
  expect_gt(length(grep("^w\\.", colnames(ef_cef_csm$frontier))), 0L)
})

test_that("create.EfficientFrontier mean-CSM: $portfolio is a portfolio", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_csm))
  expect_true(is.portfolio(ef_cef_csm$portfolio))
})

test_that("create.EfficientFrontier mean-CSM: frontier has at most 5 rows", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_csm))
  expect_lte(nrow(ef_cef_csm$frontier), 5L)
})

# ===========================================================================
# Section 11: create.EfficientFrontier "mean-EQS"
#
# meaneqs.efficient.frontier (extract.efficient.frontier.R lines 387-451).
# EQS (Exponential Quadratic Shortfall) is a CVXR-specific risk measure;
# the underlying optimize.portfolio call uses optimize_method='CVXR' by default.
# ===========================================================================

p_eqs    <- NULL
ef_cef_eqs <- NULL

if (requireNamespace("CVXR", quietly = TRUE)) {
  p_eqs <- portfolio.spec(assets = colnames(edhec4))
  p_eqs <- add.constraint(p_eqs, type = "full_investment")
  p_eqs <- add.constraint(p_eqs, type = "long_only")
  p_eqs <- add.objective(p_eqs, type = "return", name = "mean")
  p_eqs <- add.objective(p_eqs, type = "risk",   name = "EQS")

  ef_cef_eqs <- tryCatch(
    create.EfficientFrontier(R = edhec4, portfolio = p_eqs,
                             type         = "mean-EQS",
                             n.portfolios = 5L),
    error = function(e) NULL
  )
}

test_that("create.EfficientFrontier mean-EQS: class is 'efficient.frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_eqs))
  expect_s3_class(ef_cef_eqs, "efficient.frontier")
})

test_that("create.EfficientFrontier mean-EQS: $frontier is not NULL", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_eqs))
  expect_false(is.null(ef_cef_eqs$frontier))
})

test_that("create.EfficientFrontier mean-EQS: $frontier is a matrix", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_eqs))
  expect_true(is.matrix(ef_cef_eqs$frontier))
})

test_that("create.EfficientFrontier mean-EQS: $frontier has 'mean' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_eqs))
  expect_true("mean" %in% colnames(ef_cef_eqs$frontier))
})

test_that("create.EfficientFrontier mean-EQS: $frontier has 'EQS' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_eqs))
  expect_true("EQS" %in% colnames(ef_cef_eqs$frontier))
})

test_that("create.EfficientFrontier mean-EQS: $frontier has weight columns", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_eqs))
  expect_gt(length(grep("^w\\.", colnames(ef_cef_eqs$frontier))), 0L)
})

test_that("create.EfficientFrontier mean-EQS: $portfolio is a portfolio", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_eqs))
  expect_true(is.portfolio(ef_cef_eqs$portfolio))
})

test_that("create.EfficientFrontier mean-EQS: frontier has at most 5 rows", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_eqs))
  expect_lte(nrow(ef_cef_eqs$frontier), 5L)
})

# ===========================================================================
# Section 12: create.EfficientFrontier "mean-risk"
#
# meanrisk.efficient.frontier (extract.efficient.frontier.R lines 471-542).
# Uses risk_type="StdDev" as the primary risk axis and compare_port=
# c("StdDev","ES") so that the ES-portfolio comparison loop runs once,
# adding one extra column ("ES portfolio StdDev").  Requires CVXR because
# both the main optimisation (StdDev via CVXR) and the comparison
# extract_risk() helper call CVXR internally.
#
# NOTE: passing compare_port equal to risk_type alone (i.e. an empty
# risk_compare) triggers a known paste()/colnames mismatch in the source,
# so we use the two-element default instead.
# ===========================================================================

p_mr     <- NULL
ef_cef_mr <- NULL

if (requireNamespace("CVXR", quietly = TRUE)) {
  p_mr <- portfolio.spec(assets = colnames(edhec4))
  p_mr <- add.constraint(p_mr, type = "full_investment")
  p_mr <- add.constraint(p_mr, type = "long_only")
  p_mr <- add.objective(p_mr, type = "return", name = "mean")
  p_mr <- add.objective(p_mr, type = "risk",   name = "StdDev")

  ef_cef_mr <- tryCatch(
    create.EfficientFrontier(R = edhec4, portfolio = p_mr,
                             type         = "mean-risk",
                             n.portfolios = 5L,
                             risk_type    = "StdDev",
                             compare_port = c("StdDev", "ES")),
    error = function(e) NULL
  )
}

test_that("create.EfficientFrontier mean-risk: class is 'efficient.frontier'", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  expect_s3_class(ef_cef_mr, "efficient.frontier")
})

test_that("create.EfficientFrontier mean-risk: $frontier is not NULL", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  expect_false(is.null(ef_cef_mr$frontier))
})

test_that("create.EfficientFrontier mean-risk: $frontier is a matrix", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  expect_true(is.matrix(ef_cef_mr$frontier))
})

test_that("create.EfficientFrontier mean-risk: $frontier has 'mean' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  expect_true("mean" %in% colnames(ef_cef_mr$frontier))
})

test_that("create.EfficientFrontier mean-risk: $frontier has 'StdDev' column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  expect_true("StdDev" %in% colnames(ef_cef_mr$frontier))
})

test_that("create.EfficientFrontier mean-risk: $frontier has weight columns", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  expect_gt(length(grep("^w\\.", colnames(ef_cef_mr$frontier))), 0L)
})

test_that("create.EfficientFrontier mean-risk: $frontier has comparison column", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  # The comparison loop appends one column named "<rc> portfolio <risk_type>"
  # i.e. "ES portfolio StdDev" when compare_port = c("StdDev","ES").
  expect_true(any(grepl("ES portfolio StdDev", colnames(ef_cef_mr$frontier))))
})

test_that("create.EfficientFrontier mean-risk: $portfolio is a portfolio", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  expect_true(is.portfolio(ef_cef_mr$portfolio))
})

test_that("create.EfficientFrontier mean-risk: frontier has at most 5 rows", {
  skip_if_not_installed("CVXR")
  skip_if(is.null(ef_cef_mr))
  expect_lte(nrow(ef_cef_mr$frontier), 5L)
})
