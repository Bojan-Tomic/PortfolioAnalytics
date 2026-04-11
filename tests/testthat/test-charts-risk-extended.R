###############################################################################
# tests/testthat/test-charts-risk-extended.R
#
# Extended tests for chart.RiskBudget methods in R/charts.risk.R
# Covers: rebalancing, opt.list, barplot, and neighbors branches
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)

# ---------------------------------------------------------------------------
# File-scope fixtures — built once, reused across all test_that blocks.
#
# trace = TRUE is REQUIRED for the neighbors-as-scalar / neighbors-as-vector
# tests because those code paths call extractStats(object) internally.
# ---------------------------------------------------------------------------

# --- Primary risk-budget portfolio (min_concentration, trace = TRUE) ---
.p_rb_ext <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.40)
  p <- add.objective(p, type = "risk_budget", name = "ES",
                     min_concentration = TRUE)
  p
})

set.seed(42)
opt_rb_ext <- tryCatch(
  optimize.portfolio(edhec4, .p_rb_ext,
                     optimize_method = "random",
                     search_size    = 500L,
                     trace          = TRUE),
  error = function(e) NULL
)

# --- Second portfolio (different bounds) for combine.optimizations ---
.p_rb_ext2 <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk_budget", name = "ES",
                     min_concentration = TRUE)
  p
})

set.seed(99)
opt_rb_ext2 <- tryCatch(
  optimize.portfolio(edhec4, .p_rb_ext2,
                     optimize_method = "random",
                     search_size    = 200L,
                     trace          = TRUE),
  error = function(e) NULL
)

# --- opt.list (needed for barplotRiskBudget and chart.RiskBudget.opt.list) ---
opt_rb_ext_list <- tryCatch(
  combine.optimizations(list(portf1 = opt_rb_ext, portf2 = opt_rb_ext2)),
  error = function(e) NULL
)

# --- Rebalancing result (random solver, uses tail() for portability) ---
opt_rb_ext_rebal <- tryCatch({
  # tail() ensures we always have 36 monthly obs regardless of edhec vintage
  R_rebal <- tail(edhec4, 36L)
  p_rbr <- portfolio.spec(assets = colnames(R_rebal))
  p_rbr <- add.constraint(p_rbr, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p_rbr <- add.constraint(p_rbr, type = "box", min = 0.05, max = 0.60)
  p_rbr <- add.objective(p_rbr, type = "risk_budget", name = "ES",
                         min_concentration = TRUE)
  set.seed(7)
  optimize.portfolio.rebalancing(R_rebal, p_rbr,
                                  optimize_method  = "random",
                                  rebalance_on    = "years",
                                  training_period = 12L,
                                  search_size     = 100L)
}, error = function(e) NULL)

# --- Portfolio with max_prisk: hits the max_prisk display branch (lines ~174) ---
.p_rb_maxp <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk_budget", name = "ES",
                     max_prisk = 0.40)
  p
})

set.seed(11)
opt_rb_maxp <- tryCatch(
  optimize.portfolio(edhec4, .p_rb_maxp,
                     optimize_method = "random",
                     search_size    = 200L),
  error = function(e) NULL
)

# --- Portfolio with min_prisk: hits the min_prisk display branch (lines ~171) ---
.p_rb_minp <- local({
  p <- portfolio.spec(assets = colnames(edhec4))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk_budget", name = "ES",
                     min_prisk = 0.10)
  p
})

set.seed(13)
opt_rb_minp <- tryCatch(
  optimize.portfolio(edhec4, .p_rb_minp,
                     optimize_method = "random",
                     search_size    = 200L),
  error = function(e) NULL
)

# --- Manually-constructed neighbors matrices (no trace required) ---
#
# For the matrix/data.frame neighbors path the chart function greps the
# *caller-supplied* matrix for column names matching "ES.contribution" or
# "ES.pct_contrib", so we only need correctly named columns — no solver needed.

# absolute: columns must contain "ES.contribution"
nb_abs_mat <- tryCatch({
  m <- matrix(
    data = c(0.0020, 0.0030, 0.0040, 0.0010,
             0.0030, 0.0020, 0.0030, 0.0020),
    nrow = 2L, ncol = 4L, byrow = TRUE
  )
  colnames(m) <- paste("ES.contribution", colnames(edhec4), sep = ".")
  m
}, error = function(e) NULL)

# pct_contrib: columns must contain "ES.pct_contrib"
nb_pct_mat <- tryCatch({
  m <- matrix(
    data = c(0.25, 0.25, 0.25, 0.25,
             0.30, 0.20, 0.30, 0.20),
    nrow = 2L, ncol = 4L, byrow = TRUE
  )
  colnames(m) <- paste("ES.pct_contrib", colnames(edhec4), sep = ".")
  m
}, error = function(e) NULL)

# =============================================================================
# Fixture integrity checks (no graphics)
# =============================================================================

test_that("opt_rb_ext is a random optimize.portfolio object with trace data", {
  skip_if(is.null(opt_rb_ext))
  expect_s3_class(opt_rb_ext, "optimize.portfolio.random")
  expect_false(is.null(opt_rb_ext$random_portfolio_objective_results),
               label = "trace results stored (trace = TRUE)")
})

test_that("opt_rb_ext$objective_measures has ES contribution and pct_contrib slots", {
  skip_if(is.null(opt_rb_ext))
  om <- opt_rb_ext$objective_measures
  expect_false(is.null(om$ES),
               label = "objective_measures$ES exists")
  expect_true(length(om$ES) > 1L,
              label = "ES has more than just the scalar value (contribution + pct_contrib)")
})

test_that("extractStats(opt_rb_ext) contains ES.contribution columns", {
  skip_if(is.null(opt_rb_ext))
  st <- extractStats(opt_rb_ext)
  rc <- grep("ES.contribution", colnames(st))
  expect_true(length(rc) > 0L,
              label = "extractStats output has at least one ES.contribution column")
  expect_equal(length(rc), ncol(edhec4),
               label = "one ES.contribution column per asset")
})

test_that("extractStats(opt_rb_ext) contains ES.pct_contrib columns", {
  skip_if(is.null(opt_rb_ext))
  st <- extractStats(opt_rb_ext)
  pc <- grep("ES.pct_contrib", colnames(st))
  expect_true(length(pc) > 0L,
              label = "extractStats output has at least one ES.pct_contrib column")
})

test_that("opt_rb_ext_list is an opt.list of length 2", {
  skip_if(is.null(opt_rb_ext_list))
  expect_s3_class(opt_rb_ext_list, "opt.list")
  expect_length(opt_rb_ext_list, 2L)
})

test_that("nb_abs_mat has one ES.contribution column per asset", {
  skip_if(is.null(nb_abs_mat))
  expect_equal(length(grep("ES.contribution", colnames(nb_abs_mat))), ncol(edhec4))
})

test_that("nb_pct_mat has one ES.pct_contrib column per asset", {
  skip_if(is.null(nb_pct_mat))
  expect_equal(length(grep("ES.pct_contrib", colnames(nb_pct_mat))), ncol(edhec4))
})

# =============================================================================
# chart.RiskBudget.optimize.portfolio — neighbors branches, absolute risk type
# Lines 122–154 in charts.risk.R
# Three sub-paths:
#   (a) is.vector && length == 1  → scalar neighbor count (lines 130–134)
#   (b) is.vector && length  > 1  → explicit portfolio indices (lines 135–140)
#   (c) is.matrix / is.data.frame → caller supplies the data (lines 141–153)
# =============================================================================

test_that("chart.RiskBudget absolute neighbors=scalar-int (length-1 path) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "absolute",
                     neighbors = 3L)
  )
})

test_that("chart.RiskBudget absolute neighbors=index-vector (length>1 path) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "absolute",
                     neighbors = c(1L, 2L, 5L))
  )
})

test_that("chart.RiskBudget absolute neighbors=matrix (matrix path) does not error", {
  skip_if(is.null(opt_rb_ext))
  skip_if(is.null(nb_abs_mat))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "absolute",
                     neighbors = nb_abs_mat)
  )
})

test_that("chart.RiskBudget absolute neighbors=data.frame (data.frame path) does not error", {
  skip_if(is.null(opt_rb_ext))
  skip_if(is.null(nb_abs_mat))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "absolute",
                     neighbors = as.data.frame(nb_abs_mat))
  )
})

# =============================================================================
# chart.RiskBudget.optimize.portfolio — neighbors branches, pct_contrib risk
# Lines 186–212 in charts.risk.R
# Same three sub-paths as above, but riskcols grep searches "ES.pct_contrib"
# =============================================================================

test_that("chart.RiskBudget pct_contrib neighbors=scalar-int (length-1 path) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "pct_contrib",
                     neighbors = 3L)
  )
})

test_that("chart.RiskBudget pct_contrib neighbors=index-vector (length>1 path) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "pct_contrib",
                     neighbors = c(1L, 2L, 5L))
  )
})

test_that("chart.RiskBudget pct_contrib neighbors=matrix (matrix path) does not error", {
  skip_if(is.null(opt_rb_ext))
  skip_if(is.null(nb_pct_mat))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "pct_contrib",
                     neighbors = nb_pct_mat)
  )
})

test_that("chart.RiskBudget pct_contrib neighbors=data.frame (data.frame path) does not error", {
  skip_if(is.null(opt_rb_ext))
  skip_if(is.null(nb_pct_mat))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "pct_contrib",
                     neighbors = as.data.frame(nb_pct_mat))
  )
})

# =============================================================================
# chart.RiskBudget.optimize.portfolio — max_prisk / min_prisk display branches
# Lines 170–179: inside the pct_contrib loop, non-NULL min_prisk / max_prisk
# trigger points() calls to overlay the constraint bounds on the chart.
# =============================================================================

test_that("chart.RiskBudget pct_contrib with max_prisk shows bound markers without error", {
  skip_if(is.null(opt_rb_maxp))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_maxp, risk.type = "pct_contrib")
  )
})

test_that("chart.RiskBudget pct_contrib with min_prisk shows bound markers without error", {
  skip_if(is.null(opt_rb_minp))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_minp, risk.type = "pct_contrib")
  )
})

# =============================================================================
# chart.RiskBudget.optimize.portfolio — margin / layout branches
# Lines 94–107:
#   minmargin = 5   when xlab is non-NULL (line 96)
#   topmargin = 1   when main == ""      (line 97)
#   bottommargin = minmargin (else)      when las <= 1 (line 107)
# =============================================================================

test_that("chart.RiskBudget absolute with xlab set (minmargin = 5 branch) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "absolute",
                     xlab      = "Portfolio Assets")
  )
})

test_that("chart.RiskBudget absolute with main='' (topmargin = 1 branch) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "absolute",
                     main      = "")
  )
})

test_that("chart.RiskBudget absolute with las=1 (bottommargin = minmargin else branch) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "absolute",
                     las       = 1L)
  )
})

test_that("chart.RiskBudget pct_contrib with main='' (topmargin = 1 branch) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "pct_contrib",
                     main      = "")
  )
})

test_that("chart.RiskBudget pct_contrib with las=1 (bottommargin else branch) does not error", {
  skip_if(is.null(opt_rb_ext))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext,
                     risk.type = "pct_contrib",
                     las       = 1L)
  )
})

# =============================================================================
# chart.RiskBudget.optimize.portfolio.rebalancing — random solver variant
# Lines 220–249: covers both risk.type branches with a random-method result
# =============================================================================

test_that("chart.RiskBudget rebalancing absolute (random solver) does not error", {
  skip_if(is.null(opt_rb_ext_rebal))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_rebal,
                     match.col = "ES",
                     risk.type = "absolute")
  )
})

test_that("chart.RiskBudget rebalancing pct_contrib (random solver) does not error", {
  skip_if(is.null(opt_rb_ext_rebal))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_rebal,
                     match.col = "ES",
                     risk.type = "pct_contrib")
  )
})

test_that("chart.RiskBudget rebalancing with main='' (topmargin=1) does not error", {
  skip_if(is.null(opt_rb_ext_rebal))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_rebal,
                     match.col = "ES",
                     risk.type = "absolute",
                     main      = "")
  )
})

# =============================================================================
# chart.RiskBudget.opt.list — legend.loc branch for pct_contrib (line plot)
# Line ~357 in charts.risk.R: the !is.null(legend.loc) branch in the
# pct_contrib section of chart.RiskBudget.opt.list was previously untested.
# =============================================================================

test_that("chart.RiskBudget.opt.list pct_contrib line with legend.loc does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_list,
                     match.col  = "ES",
                     risk.type  = "pct_contrib",
                     plot.type  = "line",
                     legend.loc = "topright")
  )
})

test_that("chart.RiskBudget.opt.list absolute with main='' (topmargin=1) does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_list,
                     match.col = "ES",
                     risk.type = "absolute",
                     plot.type = "line",
                     main      = "")
  )
})

test_that("chart.RiskBudget.opt.list pct_contrib with las=1 (bottommargin else) does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_list,
                     match.col = "ES",
                     risk.type = "pct_contrib",
                     plot.type = "line",
                     las       = 1L)
  )
})

# =============================================================================
# barplotRiskBudget — legend.loc branches (absolute and pct_contrib)
# Lines ~449 and ~460 in charts.risk.R: !is.null(legend.loc) branch in each
# risk.type section of barplotRiskBudget was previously untested.
# =============================================================================

test_that("barplotRiskBudget (via chart.RiskBudget) absolute with legend.loc does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_list,
                     match.col  = "ES",
                     risk.type  = "absolute",
                     plot.type  = "barplot",
                     legend.loc = "topright")
  )
})

test_that("barplotRiskBudget (via chart.RiskBudget) pct_contrib with legend.loc does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_list,
                     match.col  = "ES",
                     risk.type  = "pct_contrib",
                     plot.type  = "barplot",
                     legend.loc = "topright")
  )
})

test_that("barplotRiskBudget (via chart.RiskBudget) absolute with main='' does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_list,
                     match.col = "ES",
                     risk.type = "absolute",
                     plot.type = "barplot",
                     main      = "")
  )
})

test_that("barplotRiskBudget (via chart.RiskBudget) pct_contrib with las=1 does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    chart.RiskBudget(opt_rb_ext_list,
                     match.col = "ES",
                     risk.type = "pct_contrib",
                     plot.type = "barplot",
                     las       = 1L)
  )
})

# --- Direct calls to barplotRiskBudget (bypass chart.RiskBudget dispatch) ---

# barplotRiskBudget is not exported; access via ::: for direct-call coverage.

test_that("barplotRiskBudget direct call absolute does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    PortfolioAnalytics:::barplotRiskBudget(opt_rb_ext_list,
                                           match.col = "ES",
                                           risk.type = "absolute")
  )
})

test_that("barplotRiskBudget direct call pct_contrib does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    PortfolioAnalytics:::barplotRiskBudget(opt_rb_ext_list,
                                           match.col = "ES",
                                           risk.type = "pct_contrib")
  )
})

test_that("barplotRiskBudget direct call absolute with legend.loc does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    PortfolioAnalytics:::barplotRiskBudget(opt_rb_ext_list,
                                           match.col  = "ES",
                                           risk.type  = "absolute",
                                           legend.loc = "topright")
  )
})

test_that("barplotRiskBudget direct call pct_contrib with legend.loc does not error", {
  skip_if(is.null(opt_rb_ext_list))
  png(tempfile(fileext = ".png")); on.exit(dev.off(), add = TRUE)
  expect_no_error(
    PortfolioAnalytics:::barplotRiskBudget(opt_rb_ext_list,
                                           match.col  = "ES",
                                           risk.type  = "pct_contrib",
                                           legend.loc = "topright")
  )
})
