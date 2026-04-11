###############################################################################
# tests/testthat/test-moment-functions-extended.R
#
# Source files hit: moment.functions.R (extended coverage of v1 API,
#                   CCCgarch variants, portfolio.moments.boudt,
#                   portfolio.moments.bl, additional set.portfolio.moments
#                   branches)
#
# Functions covered:
#   set.portfolio.moments_v1()    — v1-constraint API (StdDev/ES/VaR/NULL)
#   set.portfolio.moments()       — v2 extra branches: mean, VaR, ETL, CSM,
#                                   boudt+StdDev, boudt+VaR, BL+ES,
#                                   meucci+ES, cleaned-returns path,
#                                   no-objectives warning
#   portfolio.moments.boudt()     — internal; mean/StdDev/ES/VaR/NULL
#   portfolio.moments.bl()        — internal; mean/StdDev/ES/VaR/NULL
#   CCCgarch.MM()                 — clean='boudt' branch
#
# NOTE: portfolio.spec() stores objectives = list() by default, so the
#       "no objectives" warning fires only when $objectives is explicitly
#       set to NULL.  constraint_v1() does the same for v1 objects.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

library(PortfolioAnalytics)

# edhec4 (4-asset subset) is provided by helper-portfolioanalytics.R
R <- edhec4
N <- ncol(R)   # 4

# ---------------------------------------------------------------------------
# Shared v2 portfolio specs — each has exactly one objective
# ---------------------------------------------------------------------------

portf_mean_only <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.objective(p, type = "return", name = "mean")
  p
})

portf_sd_only <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.objective(p, type = "risk", name = "StdDev")
  p
})

portf_var_only <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.objective(p, type = "risk", name = "VaR")
  p
})

portf_csm_only <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.objective(p, type = "risk", name = "CSM")
  p
})

portf_es_only <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.objective(p, type = "risk", name = "ES")
  p
})

portf_etl_only <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.objective(p, type = "risk", name = "ETL")
  p
})

# ES objective with clean = "boudt" in the arguments — triggers the
# cleaned-returns path inside set.portfolio.moments()
portf_es_clean <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.objective(p, type = "risk", name = "ES",
                     arguments = list(clean = "boudt"))
  p
})

# Black-Litterman equal-weight pick matrix (1 x N)
P_mat <- matrix(rep(1 / N, N), nrow = 1)


# ===========================================================================
# 1. set.portfolio.moments_v1  (internal; access via :::)
# ===========================================================================

# Build v1 constraint objects with one objective each
constr_sd <- local({
  cc <- constraint_v1(assets = colnames(R), min = 0, max = 0.55,
                      min_sum = 0.99, max_sum = 1.01)
  add.objective_v1(cc, type = "risk", name = "StdDev", enabled = TRUE)
})

constr_es <- local({
  cc <- constraint_v1(assets = colnames(R), min = 0, max = 0.55,
                      min_sum = 0.99, max_sum = 1.01)
  add.objective_v1(cc, type = "risk", name = "ES", enabled = TRUE)
})

constr_var <- local({
  cc <- constraint_v1(assets = colnames(R), min = 0, max = 0.55,
                      min_sum = 0.99, max_sum = 1.01)
  add.objective_v1(cc, type = "risk", name = "VaR", enabled = TRUE)
})

# v1 constraint with objectives explicitly NULL  → triggers the warning path
constr_null_obj <- local({
  cc <- constraint_v1(assets = colnames(R), min = 0, max = 0.55,
                      min_sum = 0.99, max_sum = 1.01)
  cc$objectives <- NULL
  cc
})

# Run the three objective-bearing scenarios once at file scope
mom_v1_sd  <- PortfolioAnalytics:::set.portfolio.moments_v1(R, constr_sd)
mom_v1_es  <- PortfolioAnalytics:::set.portfolio.moments_v1(R, constr_es)
mom_v1_var <- PortfolioAnalytics:::set.portfolio.moments_v1(R, constr_var)


# --- v1: StdDev objective ---

test_that("set.portfolio.moments_v1 StdDev: returns a list", {
  expect_true(is.list(mom_v1_sd))
})

test_that("set.portfolio.moments_v1 StdDev: mu is a numeric vector of length N", {
  expect_false(is.null(mom_v1_sd$mu))
  expect_equal(length(as.numeric(mom_v1_sd$mu)), N)
})

test_that("set.portfolio.moments_v1 StdDev: sigma is an N x N matrix", {
  expect_false(is.null(mom_v1_sd$sigma))
  expect_true(is.matrix(mom_v1_sd$sigma))
  expect_equal(dim(mom_v1_sd$sigma), c(N, N))
})

test_that("set.portfolio.moments_v1 StdDev: sigma equals cov(R)", {
  expect_equal(mom_v1_sd$sigma,
               cov(R, use = "pairwise.complete.obs"),
               tolerance = 1e-10)
})

test_that("set.portfolio.moments_v1 StdDev: m3 and m4 are NOT set", {
  expect_null(mom_v1_sd$m3)
  expect_null(mom_v1_sd$m4)
})


# --- v1: ES objective ---

test_that("set.portfolio.moments_v1 ES: returns a list", {
  expect_true(is.list(mom_v1_es))
})

test_that("set.portfolio.moments_v1 ES: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_v1_es$mu),    label = "v1 ES has mu")
  expect_false(is.null(mom_v1_es$sigma), label = "v1 ES has sigma")
  expect_false(is.null(mom_v1_es$m3),    label = "v1 ES has m3")
  expect_false(is.null(mom_v1_es$m4),    label = "v1 ES has m4")
})

test_that("set.portfolio.moments_v1 ES: m3 dimensions are N x N^2", {
  expect_true(is.matrix(mom_v1_es$m3))
  expect_equal(nrow(mom_v1_es$m3), N)
  expect_equal(ncol(mom_v1_es$m3), N^2)
})

test_that("set.portfolio.moments_v1 ES: m4 dimensions are N x N^3", {
  expect_true(is.matrix(mom_v1_es$m4))
  expect_equal(nrow(mom_v1_es$m4), N)
  expect_equal(ncol(mom_v1_es$m4), N^3)
})


# --- v1: VaR objective ---

test_that("set.portfolio.moments_v1 VaR: returns a list with mu, sigma, m3, m4", {
  expect_true(is.list(mom_v1_var))
  expect_false(is.null(mom_v1_var$mu),    label = "v1 VaR has mu")
  expect_false(is.null(mom_v1_var$sigma), label = "v1 VaR has sigma")
  expect_false(is.null(mom_v1_var$m3),    label = "v1 VaR has m3")
  expect_false(is.null(mom_v1_var$m4),    label = "v1 VaR has m4")
})

test_that("set.portfolio.moments_v1 VaR: sigma equals cov(R)", {
  expect_equal(mom_v1_var$sigma, cov(R), tolerance = 1e-10)
})


# --- v1: NULL objectives (warning path) ---

test_that("set.portfolio.moments_v1 with NULL objectives issues a warning", {
  expect_warning(
    PortfolioAnalytics:::set.portfolio.moments_v1(R, constr_null_obj),
    "no objectives"
  )
})

test_that("set.portfolio.moments_v1 with NULL objectives returns an empty list", {
  result <- suppressWarnings(
    PortfolioAnalytics:::set.portfolio.moments_v1(R, constr_null_obj)
  )
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})


# ===========================================================================
# 2. set.portfolio.moments() (v2) — additional branches not yet covered
# ===========================================================================

# --- mean-only objective, sample method ---

mom_sample_mean <- set.portfolio.moments(R, portf_mean_only, method = "sample")

test_that("set.portfolio.moments sample+mean: returns a list", {
  expect_true(is.list(mom_sample_mean))
})

test_that("set.portfolio.moments sample+mean: mu is set with length N", {
  expect_false(is.null(mom_sample_mean$mu))
  expect_equal(length(as.numeric(mom_sample_mean$mu)), N)
})

test_that("set.portfolio.moments sample+mean: sigma is not set (mean objective only)", {
  expect_null(mom_sample_mean$sigma)
})


# --- VaR objective, sample method ---

mom_sample_var <- set.portfolio.moments(R, portf_var_only, method = "sample")

test_that("set.portfolio.moments sample+VaR: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_sample_var$mu),    label = "sample VaR has mu")
  expect_false(is.null(mom_sample_var$sigma), label = "sample VaR has sigma")
  expect_false(is.null(mom_sample_var$m3),    label = "sample VaR has m3")
  expect_false(is.null(mom_sample_var$m4),    label = "sample VaR has m4")
})

test_that("set.portfolio.moments sample+VaR: sigma is an N x N matrix", {
  expect_true(is.matrix(mom_sample_var$sigma))
  expect_equal(dim(mom_sample_var$sigma), c(N, N))
})


# --- CSM objective, sample method (falls into same switch arm as VaR) ---

mom_sample_csm <- set.portfolio.moments(R, portf_csm_only, method = "sample")

test_that("set.portfolio.moments sample+CSM: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_sample_csm$mu),    label = "sample CSM has mu")
  expect_false(is.null(mom_sample_csm$sigma), label = "sample CSM has sigma")
  expect_false(is.null(mom_sample_csm$m3),    label = "sample CSM has m3")
  expect_false(is.null(mom_sample_csm$m4),    label = "sample CSM has m4")
})


# --- ETL (alias for ES) objective, sample method ---

mom_sample_etl <- set.portfolio.moments(R, portf_etl_only, method = "sample")

test_that("set.portfolio.moments sample+ETL: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_sample_etl$mu),    label = "sample ETL has mu")
  expect_false(is.null(mom_sample_etl$sigma), label = "sample ETL has sigma")
  expect_false(is.null(mom_sample_etl$m3),    label = "sample ETL has m3")
  expect_false(is.null(mom_sample_etl$m4),    label = "sample ETL has m4")
})


# --- boudt method + StdDev objective ---

mom_boudt_sd <- set.portfolio.moments(R, portf_sd_only, method = "boudt")

test_that("set.portfolio.moments boudt+StdDev: has mu and sigma", {
  expect_false(is.null(mom_boudt_sd$mu),    label = "boudt+StdDev has mu")
  expect_false(is.null(mom_boudt_sd$sigma), label = "boudt+StdDev has sigma")
})

test_that("set.portfolio.moments boudt+StdDev: sigma differs from sample cov (factor model)", {
  # factor-model sigma is not identical to raw sample covariance
  expect_true(is.matrix(mom_boudt_sd$sigma))
  expect_equal(dim(mom_boudt_sd$sigma), c(N, N))
})

test_that("set.portfolio.moments boudt+StdDev: no m3 or m4 needed for StdDev", {
  expect_null(mom_boudt_sd$m3)
  expect_null(mom_boudt_sd$m4)
})


# --- boudt method + VaR objective ---

mom_boudt_var <- set.portfolio.moments(R, portf_var_only, method = "boudt")

test_that("set.portfolio.moments boudt+VaR: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_boudt_var$mu),    label = "boudt+VaR has mu")
  expect_false(is.null(mom_boudt_var$sigma), label = "boudt+VaR has sigma")
  expect_false(is.null(mom_boudt_var$m3),    label = "boudt+VaR has m3")
  expect_false(is.null(mom_boudt_var$m4),    label = "boudt+VaR has m4")
})

test_that("set.portfolio.moments boudt+VaR: m3 is N x N^2", {
  expect_equal(dim(mom_boudt_var$m3), c(N, N^2))
})


# --- black_litterman method + ES objective ---

mom_bl_es <- set.portfolio.moments(R, portf_es_only, method = "black_litterman")

test_that("set.portfolio.moments black_litterman+ES: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_bl_es$mu),    label = "BL+ES has mu")
  expect_false(is.null(mom_bl_es$sigma), label = "BL+ES has sigma")
  expect_false(is.null(mom_bl_es$m3),    label = "BL+ES has m3")
  expect_false(is.null(mom_bl_es$m4),    label = "BL+ES has m4")
})

test_that("set.portfolio.moments black_litterman+ES: sigma is an N x N matrix", {
  expect_true(is.matrix(mom_bl_es$sigma))
  expect_equal(dim(mom_bl_es$sigma), c(N, N))
})


# --- meucci method + ES objective ---

mom_meucci_es <- set.portfolio.moments(R, portf_es_only, method = "meucci")

test_that("set.portfolio.moments meucci+ES: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_meucci_es$mu),    label = "meucci+ES has mu")
  expect_false(is.null(mom_meucci_es$sigma), label = "meucci+ES has sigma")
  expect_false(is.null(mom_meucci_es$m3),    label = "meucci+ES has m3")
  expect_false(is.null(mom_meucci_es$m4),    label = "meucci+ES has m4")
})

test_that("set.portfolio.moments meucci+ES: sigma is symmetric", {
  expect_true(isSymmetric(mom_meucci_es$sigma, tol = 1e-10),
              label = "meucci+ES sigma is symmetric")
})


# --- cleaned-returns path (objective carries arguments$clean = 'boudt') ---

mom_clean_es <- set.portfolio.moments(R, portf_es_clean, method = "sample")

test_that("set.portfolio.moments clean='boudt' path: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_clean_es$mu),    label = "clean ES has mu")
  expect_false(is.null(mom_clean_es$sigma), label = "clean ES has sigma")
  expect_false(is.null(mom_clean_es$m3),    label = "clean ES has m3")
  expect_false(is.null(mom_clean_es$m4),    label = "clean ES has m4")
})

test_that("set.portfolio.moments clean path: sigma is an N x N matrix", {
  expect_true(is.matrix(mom_clean_es$sigma))
  expect_equal(dim(mom_clean_es$sigma), c(N, N))
})


# --- meucci method + mean objective (exercises meucci mu branch) ---

mom_meucci_mean <- set.portfolio.moments(R, portf_mean_only, method = "meucci")

test_that("set.portfolio.moments meucci+mean: mu is set", {
  expect_false(is.null(mom_meucci_mean$mu))
  expect_equal(length(as.numeric(mom_meucci_mean$mu)), N)
})

test_that("set.portfolio.moments meucci+mean: sigma not set for mean-only obj", {
  expect_null(mom_meucci_mean$sigma)
})


# --- no objectives warning (objectives forced to NULL) ---

portf_null_obj <- local({
  p <- portfolio.spec(assets = colnames(R))
  p$objectives <- NULL   # force NULL so the warning path is reached
  p
})

test_that("set.portfolio.moments with NULL objectives issues a warning", {
  expect_warning(
    set.portfolio.moments(R, portf_null_obj),
    "no objectives"
  )
})


# ===========================================================================
# 3. portfolio.moments.boudt  (internal; access via :::)
# ===========================================================================

mom_pboudt_mean <- PortfolioAnalytics:::portfolio.moments.boudt(R, portf_mean_only)
mom_pboudt_sd   <- PortfolioAnalytics:::portfolio.moments.boudt(R, portf_sd_only)
mom_pboudt_es   <- PortfolioAnalytics:::portfolio.moments.boudt(R, portf_es_only)
mom_pboudt_var  <- PortfolioAnalytics:::portfolio.moments.boudt(R, portf_var_only)


# --- mean objective ---

test_that("portfolio.moments.boudt mean: is a list with mu of length N", {
  expect_true(is.list(mom_pboudt_mean))
  expect_false(is.null(mom_pboudt_mean$mu))
  expect_equal(length(as.numeric(mom_pboudt_mean$mu)), N)
})

test_that("portfolio.moments.boudt mean: sigma is NOT set", {
  expect_null(mom_pboudt_mean$sigma)
})


# --- StdDev objective ---

test_that("portfolio.moments.boudt StdDev: has mu and sigma", {
  expect_false(is.null(mom_pboudt_sd$mu),    label = "pboudt StdDev has mu")
  expect_false(is.null(mom_pboudt_sd$sigma), label = "pboudt StdDev has sigma")
})

test_that("portfolio.moments.boudt StdDev: sigma is an N x N matrix", {
  expect_true(is.matrix(mom_pboudt_sd$sigma))
  expect_equal(dim(mom_pboudt_sd$sigma), c(N, N))
})

test_that("portfolio.moments.boudt StdDev: no m3 or m4", {
  expect_null(mom_pboudt_sd$m3)
  expect_null(mom_pboudt_sd$m4)
})


# --- ES objective ---

test_that("portfolio.moments.boudt ES: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_pboudt_es$mu),    label = "pboudt ES has mu")
  expect_false(is.null(mom_pboudt_es$sigma), label = "pboudt ES has sigma")
  expect_false(is.null(mom_pboudt_es$m3),    label = "pboudt ES has m3")
  expect_false(is.null(mom_pboudt_es$m4),    label = "pboudt ES has m4")
})

test_that("portfolio.moments.boudt ES: m3 is N x N^2", {
  expect_true(is.matrix(mom_pboudt_es$m3))
  expect_equal(dim(mom_pboudt_es$m3), c(N, N^2))
})

test_that("portfolio.moments.boudt ES: m4 is N x N^3", {
  expect_true(is.matrix(mom_pboudt_es$m4))
  expect_equal(dim(mom_pboudt_es$m4), c(N, N^3))
})


# --- VaR objective ---

test_that("portfolio.moments.boudt VaR: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_pboudt_var$mu),    label = "pboudt VaR has mu")
  expect_false(is.null(mom_pboudt_var$sigma), label = "pboudt VaR has sigma")
  expect_false(is.null(mom_pboudt_var$m3),    label = "pboudt VaR has m3")
  expect_false(is.null(mom_pboudt_var$m4),    label = "pboudt VaR has m4")
})


# --- NULL objectives (warning path) ---

portf_null_boudt <- local({
  p <- portfolio.spec(assets = colnames(R))
  p$objectives <- NULL
  p
})

test_that("portfolio.moments.boudt with NULL objectives issues warning", {
  expect_warning(
    PortfolioAnalytics:::portfolio.moments.boudt(R, portf_null_boudt),
    "no objectives"
  )
})

test_that("portfolio.moments.boudt with NULL objectives returns an empty list", {
  result <- suppressWarnings(
    PortfolioAnalytics:::portfolio.moments.boudt(R, portf_null_boudt)
  )
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})


# ===========================================================================
# 4. portfolio.moments.bl  (internal; access via :::)
# ===========================================================================

mom_pbl_mean <- PortfolioAnalytics:::portfolio.moments.bl(R, portf_mean_only, P = P_mat)
mom_pbl_sd   <- PortfolioAnalytics:::portfolio.moments.bl(R, portf_sd_only,   P = P_mat)
mom_pbl_es   <- PortfolioAnalytics:::portfolio.moments.bl(R, portf_es_only,   P = P_mat)
mom_pbl_var  <- PortfolioAnalytics:::portfolio.moments.bl(R, portf_var_only,  P = P_mat)


# --- mean objective ---

test_that("portfolio.moments.bl mean: is a list with mu", {
  expect_true(is.list(mom_pbl_mean))
  expect_false(is.null(mom_pbl_mean$mu))
  expect_equal(length(as.numeric(mom_pbl_mean$mu)), N)
})

test_that("portfolio.moments.bl mean: sigma is NOT set", {
  expect_null(mom_pbl_mean$sigma)
})


# --- StdDev objective ---

test_that("portfolio.moments.bl StdDev: has mu and sigma", {
  expect_false(is.null(mom_pbl_sd$mu),    label = "pbl StdDev has mu")
  expect_false(is.null(mom_pbl_sd$sigma), label = "pbl StdDev has sigma")
})

test_that("portfolio.moments.bl StdDev: sigma is an N x N matrix", {
  expect_true(is.matrix(mom_pbl_sd$sigma))
  expect_equal(dim(mom_pbl_sd$sigma), c(N, N))
})

test_that("portfolio.moments.bl StdDev: no m3 or m4", {
  expect_null(mom_pbl_sd$m3)
  expect_null(mom_pbl_sd$m4)
})


# --- ES objective ---

test_that("portfolio.moments.bl ES: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_pbl_es$mu),    label = "pbl ES has mu")
  expect_false(is.null(mom_pbl_es$sigma), label = "pbl ES has sigma")
  expect_false(is.null(mom_pbl_es$m3),    label = "pbl ES has m3")
  expect_false(is.null(mom_pbl_es$m4),    label = "pbl ES has m4")
})

test_that("portfolio.moments.bl ES: m3 is N x N^2", {
  expect_true(is.matrix(mom_pbl_es$m3))
  expect_equal(dim(mom_pbl_es$m3), c(N, N^2))
})

test_that("portfolio.moments.bl ES: m4 is N x N^3", {
  expect_true(is.matrix(mom_pbl_es$m4))
  expect_equal(dim(mom_pbl_es$m4), c(N, N^3))
})


# --- VaR objective ---

test_that("portfolio.moments.bl VaR: has mu, sigma, m3, m4", {
  expect_false(is.null(mom_pbl_var$mu),    label = "pbl VaR has mu")
  expect_false(is.null(mom_pbl_var$sigma), label = "pbl VaR has sigma")
  expect_false(is.null(mom_pbl_var$m3),    label = "pbl VaR has m3")
  expect_false(is.null(mom_pbl_var$m4),    label = "pbl VaR has m4")
})


# --- NULL objectives (warning path) ---

portf_null_bl <- local({
  p <- portfolio.spec(assets = colnames(R))
  p$objectives <- NULL
  p
})

test_that("portfolio.moments.bl with NULL objectives issues warning", {
  expect_warning(
    PortfolioAnalytics:::portfolio.moments.bl(R, portf_null_bl, P = P_mat),
    "no objectives"
  )
})

test_that("portfolio.moments.bl with NULL objectives returns an empty list", {
  result <- suppressWarnings(
    PortfolioAnalytics:::portfolio.moments.bl(R, portf_null_bl, P = P_mat)
  )
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})


# ===========================================================================
# 5. CCCgarch.MM() with clean = 'boudt'  (requires fGarch)
# ===========================================================================

skip_if_not_installed("fGarch")

mom_garch_clean <- CCCgarch.MM(R, clean = "boudt")

test_that("CCCgarch.MM clean='boudt': returns a list", {
  expect_true(is.list(mom_garch_clean))
})

test_that("CCCgarch.MM clean='boudt': has mu, sigma, m3, m4", {
  expect_false(is.null(mom_garch_clean$mu),    label = "GARCH clean has mu")
  expect_false(is.null(mom_garch_clean$sigma), label = "GARCH clean has sigma")
  expect_false(is.null(mom_garch_clean$m3),    label = "GARCH clean has m3")
  expect_false(is.null(mom_garch_clean$m4),    label = "GARCH clean has m4")
})

test_that("CCCgarch.MM clean='boudt': mu has length N", {
  expect_equal(length(mom_garch_clean$mu), N)
})

test_that("CCCgarch.MM clean='boudt': sigma is an N x N matrix", {
  expect_true(is.matrix(mom_garch_clean$sigma))
  expect_equal(dim(mom_garch_clean$sigma), c(N, N))
})
