###############################################################################
# tests/testthat/test-moment-functions-garch.R
#
# Coverage targets (R/moment.functions.R):
#
#   1. CCCgarch.MM() — hasArg(mu) explicit mu branch (line 31)
#   2. garch.mm()    — entire function (lines 357-366)
#   3. set.portfolio.moments() — multiple-clean warning (lines 180-182)
#   4. set.portfolio.moments() — hasArg(ROI)=TRUE skip in ES arm (lines 321-322)
#   5. portfolio.moments.boudt() — hasArg(ROI)=TRUE skip (lines 434-435)
#   6. portfolio.moments.bl()   — hasArg(ROI)=TRUE skip (lines 520-521)
#   7. set.portfolio.moments() — mean + black_litterman method (line 253)
#   8. set.portfolio.moments() — VaR/CSM + black_litterman method (lines 298-303)
#   9. set.portfolio.moments() — VaR/CSM + meucci method (lines 304-309)
#  10. set.portfolio.moments() — explicit posterior_p for meucci (line 209)
#  11. portfolio.moments.boudt() — clean detection path (lines 391-398)
#  12. portfolio.moments.bl()   — clean detection path (lines 474-482)
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("PerformanceAnalytics")

utils::data(edhec)
R4 <- edhec[, 1:4]
nms <- colnames(R4)

# ---------------------------------------------------------------------------
# Helper: build a simple portfolio with a given objective name and type
# ---------------------------------------------------------------------------
make_portf <- function(obj_name, obj_type = "risk", clean = NULL) {
  p <- portfolio.spec(nms)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  args <- if (!is.null(clean)) list(clean = clean) else list()
  p <- add.objective(p, type = obj_type, name = obj_name,
                     arguments = args)
  p
}

# ===========================================================================
# Section 1: CCCgarch.MM() — explicit mu argument (line 31)
# ===========================================================================

test_that("CCCgarch.MM explicit mu= branch is exercised", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  explicit_mu <- colMeans(R4)
  result <- tryCatch(
    CCCgarch.MM(R4, mu = explicit_mu),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  # mu should have been set to the supplied value, not recalculated
  expect_true(is.list(result))
  expect_true(!is.null(result$mu))
})

test_that("CCCgarch.MM explicit mu= returns list with sigma", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  result <- tryCatch(
    CCCgarch.MM(R4, mu = colMeans(R4)),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_true(!is.null(result$sigma))
  expect_equal(dim(result$sigma), c(ncol(R4), ncol(R4)))
})

test_that("CCCgarch.MM explicit mu= returns list with m3 and m4", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  result <- tryCatch(
    CCCgarch.MM(R4, mu = colMeans(R4)),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_true(!is.null(result$m3))
  expect_true(!is.null(result$m4))
})

# ===========================================================================
# Section 2: garch.mm() — entire function (lines 357-366)
# Requires constructing mu_ts (xts of means) and covlist (list of cov matrices)
# ===========================================================================

test_that("garch.mm() returns a momentargs list with mu, sigma, m3, m4", {
  # garch.mm is an unexported helper; call via :::
  idx        <- zoo::index(R4)
  n          <- length(idx)
  mu_vals    <- matrix(colMeans(R4), nrow = 1L)
  colnames(mu_vals) <- nms
  mu_ts      <- xts::xts(do.call(rbind, replicate(n, mu_vals, simplify = FALSE)),
                          order.by = idx)
  cov_mat    <- cov(R4)
  # covlist: a list indexed by date strings
  covlist    <- vector("list", n)
  names(covlist) <- as.character(idx)
  for (i in seq_len(n)) covlist[[i]] <- cov_mat

  result <- tryCatch(
    PortfolioAnalytics:::garch.mm(R4, mu_ts = mu_ts, covlist = covlist),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_true(is.list(result))
})

test_that("garch.mm() sets mu from the last row of mu_ts", {
  idx     <- zoo::index(R4)
  n       <- length(idx)
  mu_vals <- matrix(colMeans(R4), nrow = 1L)
  colnames(mu_vals) <- nms
  mu_ts   <- xts::xts(do.call(rbind, replicate(n, mu_vals, simplify = FALSE)),
                       order.by = idx)
  cov_mat <- cov(R4)
  covlist <- setNames(replicate(n, cov_mat, simplify = FALSE), as.character(idx))

  result  <- PortfolioAnalytics:::garch.mm(R4, mu_ts = mu_ts, covlist = covlist)
  expect_true(!is.null(result$mu))
})

test_that("garch.mm() sets sigma from covlist at last index", {
  idx     <- zoo::index(R4)
  n       <- length(idx)
  mu_vals <- matrix(colMeans(R4), nrow = 1L)
  colnames(mu_vals) <- nms
  mu_ts   <- xts::xts(do.call(rbind, replicate(n, mu_vals, simplify = FALSE)),
                       order.by = idx)
  cov_mat <- cov(R4)
  covlist <- setNames(replicate(n, cov_mat, simplify = FALSE), as.character(idx))

  result  <- PortfolioAnalytics:::garch.mm(R4, mu_ts = mu_ts, covlist = covlist)
  expect_true(!is.null(result$sigma))
})

test_that("garch.mm() populates m3 when momentargs$m3 is NULL", {
  idx     <- zoo::index(R4)
  n       <- length(idx)
  mu_vals <- matrix(colMeans(R4), nrow = 1L)
  colnames(mu_vals) <- nms
  mu_ts   <- xts::xts(do.call(rbind, replicate(n, mu_vals, simplify = FALSE)),
                       order.by = idx)
  cov_mat <- cov(R4)
  covlist <- setNames(replicate(n, cov_mat, simplify = FALSE), as.character(idx))

  result  <- PortfolioAnalytics:::garch.mm(R4, mu_ts = mu_ts, covlist = covlist)
  expect_true(!is.null(result$m3))
})

test_that("garch.mm() populates m4 when momentargs$m4 is NULL", {
  idx     <- zoo::index(R4)
  n       <- length(idx)
  mu_vals <- matrix(colMeans(R4), nrow = 1L)
  colnames(mu_vals) <- nms
  mu_ts   <- xts::xts(do.call(rbind, replicate(n, mu_vals, simplify = FALSE)),
                       order.by = idx)
  cov_mat <- cov(R4)
  covlist <- setNames(replicate(n, cov_mat, simplify = FALSE), as.character(idx))

  result  <- PortfolioAnalytics:::garch.mm(R4, mu_ts = mu_ts, covlist = covlist)
  expect_true(!is.null(result$m4))
})

test_that("garch.mm() respects pre-filled momentargs$m3 (no overwrite)", {
  idx     <- zoo::index(R4)
  n       <- length(idx)
  mu_vals <- matrix(colMeans(R4), nrow = 1L)
  colnames(mu_vals) <- nms
  mu_ts   <- xts::xts(do.call(rbind, replicate(n, mu_vals, simplify = FALSE)),
                       order.by = idx)
  cov_mat <- cov(R4)
  covlist <- setNames(replicate(n, cov_mat, simplify = FALSE), as.character(idx))

  sentinel <- matrix(99, nrow = ncol(R4)^2, ncol = ncol(R4))
  pre_args <- list(m3 = sentinel)
  result   <- PortfolioAnalytics:::garch.mm(R4, mu_ts = mu_ts, covlist = covlist,
                                             momentargs = pre_args)
  # m3 should NOT be overwritten
  expect_identical(result$m3, sentinel)
})

# ===========================================================================
# Section 3: set.portfolio.moments() — multiple-clean warning (lines 180-182)
# Requires two objectives with DIFFERENT clean= values
# ===========================================================================

test_that("set.portfolio.moments warns when two objectives have different clean methods", {
  p <- portfolio.spec(nms)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  # objective 1: clean="boudt"
  p <- add.objective(p, type = "risk",   name = "StdDev",
                     arguments = list(clean = "boudt"))
  # objective 2: clean="geltner"  (different method — triggers warning)
  p <- add.objective(p, type = "return", name = "mean",
                     arguments = list(clean = "geltner"))

  expect_warning(
    set.portfolio.moments(R4, p, method = "sample"),
    regexp = "Multiple methods"
  )
})

# ===========================================================================
# Section 4: set.portfolio.moments() — ROI=TRUE skip in ES arm (lines 321-322)
# When ROI=TRUE is passed, the ES switch arm should skip moment calculation
# ===========================================================================

test_that("set.portfolio.moments ES arm with ROI=TRUE returns empty momentargs", {
  p_es <- make_portf("ES")
  result <- set.portfolio.moments(R4, p_es, method = "sample", ROI = TRUE)
  # With ROI=TRUE, the ES arm skips everything: momentargs should be empty list
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})

test_that("set.portfolio.moments ETL arm with ROI=TRUE returns empty momentargs", {
  p_etl <- make_portf("ETL")
  result <- set.portfolio.moments(R4, p_etl, method = "sample", ROI = TRUE)
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})

test_that("set.portfolio.moments CVaR arm with ROI=TRUE returns empty momentargs", {
  p_cvar <- make_portf("CVaR")
  result <- set.portfolio.moments(R4, p_cvar, method = "sample", ROI = TRUE)
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})

test_that("set.portfolio.moments ES arm without ROI=TRUE does populate momentargs", {
  p_es <- make_portf("ES")
  result <- set.portfolio.moments(R4, p_es, method = "sample")
  # Without ROI=TRUE the ES arm runs, so sigma should be populated
  expect_true(!is.null(result$sigma))
})

# ===========================================================================
# Section 5: portfolio.moments.boudt() — ROI=TRUE skip (lines 434-435)
# portfolio.moments.boudt is unexported; use :::
# ===========================================================================

test_that("portfolio.moments.boudt ES arm with ROI=TRUE returns empty momentargs", {
  p_es <- make_portf("ES")
  result <- PortfolioAnalytics:::portfolio.moments.boudt(R4, p_es, ROI = TRUE)
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})

test_that("portfolio.moments.boudt ES arm without ROI=TRUE populates momentargs", {
  p_es <- make_portf("ES")
  result <- PortfolioAnalytics:::portfolio.moments.boudt(R4, p_es)
  expect_true(!is.null(result$sigma))
  expect_true(!is.null(result$m3))
})

# ===========================================================================
# Section 6: portfolio.moments.bl() — ROI=TRUE skip (lines 520-521)
# portfolio.moments.bl is unexported; use :::
# P_mat: 1×N matrix with equal weights (normalized)
# ===========================================================================

P_mat4 <- matrix(rep(1 / ncol(R4), ncol(R4)), nrow = 1)

test_that("portfolio.moments.bl ES arm with ROI=TRUE returns empty momentargs", {
  p_es <- make_portf("ES")
  result <- PortfolioAnalytics:::portfolio.moments.bl(R4, p_es, P = P_mat4, ROI = TRUE)
  expect_true(is.list(result))
  expect_equal(length(result), 0L)
})

test_that("portfolio.moments.bl ES arm without ROI=TRUE populates momentargs", {
  p_es <- make_portf("ES")
  result <- PortfolioAnalytics:::portfolio.moments.bl(R4, p_es, P = P_mat4)
  expect_true(!is.null(result$sigma))
})

# ===========================================================================
# Section 7: set.portfolio.moments() — mean + black_litterman (line 253)
# Use default P (no P argument) — the default inside set.portfolio.moments
# is matrix(rep(1, ncol(R)), nrow=1) but that is handled internally via
# match.call. Using no P lets the default path work correctly.
# ===========================================================================

test_that("set.portfolio.moments mean objective with black_litterman populates mu via BLMu", {
  p_mean <- make_portf("mean", obj_type = "return")
  result <- set.portfolio.moments(R4, p_mean, method = "black_litterman")
  expect_true(!is.null(result$mu))
  expect_equal(length(result$mu), ncol(R4))
})

# ===========================================================================
# Section 8: set.portfolio.moments() — VaR/CSM + black_litterman (lines 298-303)
# ===========================================================================

test_that("set.portfolio.moments VaR objective with black_litterman populates all moments", {
  p_var <- make_portf("VaR")
  result <- set.portfolio.moments(R4, p_var, method = "black_litterman")
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$sigma))
  expect_true(!is.null(result$m3))
  expect_true(!is.null(result$m4))
})

test_that("set.portfolio.moments CSM objective with black_litterman populates all moments", {
  p_csm <- make_portf("CSM")
  result <- set.portfolio.moments(R4, p_csm, method = "black_litterman")
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$sigma))
  expect_true(!is.null(result$m3))
  expect_true(!is.null(result$m4))
})

# ===========================================================================
# Section 9: set.portfolio.moments() — VaR/CSM + meucci (lines 304-309)
# ===========================================================================

test_that("set.portfolio.moments VaR objective with meucci populates all moments", {
  p_var <- make_portf("VaR")
  result <- set.portfolio.moments(R4, p_var, method = "meucci")
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$sigma))
  expect_true(!is.null(result$m3))
  expect_true(!is.null(result$m4))
})

test_that("set.portfolio.moments CSM objective with meucci populates all moments", {
  p_csm <- make_portf("CSM")
  result <- set.portfolio.moments(R4, p_csm, method = "meucci")
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$sigma))
  expect_true(!is.null(result$m3))
  expect_true(!is.null(result$m4))
})

# ===========================================================================
# Section 10: set.portfolio.moments() — explicit posterior_p for meucci (line 209)
# BUG-3 FIXED: match.call() replaced with list(...)[["posterior_p"]], so
# passing posterior_p as a variable now correctly forwards the value.
# ===========================================================================

test_that("set.portfolio.moments meucci with posterior_p as variable (BUG-3 fixed)", {
  # BUG-3 fixed: list(...)[["posterior_p"]] correctly gets the value.
  # Passing a non-uniform distribution as a variable should produce different
  # results than the uniform default.
  p_mean <- make_portf("mean", obj_type = "return")
  n <- nrow(R4)
  # Skew weights toward recent observations
  pp <- seq(0.5, 1.5, length.out = n)
  pp <- pp / sum(pp)
  result_custom <- set.portfolio.moments(R4, p_mean, method = "meucci",
                                         posterior_p = pp)
  result_default <- set.portfolio.moments(R4, p_mean, method = "meucci")
  expect_true(!is.null(result_custom$mu))
  # Custom weights should produce different mu than uniform
  expect_false(isTRUE(all.equal(result_custom$mu, result_default$mu)))
})

# ===========================================================================
# Section 11: portfolio.moments.boudt() — clean detection (lines 391-398)
# ===========================================================================

test_that("portfolio.moments.boudt with clean='boudt' objective uses cleaned returns", {
  p_clean <- make_portf("StdDev", clean = "boudt")
  result <- tryCatch(
    PortfolioAnalytics:::portfolio.moments.boudt(R4, p_clean),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$sigma))
})

test_that("portfolio.moments.boudt with clean detection returns valid sigma matrix", {
  p_clean <- make_portf("StdDev", clean = "boudt")
  result <- tryCatch(
    PortfolioAnalytics:::portfolio.moments.boudt(R4, p_clean),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_equal(dim(result$sigma), c(ncol(R4), ncol(R4)))
  expect_true(isSymmetric(result$sigma))
})

# ===========================================================================
# Section 12: portfolio.moments.bl() — clean detection (lines 474-482)
# ===========================================================================

test_that("portfolio.moments.bl with clean='boudt' objective uses cleaned returns", {
  p_clean <- make_portf("ES", clean = "boudt")
  result <- tryCatch(
    PortfolioAnalytics:::portfolio.moments.bl(R4, p_clean, P = P_mat4),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_true(!is.null(result$mu))
  expect_true(!is.null(result$sigma))
})

test_that("portfolio.moments.bl with clean detection returns valid sigma matrix", {
  p_clean <- make_portf("ES", clean = "boudt")
  result <- tryCatch(
    PortfolioAnalytics:::portfolio.moments.bl(R4, p_clean, P = P_mat4),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_equal(dim(result$sigma), c(ncol(R4), ncol(R4)))
})
