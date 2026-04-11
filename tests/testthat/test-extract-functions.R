###############################################################################
# tests/testthat/test-extract-functions.R
#
# Tests for extractWeights(), extractObjectiveMeasures(), extractStats(),
# extractGroups(), equal.weight(), and inverse.volatility.weight().
#
# Source files covered (primary):
#   R/extractstats.R              — all dispatch methods for extractStats,
#                                   extractWeights, extractObjectiveMeasures,
#                                   extractGroups, name.replace
#   R/equal.weight.R              — equal.weight()
#   R/inverse.volatility.weight.R — inverse.volatility.weight()
#   R/utility.combine.R           — combine.optimizations()
#   R/generics.R                  — print.opt.list()
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
skip_if_not_installed("DEoptim")

library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(DEoptim)

# ---------------------------------------------------------------------------
# Shared setup — built once at file scope for speed
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:5]
nms <- colnames(R)
N <- ncol(R)

# --- Base long-only portfolio: minimize StdDev ---
portf.base <- portfolio.spec(assets = nms)
portf.base <- add.constraint(portf.base, type = "full_investment")
portf.base <- add.constraint(portf.base, type = "long_only")
portf.base <- add.objective(portf.base, type = "risk", name = "StdDev")

opt.roi <- optimize.portfolio(R, portf.base, optimize_method = "ROI", trace = TRUE)

# DEoptim uses relaxed weight-sum bounds
portf.de <- portf.base
portf.de$constraints[[1]]$min_sum <- 0.99
portf.de$constraints[[1]]$max_sum <- 1.01

opt.de <- optimize.portfolio(R, portf.de,
  optimize_method = "DEoptim",
  search_size = 500, trace = TRUE
)
opt.de.notrace <- optimize.portfolio(R, portf.de,
  optimize_method = "DEoptim",
  search_size = 200, trace = FALSE
)

# Random portfolio result
opt.rp <- optimize.portfolio(R, portf.de,
  optimize_method = "random",
  search_size = 500, trace = TRUE
)

# --- Portfolio with both StdDev and mean objectives (for opt.list) ---
portf.both <- portfolio.spec(assets = nms)
portf.both <- add.constraint(portf.both, type = "full_investment")
portf.both <- add.constraint(portf.both, type = "box", min = 0.05, max = 0.40)
portf.both <- add.objective(portf.both, type = "risk", name = "StdDev")
portf.both <- add.objective(portf.both, type = "return", name = "mean")

portf.both2 <- portfolio.spec(assets = nms)
portf.both2 <- add.constraint(portf.both2, type = "full_investment")
portf.both2 <- add.constraint(portf.both2, type = "box", min = 0.10, max = 0.50)
portf.both2 <- add.objective(portf.both2, type = "risk", name = "StdDev")
portf.both2 <- add.objective(portf.both2, type = "return", name = "mean")

opt.roi.a <- optimize.portfolio(R, portf.both, optimize_method = "ROI", trace = TRUE)
opt.roi.b <- optimize.portfolio(R, portf.both2, optimize_method = "ROI", trace = TRUE)

# opt.list with identical objective sets
opt.list.same <- combine.optimizations(list(opt.roi.a, opt.roi.b))

# --- Portfolio with only mean objective (different from portf.base) ---
portf.ret <- portfolio.spec(assets = nms)
portf.ret <- add.constraint(portf.ret, type = "full_investment")
portf.ret <- add.constraint(portf.ret, type = "long_only")
portf.ret <- add.objective(portf.ret, type = "return", name = "mean")
opt.roi.ret <- optimize.portfolio(R, portf.ret, optimize_method = "ROI", trace = TRUE)

# opt.list with DIFFERENT objective sets (StdDev vs mean)
opt.list.diff <- combine.optimizations(list(opt.roi, opt.roi.ret))

# --- Group-constrained portfolio ---
portf.grp <- portfolio.spec(assets = nms)
portf.grp <- add.constraint(portf.grp, type = "full_investment")
portf.grp <- add.constraint(portf.grp, type = "long_only")
portf.grp <- add.constraint(portf.grp,
  type = "group",
  groups = list(c(1, 2), c(3, 4, 5)),
  group_min = c(0.1, 0.2),
  group_max = c(0.6, 0.8)
)
portf.grp <- add.objective(portf.grp, type = "risk", name = "StdDev")
opt.grp <- optimize.portfolio(R, portf.grp, optimize_method = "ROI", trace = TRUE)

# --- Portfolio with category_labels ---
portf.cat <- portfolio.spec(
  assets          = nms,
  category_labels = c("equity", "equity", "fixed", "fixed", "equity")
)
portf.cat <- add.constraint(portf.cat, type = "full_investment")
portf.cat <- add.constraint(portf.cat, type = "long_only")
portf.cat <- add.objective(portf.cat, type = "risk", name = "StdDev")
opt.cat <- optimize.portfolio(R, portf.cat, optimize_method = "ROI", trace = TRUE)

# --- equal.weight and inverse.volatility.weight ---
portf.eqwt <- portfolio.spec(assets = nms)
portf.eqwt <- add.constraint(portf.eqwt, type = "full_investment")
portf.eqwt <- add.constraint(portf.eqwt, type = "long_only")
portf.eqwt <- add.objective(portf.eqwt, type = "risk", name = "StdDev")
portf.eqwt <- add.objective(portf.eqwt, type = "return", name = "mean")

eqwt.result <- equal.weight(R, portf.eqwt)
invol.result <- inverse.volatility.weight(R, portf.eqwt)


# ===========================================================================
# extractWeights()
# ===========================================================================

test_that("extractWeights on ROI result returns named numeric vector", {
  w <- extractWeights(opt.roi)
  expect_true(is.numeric(w))
  expect_equal(length(w), N)
  expect_equal(names(w), nms)
})

test_that("extractWeights on ROI result sums to 1", {
  expect_equal(sum(extractWeights(opt.roi)), 1, tolerance = 1e-6)
})

test_that("extractWeights on random result returns valid numeric weights", {
  w <- extractWeights(opt.rp)
  expect_true(is.numeric(w))
  expect_equal(length(w), N)
  # Random portfolios use relaxed weight-sum bounds [0.99, 1.01]
  expect_equal(sum(w), 1, tolerance = 0.02)
})

test_that("extractWeights on DEoptim result returns valid numeric weights", {
  w <- extractWeights(opt.de)
  expect_true(is.numeric(w))
  expect_equal(length(w), N)
  # DEoptim uses relaxed weight-sum bounds [0.99, 1.01]
  expect_equal(sum(w), 1, tolerance = 0.02)
})

test_that("extractWeights errors on non-optimize.portfolio object", {
  expect_error(extractWeights(list(a = 1)))
})

test_that("extractWeights.opt.list returns a weight matrix with one row per optimization", {
  wmat <- extractWeights(opt.list.same)
  expect_true(is.matrix(wmat))
  expect_equal(nrow(wmat), 2L)
  expect_equal(ncol(wmat), N)
})

test_that("extractWeights.opt.list rows each sum to approximately 1", {
  wmat <- extractWeights(opt.list.same)
  expect_equal(as.numeric(rowSums(wmat)), c(1, 1), tolerance = 1e-6)
})

test_that("extractWeights.opt.list works when opt.list has no names", {
  ol <- opt.list.same
  names(ol) <- NULL
  wmat <- extractWeights(ol)
  expect_true(is.matrix(wmat))
  expect_equal(nrow(wmat), 2L)
})

test_that("extractWeights on equal.weight result returns exactly 1/N weights", {
  w <- extractWeights(eqwt.result)
  expect_equal(as.numeric(w), rep(1 / N, N), tolerance = 1e-10)
})

test_that("extractWeights on inverse.volatility.weight result returns positive weights summing to 1", {
  w <- extractWeights(invol.result)
  expect_true(all(w > 0))
  expect_equal(sum(w), 1, tolerance = 1e-6)
})


# ===========================================================================
# extractObjectiveMeasures()
# ===========================================================================

test_that("extractObjectiveMeasures on ROI result returns a list", {
  obj <- extractObjectiveMeasures(opt.roi)
  expect_true(is.list(obj))
})

test_that("extractObjectiveMeasures on ROI result has numeric StdDev element", {
  obj <- extractObjectiveMeasures(opt.roi)
  expect_true(!is.null(obj$StdDev))
  expect_true(is.numeric(obj$StdDev))
  expect_true(obj$StdDev > 0)
})

test_that("extractObjectiveMeasures on random result returns list with StdDev", {
  obj <- extractObjectiveMeasures(opt.rp)
  expect_true(is.list(obj))
  expect_true(is.numeric(obj$StdDev))
})

test_that("extractObjectiveMeasures on DEoptim result returns list with StdDev", {
  obj <- extractObjectiveMeasures(opt.de)
  expect_true(is.list(obj))
  expect_true(is.numeric(obj$StdDev))
})

test_that("extractObjectiveMeasures errors on wrong class", {
  expect_error(extractObjectiveMeasures(list(a = 1)))
})

test_that("extractObjectiveMeasures.opt.list with identical objectives returns a matrix", {
  obj_mat <- extractObjectiveMeasures(opt.list.same)
  expect_true(is.matrix(obj_mat))
  expect_equal(nrow(obj_mat), 2L)
})

test_that("extractObjectiveMeasures.opt.list has StdDev and mean columns", {
  obj_mat <- extractObjectiveMeasures(opt.list.same)
  expect_true("StdDev" %in% colnames(obj_mat))
  expect_true("mean" %in% colnames(obj_mat))
})

test_that("extractObjectiveMeasures.opt.list with different objectives returns matrix-like result", {
  obj <- extractObjectiveMeasures(opt.list.diff)
  expect_true(is.matrix(obj) || is.list(obj) || is.data.frame(obj))
})

test_that("extractObjectiveMeasures.opt.list errors on wrong class", {
  expect_error(extractObjectiveMeasures.opt.list(list(a = 1)))
})

test_that("extractObjectiveMeasures.opt.list works when opt.list has no names", {
  ol <- opt.list.same
  names(ol) <- NULL
  obj_mat <- extractObjectiveMeasures(ol)
  expect_true(is.matrix(obj_mat))
})

test_that("extractObjectiveMeasures on equal.weight result returns list with StdDev and mean", {
  obj <- extractObjectiveMeasures(eqwt.result)
  expect_true(is.list(obj))
  expect_true(is.numeric(obj$StdDev))
  expect_true(is.numeric(obj$mean))
})

test_that("extractObjectiveMeasures on inverse.volatility.weight result returns a list", {
  obj <- extractObjectiveMeasures(invol.result)
  expect_true(is.list(obj))
  expect_true(is.numeric(obj$StdDev))
})


# ===========================================================================
# extractStats()
# ===========================================================================

test_that("extractStats on ROI result returns a named numeric vector", {
  stats <- extractStats(opt.roi)
  expect_true(is.numeric(stats))
  expect_false(is.null(names(stats)))
})

test_that("extractStats on ROI result contains weight columns prefixed 'w.'", {
  stats <- extractStats(opt.roi)
  w_cols <- grep("^w\\.", names(stats), value = TRUE)
  expect_equal(length(w_cols), N)
})

test_that("extractStats on ROI result contains 'out' element", {
  stats <- extractStats(opt.roi)
  expect_true("out" %in% names(stats))
})

test_that("extractStats on ROI result contains objective measure column", {
  stats <- extractStats(opt.roi)
  expect_true("StdDev" %in% names(stats))
})

test_that("extractStats on random result returns a matrix", {
  stats <- extractStats(opt.rp)
  expect_true(is.matrix(stats))
})

test_that("extractStats on random result has 'out' and weight columns", {
  stats <- extractStats(opt.rp)
  w_cols <- grep("^w\\.", colnames(stats), value = TRUE)
  expect_equal(length(w_cols), N)
  expect_true("out" %in% colnames(stats))
})

test_that("extractStats on DEoptim result returns a matrix", {
  stats <- extractStats(opt.de)
  expect_true(is.matrix(stats))
})

test_that("extractStats on DEoptim result has 'out' and weight columns", {
  stats <- extractStats(opt.de)
  w_cols <- grep("^w\\.", colnames(stats), value = TRUE)
  expect_equal(length(w_cols), N)
  expect_true("out" %in% colnames(stats))
})

test_that("extractStats on DEoptim result errors when trace=FALSE", {
  expect_error(extractStats(opt.de.notrace))
})

test_that("extractStats on equal.weight result returns named numeric vector with asset weight columns", {
  stats <- extractStats(eqwt.result)
  expect_true(is.numeric(stats))
  # eqwt extractStats uses bare asset names (no 'w.' prefix), unlike ROI/random/DE
  expect_true(all(nms %in% names(stats)))
  expect_true("out" %in% names(stats))
})

test_that("extractStats on inverse.volatility.weight result returns named numeric vector with asset weight columns", {
  stats <- extractStats(invol.result)
  expect_true(is.numeric(stats))
  # invol extractStats uses bare asset names (no 'w.' prefix)
  expect_true(all(nms %in% names(stats)))
  expect_true("out" %in% names(stats))
})

test_that("extractStats.opt.list returns a list", {
  stats <- extractStats(opt.list.same)
  expect_true(is.list(stats))
  expect_equal(length(stats), 2L)
})


# ===========================================================================
# extractGroups()
# ===========================================================================

test_that("extractGroups errors on non-optimize.portfolio object", {
  expect_error(extractGroups(list(a = 1)))
})

test_that("extractGroups with group constraint returns a list with group_weights", {
  grps <- extractGroups(opt.grp)
  expect_true(is.list(grps))
  expect_false(is.null(grps$group_weights))
  expect_equal(length(grps$group_weights), 2L) # two groups
})

test_that("extractGroups group_weights sum equals sum of all asset weights", {
  grps <- extractGroups(opt.grp)
  expect_equal(sum(grps$group_weights), sum(grps$weights), tolerance = 1e-8)
})

test_that("extractGroups group_weights match manually computed group sums", {
  grps <- extractGroups(opt.grp)
  w <- grps$weights
  g1_sum <- sum(w[c(1, 2)])
  g2_sum <- sum(w[c(3, 4, 5)])
  expect_equal(as.numeric(grps$group_weights[1]), g1_sum, tolerance = 1e-8)
  expect_equal(as.numeric(grps$group_weights[2]), g2_sum, tolerance = 1e-8)
})

test_that("extractGroups with category_labels returns category_weights", {
  grps <- extractGroups(opt.cat)
  expect_false(is.null(grps$category_weights))
  # two categories: equity (assets 1,2,5) and fixed (assets 3,4)
  expect_equal(length(grps$category_weights), 2L)
})

test_that("extractGroups category_weights sum to approximately 1", {
  grps <- extractGroups(opt.cat)
  expect_equal(sum(grps$category_weights), 1, tolerance = 1e-6)
})

test_that("extractGroups on portfolio without group constraint returns NULL group_weights", {
  grps <- extractGroups(opt.roi)
  expect_null(grps$group_weights)
})

test_that("extractGroups on portfolio without category_labels returns NULL category_weights", {
  grps <- extractGroups(opt.roi)
  expect_null(grps$category_weights)
})

test_that("extractGroups always returns the optimal weights", {
  grps <- extractGroups(opt.grp)
  expect_equal(grps$weights, extractWeights(opt.grp))
})


# ===========================================================================
# equal.weight()
# ===========================================================================

test_that("equal.weight() returns an optimize.portfolio.eqwt object", {
  expect_s3_class(eqwt.result, "optimize.portfolio.eqwt")
  expect_s3_class(eqwt.result, "optimize.portfolio")
})

test_that("equal.weight() weights are exactly 1/N for long-only full-investment", {
  w <- extractWeights(eqwt.result)
  expect_equal(as.numeric(w), rep(1 / N, N), tolerance = 1e-10)
})

test_that("equal.weight() weight names match asset names", {
  w <- extractWeights(eqwt.result)
  expect_equal(names(w), nms)
})

test_that("equal.weight() has numeric StdDev and mean in objective measures", {
  obj <- extractObjectiveMeasures(eqwt.result)
  expect_true(is.numeric(obj$StdDev))
  expect_true(is.numeric(obj$mean))
})

test_that("equal.weight() errors when portfolio argument is not a portfolio object", {
  expect_error(equal.weight(R, list(a = 1)))
})

test_that("equal.weight() warns and subsets when ncol(R) > nassets", {
  p <- portfolio.spec(assets = nms[1:3])
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_warning(equal.weight(R, p), "subsetting")
})

test_that("equal.weight() errors when ncol(R) < nassets", {
  p <- portfolio.spec(assets = paste0("X", 1:10))
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_error(equal.weight(R, p))
})


# ===========================================================================
# inverse.volatility.weight()
# ===========================================================================

test_that("inverse.volatility.weight() returns an optimize.portfolio.invol object", {
  expect_s3_class(invol.result, "optimize.portfolio.invol")
  expect_s3_class(invol.result, "optimize.portfolio")
})

test_that("inverse.volatility.weight() weights are positive and sum to 1", {
  w <- extractWeights(invol.result)
  expect_true(all(w > 0))
  expect_equal(sum(w), 1, tolerance = 1e-6)
})

test_that("inverse.volatility.weight() weights are proportional to 1/StdDev", {
  inv_vol <- 1 / as.numeric(PerformanceAnalytics::StdDev(R))
  expected_w <- inv_vol / sum(inv_vol)
  actual_w <- as.numeric(extractWeights(invol.result))
  expect_equal(actual_w, expected_w, tolerance = 1e-6)
})

test_that("inverse.volatility.weight() weight names match asset names", {
  w <- extractWeights(invol.result)
  expect_equal(names(w), nms)
})

test_that("inverse.volatility.weight() errors when portfolio is not a portfolio object", {
  expect_error(inverse.volatility.weight(R, list(a = 1)))
})

test_that("inverse.volatility.weight() warns and subsets when ncol(R) > nassets", {
  p <- portfolio.spec(assets = nms[1:3])
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_warning(inverse.volatility.weight(R, p), "subsetting")
})


# ===========================================================================
# combine.optimizations() and print.opt.list()
# ===========================================================================

test_that("combine.optimizations() returns an opt.list object", {
  ol <- combine.optimizations(list(opt.roi, opt.roi.ret))
  expect_s3_class(ol, "opt.list")
  expect_equal(length(ol), 2L)
})

test_that("combine.optimizations() errors when a list element is not optimize.portfolio", {
  expect_error(combine.optimizations(list(opt.roi, list(a = 1))))
})

test_that("combine.optimizations() errors when x is not a list", {
  expect_error(combine.optimizations(42))
})

test_that("print.opt.list() produces output without error", {
  ol <- combine.optimizations(list(opt.roi, opt.roi.ret))
  expect_output(print(ol))
})
  p_bnds <- portfolio.spec(assets=4)
  p_bnds <- add.constraint(p_bnds, type='weight_sum', min_sum=0.99, max_sum=1.01)
  p_bnds <- add.constraint(p_bnds, type='box', min=0, max=1)
  p_bnds <- add.objective(p_bnds, type='risk', name='StdDev')

test_that('extractStats handles GenSA without trace', {
  p <- portfolio.spec(assets=4)
  p <- add.constraint(p, type='weight_sum', min_sum=0.99, max_sum=1.01)
  p <- add.objective(p, type='risk', name='StdDev')
  opt <- optimize.portfolio(edhec4, p_bnds, optimize_method='GenSA', trace=FALSE, maxit=5)
  # res <- extractStats(opt)
  expect_error(extractStats(opt))
})

test_that('extractStats handles pso without trace', {
  p <- portfolio.spec(assets=4)
  p <- add.constraint(p, type='weight_sum', min_sum=0.99, max_sum=1.01)
  p <- add.objective(p, type='risk', name='StdDev')
  opt <- optimize.portfolio(edhec4, p_bnds, optimize_method='pso', trace=FALSE, maxit=5)
  # res <- extractStats(opt)
  expect_error(extractStats(opt))
})
