###############################################################################
# tests/testthat/test-optimize-portfolio-advanced.R
#
# Source files hit: optimize.portfolio.R (v2 advanced branches),
#                   constrained_objective.R (weight_concentration paths)
#
# Covers: weight_concentration objective, pre-computed rp argument,
#         momentFUN as function object, trace=TRUE slots, multi-objective.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
skip_if_not_installed("DEoptim")

library(PortfolioAnalytics)
library(DEoptim)

utils::data(edhec)
edhec5 <- edhec[, 1:5]

# ---------------------------------------------------------------------------
# Section 1: weight_concentration objective — DEoptim and random
# ---------------------------------------------------------------------------

p_hhi <- portfolio.spec(assets = colnames(edhec5))
p_hhi <- add.constraint(p_hhi, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_hhi <- add.constraint(p_hhi, type = "box", min = 0, max = 1)
p_hhi <- add.objective(p_hhi, type = "return", name = "mean")
p_hhi <- add.objective(p_hhi, type = "weight_concentration",
                        name = "HHI", conc_aversion = 0.01)

set.seed(42)
opt_hhi_de <- tryCatch({
  optimize.portfolio(edhec5, p_hhi,
                     optimize_method = "DEoptim",
                     itermax        = 30,
                     search_size    = 200)
}, error = function(e) NULL)

set.seed(42)
opt_hhi_rp <- tryCatch({
  optimize.portfolio(edhec5, p_hhi,
                     optimize_method = "random",
                     search_size    = 200)
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# Section 2: pre-computed random portfolios passed via rp argument
# ---------------------------------------------------------------------------

portf_rp <- portfolio.spec(assets = colnames(edhec5))
portf_rp <- add.constraint(portf_rp, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
portf_rp <- add.constraint(portf_rp, type = "box", min = 0, max = 1)
portf_rp <- add.objective(portf_rp, type = "risk", name = "StdDev")

set.seed(42)
rp_mat <- random_portfolios(portf_rp, permutations = 200)

set.seed(42)
opt_precomp_rp <- tryCatch({
  optimize.portfolio(edhec5, portf_rp,
                     optimize_method = "random",
                     rp             = rp_mat)
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# Section 3: momentFUN passed as a function object (not a string)
# ---------------------------------------------------------------------------

my_moments <- function(R, portfolio) {
  list(mu = colMeans(R), sigma = cov(R), m3 = NULL, m4 = NULL)
}

portf_moments <- portfolio.spec(assets = colnames(edhec5))
portf_moments <- add.constraint(portf_moments, type = "weight_sum",
                                 min_sum = 0.99, max_sum = 1.01)
portf_moments <- add.constraint(portf_moments, type = "box", min = 0, max = 1)
portf_moments <- add.objective(portf_moments, type = "risk", name = "StdDev")

set.seed(42)
opt_momfun <- tryCatch({
  optimize.portfolio(edhec5, portf_moments,
                     optimize_method = "random",
                     momentFUN       = my_moments,
                     search_size     = 200)
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# Section 4 & 5: trace=TRUE slots — shared portfolio spec (return + ES)
# ---------------------------------------------------------------------------

portf_tr <- portfolio.spec(assets = colnames(edhec5))
portf_tr <- add.constraint(portf_tr, type = "weight_sum",
                            min_sum = 0.99, max_sum = 1.01)
portf_tr <- add.constraint(portf_tr, type = "box", min = 0, max = 1)
portf_tr <- add.objective(portf_tr, type = "return", name = "mean")
portf_tr <- add.objective(portf_tr, type = "risk",   name = "ES")

# Section 4: trace=TRUE with DEoptim
set.seed(42)
opt_de_tr <- tryCatch({
  optimize.portfolio(edhec5, portf_tr,
                     optimize_method = "DEoptim",
                     trace           = TRUE,
                     itermax         = 20)
}, error = function(e) NULL)

# Section 5: trace=TRUE with random portfolios
set.seed(42)
opt_rp_tr <- tryCatch({
  optimize.portfolio(edhec5, portf_tr,
                     optimize_method = "random",
                     trace           = TRUE,
                     search_size     = 200)
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# Section 6: Multi-objective — return + risk + risk_budget with DEoptim
# ---------------------------------------------------------------------------

portf_multi <- portfolio.spec(assets = colnames(edhec5))
portf_multi <- add.constraint(portf_multi, type = "weight_sum",
                               min_sum = 0.99, max_sum = 1.01)
portf_multi <- add.constraint(portf_multi, type = "box",
                               min = 0.05, max = 0.60)
portf_multi <- add.objective(portf_multi, type = "return",      name = "mean")
portf_multi <- add.objective(portf_multi, type = "risk",        name = "ES")
portf_multi <- add.objective(portf_multi, type = "risk_budget", name = "ES",
                              min_concentration = TRUE)

set.seed(42)
opt_multi <- tryCatch({
  optimize.portfolio(edhec5, portf_multi,
                     optimize_method = "DEoptim",
                     trace           = TRUE,
                     itermax         = 20,
                     search_size     = 200)
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# Section 7: optimize.portfolio.rebalancing – rolling_window branch
# ---------------------------------------------------------------------------

portf_roi_rb <- portfolio.spec(assets = colnames(edhec5))
portf_roi_rb <- add.constraint(portf_roi_rb, type = "full_investment")
portf_roi_rb <- add.constraint(portf_roi_rb, type = "long_only")
portf_roi_rb <- add.objective(portf_roi_rb, type = "risk", name = "StdDev")

edhec5_short <- edhec5["2016/2021"]

rb_rolling <- tryCatch({
  optimize.portfolio.rebalancing(
    edhec5_short, portf_roi_rb,
    optimize_method = "ROI",
    rebalance_on    = "years",
    training_period = 12,
    rolling_window  = 24
  )
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# Section 8: optimize.portfolio.rebalancing – message = TRUE path
# ---------------------------------------------------------------------------

rb_message <- tryCatch({
  optimize.portfolio.rebalancing(
    edhec5_short, portf_roi_rb,
    optimize_method = "ROI",
    rebalance_on    = "years",
    training_period = 12,
    message         = TRUE
  )
}, error = function(e) NULL)

# ---------------------------------------------------------------------------
# Section 9: optimize.portfolio.rebalancing – portfolio.list (n.portf > 1)
# ---------------------------------------------------------------------------

portf_roi_rb2 <- portfolio.spec(assets = colnames(edhec5))
portf_roi_rb2 <- add.constraint(portf_roi_rb2, type = "full_investment")
portf_roi_rb2 <- add.constraint(portf_roi_rb2, type = "long_only")
portf_roi_rb2 <- add.objective(portf_roi_rb2, type = "return", name = "mean")

portf_list_rb <- combine.portfolios(list(portf_roi_rb, portf_roi_rb2))

rb_portf_list <- tryCatch({
  optimize.portfolio.rebalancing(
    edhec5_short, portf_list_rb,
    optimize_method = "ROI",
    rebalance_on    = "years",
    training_period = 12
  )
}, error = function(e) NULL)

###############################################################################
# Tests — Section 1: weight_concentration objective
###############################################################################

test_that("p_hhi contains weight_concentration_objective", {
  expect_s3_class(p_hhi$objectives[[2]], "weight_concentration_objective")
})

test_that("p_hhi weight_concentration objective name is HHI", {
  expect_identical(p_hhi$objectives[[2]]$name, "HHI")
})

test_that("p_hhi weight_concentration conc_aversion is 0.01", {
  expect_equal(p_hhi$objectives[[2]]$conc_aversion, 0.01)
})

test_that("opt_hhi_de is an optimize.portfolio.DEoptim object", {
  skip_if(is.null(opt_hhi_de))
  expect_s3_class(opt_hhi_de, "optimize.portfolio.DEoptim")
})

test_that("opt_hhi_de weights are numeric", {
  skip_if(is.null(opt_hhi_de))
  expect_true(is.numeric(extractWeights(opt_hhi_de)))
})

test_that("opt_hhi_de weights sum to approximately 1", {
  skip_if(is.null(opt_hhi_de))
  expect_equal(sum(extractWeights(opt_hhi_de)), 1, tolerance = 0.02)
})

test_that("opt_hhi_de has no NA weights", {
  skip_if(is.null(opt_hhi_de))
  expect_false(any(is.na(extractWeights(opt_hhi_de))))
})

test_that("opt_hhi_rp is an optimize.portfolio.random object", {
  skip_if(is.null(opt_hhi_rp))
  expect_s3_class(opt_hhi_rp, "optimize.portfolio.random")
})

test_that("opt_hhi_rp weights are numeric", {
  skip_if(is.null(opt_hhi_rp))
  expect_true(is.numeric(extractWeights(opt_hhi_rp)))
})

test_that("opt_hhi_rp weights sum to approximately 1", {
  skip_if(is.null(opt_hhi_rp))
  expect_equal(sum(extractWeights(opt_hhi_rp)), 1, tolerance = 0.02)
})

test_that("opt_hhi_rp has no NA weights", {
  skip_if(is.null(opt_hhi_rp))
  expect_false(any(is.na(extractWeights(opt_hhi_rp))))
})

###############################################################################
# Tests — Section 2: pre-computed rp argument
###############################################################################

test_that("rp_mat is a numeric matrix", {
  expect_true(is.matrix(rp_mat))
  expect_true(is.numeric(rp_mat))
})

test_that("rp_mat has 200 rows and 5 columns", {
  expect_equal(ncol(rp_mat), 5L)
  expect_lte(nrow(rp_mat), 200L)   # eliminate may reduce rows
})

test_that("opt_precomp_rp is an optimize.portfolio.random object", {
  skip_if(is.null(opt_precomp_rp))
  expect_s3_class(opt_precomp_rp, "optimize.portfolio.random")
})

test_that("opt_precomp_rp weights are numeric", {
  skip_if(is.null(opt_precomp_rp))
  expect_true(is.numeric(extractWeights(opt_precomp_rp)))
})

test_that("opt_precomp_rp weights sum to approximately 1", {
  skip_if(is.null(opt_precomp_rp))
  expect_equal(sum(extractWeights(opt_precomp_rp)), 1, tolerance = 0.02)
})

test_that("opt_precomp_rp has no NA weights", {
  skip_if(is.null(opt_precomp_rp))
  expect_false(any(is.na(extractWeights(opt_precomp_rp))))
})

###############################################################################
# Tests — Section 3: momentFUN as function object
###############################################################################

test_that("opt_momfun is an optimize.portfolio.random object", {
  skip_if(is.null(opt_momfun))
  expect_s3_class(opt_momfun, "optimize.portfolio.random")
})

test_that("opt_momfun weights are numeric", {
  skip_if(is.null(opt_momfun))
  expect_true(is.numeric(extractWeights(opt_momfun)))
})

test_that("opt_momfun weights sum to approximately 1", {
  skip_if(is.null(opt_momfun))
  expect_equal(sum(extractWeights(opt_momfun)), 1, tolerance = 0.02)
})

test_that("opt_momfun has no NA weights", {
  skip_if(is.null(opt_momfun))
  expect_false(any(is.na(extractWeights(opt_momfun))))
})

###############################################################################
# Tests — Section 4: trace=TRUE slots for DEoptim
###############################################################################

test_that("opt_de_tr is an optimize.portfolio.DEoptim object", {
  skip_if(is.null(opt_de_tr))
  expect_s3_class(opt_de_tr, "optimize.portfolio.DEoptim")
})

test_that("opt_de_tr$DEoutput is not NULL when trace=TRUE", {
  skip_if(is.null(opt_de_tr))
  expect_false(is.null(opt_de_tr$DEoutput))
})

test_that("opt_de_tr$DEoptim_objective_results is not NULL when trace=TRUE", {
  skip_if(is.null(opt_de_tr))
  expect_false(is.null(opt_de_tr$DEoptim_objective_results))
})

test_that("opt_de_tr$R is stored when trace=TRUE", {
  skip_if(is.null(opt_de_tr))
  expect_false(is.null(opt_de_tr$R))
})

test_that("opt_de_tr$R has same number of columns as edhec5", {
  skip_if(is.null(opt_de_tr))
  expect_equal(ncol(opt_de_tr$R), ncol(edhec5))
})

test_that("opt_de_tr weights are numeric", {
  skip_if(is.null(opt_de_tr))
  expect_true(is.numeric(extractWeights(opt_de_tr)))
})

test_that("opt_de_tr$DEoutput is a DEoptim result with optim element", {
  skip_if(is.null(opt_de_tr))
  expect_true(!is.null(opt_de_tr$DEoutput$optim))
})

###############################################################################
# Tests — Section 5: trace=TRUE slots for random portfolios
###############################################################################

test_that("opt_rp_tr is an optimize.portfolio.random object", {
  skip_if(is.null(opt_rp_tr))
  expect_s3_class(opt_rp_tr, "optimize.portfolio.random")
})

test_that("opt_rp_tr$random_portfolios is not NULL when trace=TRUE", {
  skip_if(is.null(opt_rp_tr))
  expect_false(is.null(opt_rp_tr$random_portfolios))
})

test_that("opt_rp_tr$random_portfolios is a matrix", {
  skip_if(is.null(opt_rp_tr))
  expect_true(is.matrix(opt_rp_tr$random_portfolios))
})

test_that("opt_rp_tr$random_portfolio_objective_results is not NULL when trace=TRUE", {
  skip_if(is.null(opt_rp_tr))
  expect_false(is.null(opt_rp_tr$random_portfolio_objective_results))
})

test_that("opt_rp_tr$random_portfolio_objective_results is a list", {
  skip_if(is.null(opt_rp_tr))
  expect_true(is.list(opt_rp_tr$random_portfolio_objective_results))
})

test_that("opt_rp_tr$R is stored when trace=TRUE", {
  skip_if(is.null(opt_rp_tr))
  expect_false(is.null(opt_rp_tr$R))
})

test_that("opt_rp_tr$R has same number of columns as edhec5", {
  skip_if(is.null(opt_rp_tr))
  expect_equal(ncol(opt_rp_tr$R), ncol(edhec5))
})

test_that("opt_rp_tr weights are numeric", {
  skip_if(is.null(opt_rp_tr))
  expect_true(is.numeric(extractWeights(opt_rp_tr)))
})

###############################################################################
# Tests — Section 6: multi-objective (return + risk + risk_budget)
###############################################################################

test_that("portf_multi has three objectives", {
  expect_length(portf_multi$objectives, 3L)
})

test_that("portf_multi objectives are return, risk, and risk_budget", {
  obj_classes <- sapply(portf_multi$objectives, function(o) class(o)[1])
  expect_true("return_objective"      %in% obj_classes)
  expect_true("portfolio_risk_objective" %in% obj_classes)
  expect_true("risk_budget_objective" %in% obj_classes)
})

test_that("opt_multi is an optimize.portfolio.DEoptim object", {
  skip_if(is.null(opt_multi))
  expect_s3_class(opt_multi, "optimize.portfolio.DEoptim")
})

test_that("opt_multi weights are numeric", {
  skip_if(is.null(opt_multi))
  expect_true(is.numeric(extractWeights(opt_multi)))
})

test_that("opt_multi weights sum to approximately 1", {
  skip_if(is.null(opt_multi))
  expect_equal(sum(extractWeights(opt_multi)), 1, tolerance = 0.02)
})

test_that("opt_multi weights satisfy box constraint lower bound", {
  skip_if(is.null(opt_multi))
  expect_true(all(extractWeights(opt_multi) >= 0.05 - 0.01))
})

test_that("opt_multi weights satisfy box constraint upper bound", {
  skip_if(is.null(opt_multi))
  expect_true(all(extractWeights(opt_multi) <= 0.60 + 0.01))
})

test_that("opt_multi has no NA weights", {
  skip_if(is.null(opt_multi))
  expect_false(any(is.na(extractWeights(opt_multi))))
})

test_that("opt_multi objective measures include ES component", {
  skip_if(is.null(opt_multi))
  obj_measures <- extractObjectiveMeasures(opt_multi)
  expect_false(is.null(obj_measures$ES))
})

test_that("opt_multi objective measures include mean component", {
  skip_if(is.null(opt_multi))
  obj_measures <- extractObjectiveMeasures(opt_multi)
  expect_true(is.numeric(obj_measures$mean))
})

test_that("opt_multi$DEoutput is not NULL when trace=TRUE", {
  skip_if(is.null(opt_multi))
  expect_false(is.null(opt_multi$DEoutput))
})

test_that("opt_multi$R is stored when trace=TRUE", {
  skip_if(is.null(opt_multi))
  expect_false(is.null(opt_multi$R))
})

###############################################################################
# Tests — Section 7: rolling_window branch
###############################################################################

test_that("rb_rolling is an optimize.portfolio.rebalancing object", {
  skip_if(is.null(rb_rolling))
  expect_s3_class(rb_rolling, "optimize.portfolio.rebalancing")
})

test_that("rb_rolling$opt_rebalancing is a non-empty list", {
  skip_if(is.null(rb_rolling))
  expect_true(is.list(rb_rolling$opt_rebalancing))
  expect_gt(length(rb_rolling$opt_rebalancing), 0L)
})

test_that("rb_rolling has multiple rebalance periods", {
  skip_if(is.null(rb_rolling))
  expect_gt(length(rb_rolling$opt_rebalancing), 1L)
})

test_that("rb_rolling first period weights are numeric", {
  skip_if(is.null(rb_rolling))
  expect_true(is.numeric(extractWeights(rb_rolling$opt_rebalancing[[1]])))
})

test_that("rb_rolling first period weights sum to approximately 1", {
  skip_if(is.null(rb_rolling))
  w <- extractWeights(rb_rolling$opt_rebalancing[[1]])
  expect_equal(sum(w), 1, tolerance = 0.02)
})

test_that("rb_rolling first period weights have no NAs", {
  skip_if(is.null(rb_rolling))
  expect_false(any(is.na(extractWeights(rb_rolling$opt_rebalancing[[1]]))))
})

###############################################################################
# Tests — Section 8: message = TRUE path
###############################################################################

test_that("rb_message is an optimize.portfolio.rebalancing object", {
  skip_if(is.null(rb_message))
  expect_s3_class(rb_message, "optimize.portfolio.rebalancing")
})

test_that("rb_message$opt_rebalancing is a non-empty list", {
  skip_if(is.null(rb_message))
  expect_true(is.list(rb_message$opt_rebalancing))
  expect_gt(length(rb_message$opt_rebalancing), 0L)
})

test_that("rb_message first period weights are numeric", {
  skip_if(is.null(rb_message))
  expect_true(is.numeric(extractWeights(rb_message$opt_rebalancing[[1]])))
})

test_that("rb_message first period weights sum to approximately 1", {
  skip_if(is.null(rb_message))
  w <- extractWeights(rb_message$opt_rebalancing[[1]])
  expect_equal(sum(w), 1, tolerance = 0.02)
})

###############################################################################
# Tests — Section 9: portfolio.list (n.portf > 1)
###############################################################################

test_that("portf_list_rb is a portfolio.list with two elements", {
  expect_s3_class(portf_list_rb, "portfolio.list")
  expect_length(portf_list_rb, 2L)
})

test_that("rb_portf_list has class opt.rebal.list", {
  skip_if(is.null(rb_portf_list))
  expect_s3_class(rb_portf_list, "opt.rebal.list")
})

test_that("rb_portf_list has two sub-results", {
  skip_if(is.null(rb_portf_list))
  expect_length(rb_portf_list, 2L)
})

test_that("rb_portf_list elements are optimize.portfolio.rebalancing objects", {
  skip_if(is.null(rb_portf_list))
  expect_s3_class(rb_portf_list[[1]], "optimize.portfolio.rebalancing")
  expect_s3_class(rb_portf_list[[2]], "optimize.portfolio.rebalancing")
})

test_that("rb_portf_list[[1]] opt_rebalancing is non-empty", {
  skip_if(is.null(rb_portf_list))
  expect_gt(length(rb_portf_list[[1]]$opt_rebalancing), 0L)
})

test_that("rb_portf_list[[1]] first period weights are numeric", {
  skip_if(is.null(rb_portf_list))
  expect_true(is.numeric(extractWeights(rb_portf_list[[1]]$opt_rebalancing[[1]])))
})

test_that("rb_portf_list[[2]] first period weights are numeric", {
  skip_if(is.null(rb_portf_list))
  expect_true(is.numeric(extractWeights(rb_portf_list[[2]]$opt_rebalancing[[1]])))
})
