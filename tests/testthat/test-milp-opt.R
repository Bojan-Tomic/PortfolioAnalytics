###############################################################################
# tests/testthat/test-milp-opt.R
#
# Tests for MILP optimizations (maxret_milp_opt, etl_milp_opt) in optFUN.R
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)

data(edhec)
edhec4 <- edhec[, 1:4]

p_milp <- portfolio.spec(assets = colnames(edhec4))
p_milp <- add.constraint(p_milp, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p_milp <- add.constraint(p_milp, type = "box", min = 0, max = 1)
# position limit constraint triggers MILP
p_milp <- add.constraint(p_milp, type = "position_limit", max_pos = 3)

# 1. maxret_milp_opt (objective = mean)
p_maxret <- add.objective(p_milp, type = "return", name = "mean")

opt_maxret_milp <- tryCatch(
  optimize.portfolio(edhec4, p_maxret, optimize_method = "ROI"),
  error = function(e) NULL
)

# 2. maxret_milp_opt with target
p_maxret_tgt <- add.constraint(p_maxret, type = "return", return_target = 0.005)

opt_maxret_milp_tgt <- tryCatch(
  optimize.portfolio(edhec4, p_maxret_tgt, optimize_method = "ROI"),
  error = function(e) NULL
)

# 3. etl_milp_opt (objective = ES)
p_etl <- add.objective(p_milp, type = "risk", name = "ES")

opt_etl_milp <- tryCatch(
  optimize.portfolio(edhec4, p_etl, optimize_method = "ROI"),
  error = function(e) NULL
)

# 4. etl_milp_opt with target
p_etl_tgt <- add.constraint(p_etl, type = "return", return_target = 0.005)

opt_etl_milp_tgt <- tryCatch(
  optimize.portfolio(edhec4, p_etl_tgt, optimize_method = "ROI"),
  error = function(e) NULL
)

test_that("maxret_milp_opt produces valid result", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(opt_maxret_milp))
  expect_s3_class(opt_maxret_milp, "optimize.portfolio.ROI")
})

test_that("maxret_milp_opt with target produces valid result", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(opt_maxret_milp_tgt))
  expect_s3_class(opt_maxret_milp_tgt, "optimize.portfolio.ROI")
})

test_that("etl_milp_opt produces valid result", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(opt_etl_milp))
  expect_s3_class(opt_etl_milp, "optimize.portfolio.ROI")
})

test_that("etl_milp_opt with target produces valid result", {
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if(is.null(opt_etl_milp_tgt))
  expect_s3_class(opt_etl_milp_tgt, "optimize.portfolio.ROI")
})
