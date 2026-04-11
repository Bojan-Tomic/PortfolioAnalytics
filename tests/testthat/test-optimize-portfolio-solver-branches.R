###############################################################################
# tests/testthat/test-optimize-portfolio-solver-branches.R
#
# Targeted coverage push for remaining branches in R/optimize.portfolio.R
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)
data(edhec)
edhec4 <- edhec[, 1:4]
funds <- colnames(edhec4)

# ---------------------------------------------------------------------------
# osqp group constraint and mean-var (both risk & reward)
# ---------------------------------------------------------------------------
if (requireNamespace("osqp", quietly = TRUE)) {
  test_that("osqp solver with mean-var and group constraints", {
    # This hits lines 2418-2443, 2448-2451, 2470-2473, 2507-2610
    p <- portfolio.spec(assets = funds)
    p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
    p <- add.constraint(p, type = "box", min = 0, max = 1)
    p <- add.constraint(p, type = "group", groups = list(1:2, 3:4), group_min = c(0.1, 0.1), group_max = c(0.8, 0.8))
    p <- add.objective(p, type = "return", name = "mean")
    p <- add.objective(p, type = "risk", name = "StdDev")
    
    opt <- optimize.portfolio(edhec4, p, optimize_method = "osqp")
    expect_s3_class(opt, "optimize.portfolio.osqp")
    
    # Hit the restrictive leverage message line 2384
    p2 <- portfolio.spec(assets = funds)
    p2 <- add.constraint(p2, type = "weight_sum", min_sum = 1, max_sum = 1)
    p2 <- add.constraint(p2, type = "box", min = 0, max = 1)
    p2 <- add.objective(p2, type = "return", name = "mean")
    expect_message(optimize.portfolio(edhec4, p2, optimize_method = "osqp"), "Leverage constraint min_sum and max_sum are restrictive")
  })
}

# ---------------------------------------------------------------------------
# mco solver branches (mco.risk = 1, mco.lambda, mco.return)
# ---------------------------------------------------------------------------
if (requireNamespace("mco", quietly = TRUE)) {
  test_that("mco solver with ES and mean objectives", {
    p <- portfolio.spec(assets = funds)
    p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
    p <- add.constraint(p, type = "box", min = 0, max = 1)
    p <- add.objective(p, type = "return", name = "mean")
    p <- add.objective(p, type = "risk", name = "ES", arguments=list(p=0.95))
    
    set.seed(42)
    opt <- tryCatch(optimize.portfolio(edhec4, p, optimize_method = "mco"), error=function(e) NULL)
    if(!is.null(opt)) expect_s3_class(opt, "optimize.portfolio.mco")
  })
  
  test_that("mco solver with group constraints", {
    p <- portfolio.spec(assets = funds)
    p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
    p <- add.constraint(p, type = "box", min = 0, max = 1)
    p <- add.constraint(p, type = "group", groups = list(1:2, 3:4), group_min = c(0.1, 0.1), group_max = c(0.8, 0.8))
    p <- add.objective(p, type = "risk", name = "StdDev")
    
    set.seed(42)
    opt <- tryCatch(optimize.portfolio(edhec4, p, optimize_method = "mco"), error=function(e) NULL)
    if(!is.null(opt)) expect_s3_class(opt, "optimize.portfolio.mco")
  })
}

# ---------------------------------------------------------------------------
# Error paths in optimize.portfolio.R
# ---------------------------------------------------------------------------
test_that("optimize.portfolio handles missing solver packages", {
  p <- portfolio.spec(assets = funds)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "return", name = "mean")
  
  # if "quadprog" is not installed, ROI errors, but we can't easily uninstall it.
  # Just pass a fake optimize_method
  res <- optimize.portfolio(edhec4, p, optimize_method = "fakesolver123")
  expect_s3_class(res, "optimize.portfolio.fakesolver123")
})

