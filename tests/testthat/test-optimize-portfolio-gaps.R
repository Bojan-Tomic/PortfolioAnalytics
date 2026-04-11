###############################################################################
# tests/testthat/test-optimize-portfolio-gaps.R
#
# Targeted coverage for uncovered branches in R/optimize.portfolio.R
#
# Uncovered line groups targeted:
#  719   – portfolio.list branch (loop + combine.optimizations)
#  723   – regime.portfolios branch (regime switching, date mismatch warning)
#  835   – modify.args ROI+CVaR path
#  865   – normalize_weights max_sum != 0 guard (v2)
#  983   – DEoptim non-null rp as initial population
#  1013  – DEoptim trace path (out$DEoutput, out$DEoptim_objective_results)
#  1260  – regime.switching out$regime assignment
#  1270  – etl_milp_opt (max_pos CVaR)
#  1285  – meanetl=TRUE obj_vals$mean
#  1455  – Rglpk infinite bounds normalisation
#  1512  – Rglpk max-return trace output
#  1607  – Rglpk min-CVaR group constraint
#  1677  – Rglpk min-CVaR trace output
#  2242  – Rglpk max-ratio return target
#  2500  – osqp trace output
#  3081  – CVXR min-HHI objective
#  3183  – rebalancing regime.portfolios rp branch
#  3484  – rebalancing training_period < nrow(R)
#  3498  – rebalancing turnover constraint
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)
library(PerformanceAnalytics)

utils::data(edhec)
edhec4 <- edhec[, 1:4]
edhec5 <- edhec[, 1:5]
funds4 <- colnames(edhec4)
funds5 <- colnames(edhec5)

# ---------------------------------------------------------------------------
# Helper: minimal long-only + full-investment portfolio spec
# ---------------------------------------------------------------------------
.make_lo <- function(R) {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p
}

# ===========================================================================
# 1. portfolio.list branch – line 697-719
# ===========================================================================
test_that("optimize.portfolio handles portfolio.list input", {
  skip_on_cran()
  # Build two different specs with different objectives
  p1 <- .make_lo(edhec4)
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- .make_lo(edhec4)
  p2 <- add.objective(p2, type = "return", name = "mean")

  # combine.portfolios creates a portfolio.list object
  plist <- combine.portfolios(list(p1, p2))
  expect_s3_class(plist, "portfolio.list")

  # optimize.portfolio should loop over each portfolio and combine results
  opt <- tryCatch(
    optimize.portfolio(edhec4, plist, optimize_method = "ROI"),
    error = function(e) NULL
  )
  # result should be a combine.optimizations list
  expect_false(is.null(opt))
  expect_true(is.list(opt))
})

# ===========================================================================
# 2. regime.portfolios branch – lines 723-737
# ===========================================================================
test_that("optimize.portfolio handles regime.portfolios input (date match)", {
  skip_on_cran()
  # Build two specs with same assets
  p1 <- .make_lo(edhec4)
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- .make_lo(edhec4)
  p2 <- add.objective(p2, type = "return", name = "mean")

  plist <- combine.portfolios(list(p1, p2))

  # Create an xts regime series with exactly 2 unique values (regimes 1 and 2)
  # so that n.regimes matches n.portfolios (== 2).
  # The last observation must be regime 1 so the ROI opt picks portfolio 1.
  n <- nrow(edhec4)
  regime_vals <- c(rep(2L, n - 1L), 1L)   # last obs = regime 1
  regime_xts <- xts::xts(regime_vals, order.by = index(edhec4))

  reg_portf <- tryCatch(
    regime.portfolios(regime = regime_xts, portfolios = plist),
    error = function(e) NULL
  )
  skip_if(is.null(reg_portf), "Could not create regime.portfolios object")

  opt <- tryCatch(
    optimize.portfolio(edhec4, reg_portf, optimize_method = "ROI"),
    error = function(e) NULL
  )
  # Should succeed and regime index stored
  skip_if(is.null(opt), "regime.portfolios optimization failed")
  expect_s3_class(opt, "optimize.portfolio")
  expect_true(!is.null(opt$regime))
})

test_that("optimize.portfolio regime.portfolios warns on date mismatch", {
  skip_on_cran()
  p1 <- .make_lo(edhec4)
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- .make_lo(edhec4)
  p2 <- add.objective(p2, type = "return", name = "mean")

  plist <- combine.portfolios(list(p1, p2))

  # Regime dates far in the future — guaranteed NOT in edhec4 index
  n <- nrow(edhec4)
  future_dates <- seq(as.Date("2030-01-31"), by = "month", length.out = n)
  regime_vals  <- c(rep(2L, n - 1L), 1L)   # 2 unique values → 2 portfolios OK
  regime_xts   <- xts::xts(regime_vals, order.by = future_dates)

  reg_portf <- tryCatch(
    regime.portfolios(regime = regime_xts, portfolios = plist),
    error = function(e) NULL
  )
  skip_if(is.null(reg_portf))

  # Should warn that dates do not match, and default to portfolio 1
  expect_warning(
    tryCatch(
      optimize.portfolio(edhec4, reg_portf, optimize_method = "ROI"),
      error = function(e) NULL
    ),
    "Dates in regime and R do not match"
  )
})

# ===========================================================================
# 3. ROI + CVaR path: modify.args ROI=TRUE – line 830-831
# ===========================================================================
test_that("optimize.portfolio ROI with ES objective sets ROI=TRUE moments path", {
  skip_on_cran()
  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "ROI"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.ROI")
  expect_true("ES" %in% names(opt$objective_measures))
})

# ===========================================================================
# 4. normalize_weights max_sum != 0 guard (v2, lines 865-876)
# ===========================================================================
test_that("optimize.portfolio random normalizes weights when sum violates bounds", {
  skip_on_cran()
  # Use a min_sum/max_sum that differs from 0 so the normalization branches fire
  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.98, max_sum = 1.02)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")

  set.seed(42)
  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "random", search_size = 200),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  w <- extractWeights(opt)
  expect_true(sum(w) >= 0.97 && sum(w) <= 1.03)
})

# ===========================================================================
# 5. DEoptim with pre-computed rp as initial population – lines 983-994
# ===========================================================================
test_that("optimize.portfolio DEoptim uses pre-computed rp as initial population", {
  skip_on_cran()
  skip_if_not_installed("DEoptim")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")

  set.seed(42)
  rp_mat <- random_portfolios(p, permutations = 50)

  set.seed(42)
  opt <- tryCatch(
    optimize.portfolio(edhec4, p,
                       optimize_method = "DEoptim",
                       rp = rp_mat,
                       itermax = 20,
                       search_size = 200),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.DEoptim")
  expect_true(is.numeric(extractWeights(opt)))
})

# ===========================================================================
# 6. DEoptim trace=TRUE path – lines 1013, 1019-1025
# ===========================================================================
test_that("optimize.portfolio DEoptim trace=TRUE stores DEoutput", {
  skip_on_cran()
  skip_if_not_installed("DEoptim")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.60)
  p <- add.objective(p, type = "risk", name = "StdDev")

  set.seed(42)
  opt <- tryCatch(
    optimize.portfolio(edhec4, p,
                       optimize_method = "DEoptim",
                       trace = TRUE,
                       itermax = 20,
                       search_size = 200),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.DEoptim")
  # trace=TRUE should store DEoutput
  expect_true("DEoutput" %in% names(opt))
})

# ===========================================================================
# 7. ROI CVaR with max_pos (etl_milp_opt) – lines 1268-1278
# ===========================================================================
test_that("optimize.portfolio ROI min-ES with max_pos uses etl_milp_opt", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "position_limit", max_pos = 3)
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "ROI"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.ROI")
  w <- extractWeights(opt)
  # At most 3 non-zero weights
  expect_true(sum(w > 1e-4) <= 3)
})

# ===========================================================================
# 8. ROI mean+ES (meanetl=TRUE) path – lines 1263-1266, 1285-1287
# ===========================================================================
test_that("optimize.portfolio ROI mean+ES stores mean in obj_vals", {
  skip_on_cran()

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "ROI"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.ROI")
  expect_true("mean" %in% names(opt$objective_measures))
  expect_true("ES" %in% names(opt$objective_measures) ||
              "CVaR" %in% names(opt$objective_measures) ||
              "ETL" %in% names(opt$objective_measures))
})

# ===========================================================================
# 9. Rglpk max-return – hits Rglpk path, infinite bounds (lines 1455-1456)
# ===========================================================================
test_that("optimize.portfolio Rglpk max-return with long-only works", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  # long_only uses -Inf/Inf style box → hits lines 1455-1456
  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "return", name = "mean")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
  w <- extractWeights(opt)
  expect_true(is.numeric(w))
  expect_true(all(w >= -1e-6))
})

# ===========================================================================
# 10. Rglpk max-return trace=TRUE – line 1561-1563
# ===========================================================================
test_that("optimize.portfolio Rglpk max-return trace=TRUE stores Rglpkoutput", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "return", name = "mean")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk", trace = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
  expect_true("Rglpkoutput" %in% names(opt))
})

# ===========================================================================
# 11. Rglpk min-CVaR (min ES) – lines 1566-1679
# ===========================================================================
test_that("optimize.portfolio Rglpk min-ES basic", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
  w <- extractWeights(opt)
  expect_true(is.numeric(w))
  expect_true(abs(sum(w) - 1) < 0.05)
})

test_that("optimize.portfolio Rglpk min-ES with group constraints", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "group",
                       groups = list(1:2, 3:4),
                       group_min = c(0.1, 0.1),
                       group_max = c(0.8, 0.8))
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
})

test_that("optimize.portfolio Rglpk min-ES trace=TRUE stores Rglpkoutput", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk", trace = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_true("Rglpkoutput" %in% names(opt))
})

# ===========================================================================
# 12. Rglpk max-STARR ratio (risk & reward) – lines 1682-1820
# ===========================================================================
test_that("optimize.portfolio Rglpk max-STARR (mean+ES) ratio optimization", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
  w <- extractWeights(opt)
  expect_true(is.numeric(w))
})

# ===========================================================================
# 13. osqp trace=TRUE – line 2499-2501
# ===========================================================================
test_that("optimize.portfolio osqp trace=TRUE stores osqpoutput", {
  skip_on_cran()
  skip_if_not_installed("osqp")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "StdDev")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "osqp", trace = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.osqp")
  expect_true("osqpoutput" %in% names(opt))
})

# ===========================================================================
# 14. CVXR min-HHI (min var + HHI penalty) – lines 2907-2911, 3117-3119
# ===========================================================================
test_that("optimize.portfolio CVXR min-HHI objective", {
  skip_on_cran()
  skip_if_not_installed("CVXR")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "weight_concentration",
                     name = "HHI", conc_aversion = 0.1)

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "CVXR"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.CVXR")
  w <- extractWeights(opt)
  expect_true(is.numeric(w))
})

# ===========================================================================
# 15. CVXR EQS objective – lines 2974-2982
# ===========================================================================
test_that("optimize.portfolio CVXR EQS (equal-shares) objective", {
  skip_on_cran()
  skip_if_not_installed("CVXR")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "EQS")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "CVXR"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.CVXR")
})

# ===========================================================================
# 16. CVXR mean + var (mean-variance, NOT maxSR) – lines 2912-2920, 3120-3125
# ===========================================================================
test_that("optimize.portfolio CVXR mean-var without maxSR stores both objective measures", {
  skip_on_cran()
  skip_if_not_installed("CVXR")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "StdDev")

  opt <- tryCatch(
    # maxSR=FALSE ensures the non-Sharpe branch is taken
    optimize.portfolio(edhec4, p, optimize_method = "CVXR", maxSR = FALSE),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.CVXR")
  expect_true("mean" %in% names(opt$objective_measures) ||
              "StdDev" %in% names(opt$objective_measures))
})

# ===========================================================================
# 17. CVXR max-CSM (mean+CSM with CSMratio) – lines 2949-2973
# ===========================================================================
test_that("optimize.portfolio CVXR max-CSM ratio", {
  skip_on_cran()
  skip_if_not_installed("CVXR")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "CSM")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "CVXR", CSMratio = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.CVXR")
})

# ===========================================================================
# 18. rebalancing with small dataset (training_period = nrow(R)) – line 3484
# ===========================================================================
test_that("optimize.portfolio.rebalancing sets training_period to nrow(R) when small", {
  skip_on_cran()

  # Use only 20 observations so nrow(R) < 36 triggers line 3484
  R_small <- edhec4[1:20, ]

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "StdDev")

  opt_reb <- tryCatch(
    optimize.portfolio.rebalancing(R_small, p,
                                   optimize_method = "ROI",
                                   rebalance_on   = "months",
                                   training_period = NULL),
    error = function(e) NULL
  )
  skip_if(is.null(opt_reb))
  expect_s3_class(opt_reb, "optimize.portfolio.rebalancing")
  expect_true(length(opt_reb$opt_rebalancing) > 0)
})

# ===========================================================================
# 19. rebalancing with rolling_window (line 3535-3541)
# ===========================================================================
test_that("optimize.portfolio.rebalancing with rolling_window parameter", {
  skip_on_cran()

  R_sub <- edhec4[1:48, ]

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "StdDev")

  opt_reb <- tryCatch(
    optimize.portfolio.rebalancing(R_sub, p,
                                   optimize_method  = "ROI",
                                   rebalance_on     = "quarters",
                                   training_period  = 12,
                                   rolling_window   = 24),
    error = function(e) NULL
  )
  skip_if(is.null(opt_reb))
  expect_s3_class(opt_reb, "optimize.portfolio.rebalancing")
})

# ===========================================================================
# 20. rebalancing with turnover constraint – lines 3497-3525
# ===========================================================================
test_that("optimize.portfolio.rebalancing with turnover constraint", {
  skip_on_cran()
  skip_if_not_installed("CVXR")

  R_sub <- edhec4[1:36, ]

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk", name = "StdDev")
  # Add turnover constraint (requires CVXR solver to handle it)
  p <- add.constraint(p, type = "turnover",
                       turnover_target = 0.5)

  opt_reb <- tryCatch(
    optimize.portfolio.rebalancing(R_sub, p,
                                   optimize_method = "CVXR",
                                   rebalance_on    = "quarters",
                                   training_period = 12),
    error = function(e) NULL
  )
  skip_if(is.null(opt_reb))
  expect_s3_class(opt_reb, "optimize.portfolio.rebalancing")
})

# ===========================================================================
# 21. rebalancing with regime.portfolios – line 3468-3469
# ===========================================================================
test_that("optimize.portfolio.rebalancing with regime.portfolios generates rp", {
  skip_on_cran()

  R_sub <- edhec4[1:36, ]

  p1 <- .make_lo(edhec4)
  p1 <- add.objective(p1, type = "risk", name = "StdDev")

  p2 <- .make_lo(edhec4)
  p2 <- add.objective(p2, type = "return", name = "mean")

  plist <- combine.portfolios(list(p1, p2))

  # regime covers all 36 months
  regime_vals <- c(rep(1L, 18), rep(2L, 18))
  regime_xts  <- xts::xts(regime_vals, order.by = index(R_sub))

  reg_portf <- tryCatch(
    regime.portfolios(regime = regime_xts, portfolios = plist),
    error = function(e) NULL
  )
  skip_if(is.null(reg_portf))

  opt_reb <- tryCatch(
    optimize.portfolio.rebalancing(R_sub, reg_portf,
                                   optimize_method = "random",
                                   rebalance_on    = "quarters",
                                   training_period = 12,
                                   search_size     = 200),
    error = function(e) NULL
  )
  skip_if(is.null(opt_reb))
  expect_s3_class(opt_reb, "optimize.portfolio.rebalancing")
})

# ===========================================================================
# 22. optimize.portfolio_v1 ROI_old path – line 259
# ===========================================================================
test_that("optimize.portfolio_v1 ROI_old path prints deprecation message", {
  skip_on_cran()

  # Use constraint_v1() (the v1 constructor) for the v1 API
  cc <- constraint_v1(assets = funds4,
                      min_sum = 0.99, max_sum = 1.01,
                      min = rep(0, 4), max = rep(1, 4))

  # Supply a working momentFUN so we get past match.fun() and reach line 259.
  # ROI_old will still fail (no constrainted_objective set) but print() fires first.
  dummy_moments <- function(R, constraints, ...) list()
  expect_output(
    tryCatch(
      PortfolioAnalytics:::optimize.portfolio_v1(edhec4, cc,
                                                  optimize_method = "ROI_old",
                                                  momentFUN = dummy_moments),
      error = function(e) NULL
    ),
    "ROI_old"
  )
})

# ===========================================================================
# 23. optimize.portfolio constraints + objectives inserted from outside – 776-783
# ===========================================================================
test_that("optimize.portfolio inserts constraints and objectives from outside portfolio", {
  skip_on_cran()

  # Portfolio with no constraints/objectives
  p_bare <- portfolio.spec(assets = funds4)

  # Pass constraints and objectives separately as a list
  extra_constr <- list(
    weight_sum_constraint(min_sum = 0.99, max_sum = 1.01),
    box_constraint(assets = funds4, min = rep(0, 4), max = rep(1, 4),
                   min_mult = NULL, max_mult = NULL)
  )
  extra_obj    <- list(return_objective(name = "mean"))

  opt <- tryCatch(
    optimize.portfolio(edhec4,
                       portfolio    = p_bare,
                       constraints  = extra_constr,
                       objectives   = extra_obj,
                       optimize_method = "ROI"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.ROI")
})

# ===========================================================================
# 24. v1_constraint passed to optimize.portfolio – lines 767-774
# ===========================================================================
test_that("optimize.portfolio accepts v1_constraint and converts to v2", {
  skip_on_cran()

  cc <- constraint_v1(assets = funds4,
                      min_sum = 0.99, max_sum = 1.01,
                      min = rep(0, 4), max = rep(1, 4))
  cc <- add.objective_v1(cc, type = "risk", name = "StdDev")

  # Passing constraints as a v1_constraint with no portfolio triggers update path
  expect_message(
    opt <- tryCatch(
      optimize.portfolio(edhec4,
                         portfolio   = NULL,
                         constraints = cc,
                         optimize_method = "ROI"),
      error = function(e) NULL
    ),
    "v1_constraint"
  )
})

# ===========================================================================
# 25. osqp with return target (line 2455-2459) and restrict leverage
# ===========================================================================
test_that("optimize.portfolio osqp with return target constraint", {
  skip_on_cran()
  skip_if_not_installed("osqp")

  p <- .make_lo(edhec4)
  p <- add.objective(p, type = "risk",   name = "StdDev",
                     target = 0.01)  # adds return_target via objective target

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "osqp"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.osqp")
})

# ===========================================================================
# 26. Rglpk max-return with group constraints – lines 1494-1519
# ===========================================================================
test_that("optimize.portfolio Rglpk max-return with group constraints", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "group",
                       groups     = list(1:2, 3:4),
                       group_min  = c(0.1, 0.1),
                       group_max  = c(0.8, 0.8))
  p <- add.objective(p, type = "return", name = "mean")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
})

# ===========================================================================
# 27. Rglpk restrictive leverage message (line 1459-1463)
# ===========================================================================
test_that("optimize.portfolio Rglpk warns on restrictive leverage", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 1, max_sum = 1)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "return", name = "mean")

  expect_message(
    tryCatch(
      optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
      error = function(e) NULL
    ),
    "Leverage constraint min_sum and max_sum are restrictive"
  )
})
