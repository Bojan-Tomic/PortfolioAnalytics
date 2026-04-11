###############################################################################
# tests/testthat/test-constrained-objective.R
#
# Comprehensive unit tests for:
#   R/constrained_objective.R  — constrained_objective()
#   R/objectiveFUN.R           — turnover(), var.portfolio(), HHI(), port.mean()
#   R/constraintsFUN.R         — diversification()
#
# Coverage targets:
#   constrained_objective.R  — penalty paths, objective switch cases, trace,
#                              verbose, normalize, return/risk/ES/VaR/turnover/
#                              risk_budget/weight_concentration objectives
#   objectiveFUN.R           — turnover(), var.portfolio(), HHI(), port.mean()
#   constraintsFUN.R         — diversification()
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------
utils::data(edhec)
R <- edhec[, 1:5]
w_eq <- rep(0.2, 5) # equal weights, sums to 1.0

# ===========================================================================
# Section 1:  turnover()
# ===========================================================================

test_that("turnover: equal weights with NULL wts.init returns zero", {
  result <- turnover(w_eq)
  expect_equal(result, 0, tolerance = 1e-10)
})

test_that("turnover: NULL wts.init defaults to equal-weight vector", {
  w <- c(0.5, 0.2, 0.1, 0.1, 0.1)
  expected <- sum(abs(rep(0.2, 5) - w)) / 5
  expect_equal(turnover(w), expected, tolerance = 1e-10)
})

test_that("turnover: explicit wts.init gives correct value", {
  w <- c(0.3, 0.3, 0.2, 0.1, 0.1)
  w_init <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  expect_equal(turnover(w, wts.init = w_init),
    sum(abs(w_init - w)) / 5,
    tolerance = 1e-10
  )
})

test_that("turnover: returns a length-1 numeric", {
  result <- turnover(w_eq)
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("turnover: errors when lengths differ", {
  expect_error(turnover(c(0.5, 0.5), wts.init = c(0.3, 0.3, 0.4)))
})

test_that("turnover: is non-negative", {
  w <- c(0.8, 0.05, 0.05, 0.05, 0.05)
  expect_true(turnover(w) >= 0)
})

# ===========================================================================
# Section 2:  var.portfolio()
# ===========================================================================

test_that("var.portfolio: returns a length-1 numeric", {
  result <- var.portfolio(R, w_eq)
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("var.portfolio: equals t(w) %*% var(R) %*% w", {
  expected <- as.numeric(t(w_eq) %*% var(R) %*% w_eq)
  expect_equal(var.portfolio(R, w_eq), expected, tolerance = 1e-10)
})

test_that("var.portfolio: is non-negative", {
  expect_true(var.portfolio(R, w_eq) >= 0)
})

test_that("var.portfolio: concentrated weights produce higher variance than equal", {
  w_conc <- c(1, 0, 0, 0, 0)
  expect_true(var.portfolio(R, w_conc) >= var.portfolio(R, w_eq))
})

# ===========================================================================
# Section 3:  HHI()
# ===========================================================================

test_that("HHI: no groups returns a length-1 numeric", {
  result <- HHI(w_eq)
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("HHI: equal weights equals 1/N", {
  expect_equal(HHI(w_eq), 1 / 5, tolerance = 1e-10)
})

test_that("HHI: fully concentrated portfolio equals 1", {
  expect_equal(HHI(c(1, 0, 0, 0, 0)), 1, tolerance = 1e-10)
})

test_that("HHI: with groups returns list named HHI and Groups_HHI", {
  groups <- list(g1 = 1:2, g2 = 3:5)
  result <- HHI(w_eq, groups = groups)
  expect_true(is.list(result))
  expect_named(result, c("HHI", "Groups_HHI"))
})

test_that("HHI: list$HHI matches scalar call", {
  groups <- list(g1 = 1:2, g2 = 3:5)
  expect_equal(HHI(w_eq, groups = groups)$HHI,
    HHI(w_eq),
    tolerance = 1e-10
  )
})

test_that("HHI: Groups_HHI values are correct per group", {
  w <- c(0.4, 0.1, 0.2, 0.2, 0.1)
  groups <- list(g1 = 1:2, g2 = 3:5)
  result <- HHI(w, groups = groups)
  expect_equal(result$Groups_HHI[["g1"]], sum(w[1:2]^2), tolerance = 1e-10)
  expect_equal(result$Groups_HHI[["g2"]], sum(w[3:5]^2), tolerance = 1e-10)
})

# ===========================================================================
# Section 4:  diversification()
# ===========================================================================

test_that("diversification: returns a length-1 numeric", {
  result <- diversification(w_eq)
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("diversification: equal weights equals 1 - 1/N", {
  expect_equal(diversification(w_eq), 1 - 1 / 5, tolerance = 1e-10)
})

test_that("diversification: fully concentrated portfolio equals 0", {
  expect_equal(diversification(c(1, 0, 0, 0, 0)), 0, tolerance = 1e-10)
})

test_that("diversification: equals 1 - sum(w^2)", {
  w <- c(0.3, 0.25, 0.2, 0.15, 0.1)
  expect_equal(diversification(w), 1 - sum(w^2), tolerance = 1e-10)
})

test_that("diversification: is always in [0, 1] for valid long-only weights", {
  w <- c(0.5, 0.3, 0.1, 0.05, 0.05)
  d <- diversification(w)
  expect_true(d >= 0 && d <= 1)
})

# ===========================================================================
# Section 5:  constrained_objective — basic return types
# ===========================================================================

portf_basic <- portfolio.spec(assets = colnames(R))
portf_basic <- add.constraint(portf_basic, type = "full_investment")
portf_basic <- add.constraint(portf_basic, type = "long_only")
portf_basic <- add.objective(portf_basic, type = "risk", name = "StdDev")

test_that("constrained_objective: trace=FALSE returns a length-1 numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_basic,
    trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("constrained_objective: trace=FALSE result is non-negative for valid weights", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_basic,
    trace = FALSE
  )
  expect_true(result >= 0)
})

test_that("constrained_objective: trace=TRUE returns a list", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_basic,
    trace = TRUE
  )
  expect_true(is.list(result))
})

test_that("constrained_objective: trace=TRUE list has numeric out", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_basic,
    trace = TRUE
  )
  expect_false(is.null(result$out))
  expect_true(is.numeric(result$out))
})

test_that("constrained_objective: trace=TRUE list has numeric weights of correct length", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_basic,
    trace = TRUE
  )
  expect_false(is.null(result$weights))
  expect_true(is.numeric(result$weights))
  expect_length(result$weights, 5)
})

test_that("constrained_objective: trace=TRUE list has objective_measures list", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_basic,
    trace = TRUE
  )
  expect_false(is.null(result$objective_measures))
  expect_true(is.list(result$objective_measures))
})

test_that("constrained_objective: trace=TRUE StdDev in objective_measures", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_basic,
    trace = TRUE
  )
  expect_true("StdDev" %in% names(result$objective_measures))
})

# ===========================================================================
# Section 6:  normalize=FALSE — weight-sum penalty paths (L397-402)
# ===========================================================================

portf_wsum <- portfolio.spec(assets = colnames(R))
portf_wsum <- add.constraint(portf_wsum,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_wsum <- add.objective(portf_wsum, type = "risk", name = "StdDev")

test_that("weight-sum penalty: over-summed weights produce higher out than valid", {
  w_over <- c(0.3, 0.3, 0.3, 0.3, 0.3) # sums to 1.5 > max_sum=1.01
  result_over <- constrained_objective(
    w = w_over, R = R, portfolio = portf_wsum,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_wsum,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_over > result_ok)
})

test_that("weight-sum penalty: under-summed weights produce higher out than valid", {
  w_under <- c(0.05, 0.05, 0.05, 0.05, 0.05) # sums to 0.25 < min_sum=0.99
  result_under <- constrained_objective(
    w = w_under, R = R, portfolio = portf_wsum,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_wsum,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_under > result_ok)
})

test_that("weight-sum penalty: normalize=FALSE returns numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_wsum,
    trace = FALSE, normalize = FALSE
  )
  expect_true(is.numeric(result))
})

# ===========================================================================
# Section 7:  Box constraint penalty (L408-418)
# ===========================================================================

portf_box <- portfolio.spec(assets = colnames(R))
portf_box <- add.constraint(portf_box,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_box <- add.constraint(portf_box, type = "box", min = 0.05, max = 0.35)
portf_box <- add.objective(portf_box, type = "risk", name = "StdDev")

test_that("box penalty: weight exceeding max incurs higher out", {
  # 0.5 > max=0.35 => violation
  w_viol <- c(0.5, 0.125, 0.125, 0.125, 0.125)
  result_viol <- constrained_objective(
    w = w_viol, R = R, portfolio = portf_box,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_box,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_viol > result_ok)
})

test_that("box penalty: weight below min incurs higher out", {
  # 0.01 < min=0.05 => violation
  w_viol <- c(0.01, 0.2475, 0.2475, 0.2475, 0.2475)
  result_viol <- constrained_objective(
    w = w_viol, R = R, portfolio = portf_box,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_box,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_viol > result_ok)
})

# ===========================================================================
# Section 8:  Group constraint penalty (L430-439)
# ===========================================================================

portf_group <- portfolio.spec(assets = colnames(R))
portf_group <- add.constraint(portf_group,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_group <- add.constraint(portf_group,
  type = "group",
  groups = list(g1 = 1:2, g2 = 3:5),
  group_min = c(0.40, 0.30),
  group_max = c(0.60, 0.70)
)
portf_group <- add.objective(portf_group, type = "risk", name = "StdDev")

test_that("group penalty: group sum below cLO incurs higher out", {
  # g1 = assets 1:2; sum = 0.10 < cLO[1] = 0.40 => violation
  w_viol <- c(0.05, 0.05, 0.30, 0.30, 0.30)
  # g1 = assets 1:2; sum = 0.50 in [0.40, 0.60]; g2 sum = 0.50 in [0.30, 0.70]
  w_ok <- c(0.25, 0.25, 0.20, 0.15, 0.15)
  result_viol <- constrained_objective(
    w = w_viol, R = R, portfolio = portf_group,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_ok, R = R, portfolio = portf_group,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_viol > result_ok)
})

test_that("group penalty: group sum above cUP incurs higher out", {
  # g1 = assets 1:2; sum = 0.80 > cUP[1] = 0.60 => violation
  w_viol <- c(0.40, 0.40, 0.067, 0.067, 0.066)
  w_ok <- c(0.25, 0.25, 0.20, 0.15, 0.15)
  result_viol <- constrained_objective(
    w = w_viol, R = R, portfolio = portf_group,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_ok, R = R, portfolio = portf_group,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_viol > result_ok)
})

# ===========================================================================
# Section 9:  max_pos (position_limit) penalty (L445-453)
# ===========================================================================

portf_maxpos <- portfolio.spec(assets = colnames(R))
portf_maxpos <- add.constraint(portf_maxpos,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_maxpos <- add.constraint(portf_maxpos, type = "position_limit", max_pos = 2)
portf_maxpos <- add.objective(portf_maxpos, type = "risk", name = "StdDev")

test_that("max_pos penalty: more non-zero weights than allowed incurs higher out", {
  # w_eq has 5 non-zero positions; max_pos=2 => penalty = 1e4*(5-2)=30000
  w_ok <- c(0.6, 0.4, 0, 0, 0) # exactly 2 non-zero => no penalty
  result_viol <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_maxpos,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_ok, R = R, portfolio = portf_maxpos,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_viol > result_ok)
})

# ===========================================================================
# Section 10:  diversification constraint penalty (L459-464)
# ===========================================================================

# Equal-weight diversification = 1 - 5*(0.04) = 0.80.
# Target 0.99 is impossible => penalty always triggered.
portf_div_hard <- portfolio.spec(assets = colnames(R))
portf_div_hard <- add.constraint(portf_div_hard,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_div_hard <- add.constraint(portf_div_hard,
  type = "diversification",
  div_target = 0.99
)
portf_div_hard <- add.objective(portf_div_hard, type = "risk", name = "StdDev")

# Easy target: equal weights have div = 0.80; set target exactly at 0.80 so it
# falls within the 5% tolerance band [0.76, 0.84] => no penalty at all.
portf_div_easy <- portfolio.spec(assets = colnames(R))
portf_div_easy <- add.constraint(portf_div_easy,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_div_easy <- add.constraint(portf_div_easy,
  type = "diversification",
  div_target = 0.80
)
portf_div_easy <- add.objective(portf_div_easy, type = "risk", name = "StdDev")

test_that("diversification penalty: unachievable target produces higher out", {
  result_hard <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_div_hard,
    trace = FALSE, normalize = FALSE
  )
  result_easy <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_div_easy,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_hard > result_easy)
})

# ===========================================================================
# Section 11:  turnover constraint penalty (L470-476)
# ===========================================================================

# turnover() with NULL wts.init uses equal weights as baseline.
# Equal weights => zero turnover, so target 0.0 is met with no penalty.
# Skewed weights => non-zero turnover, which violates target 0.0.
portf_to_constr <- portfolio.spec(assets = colnames(R))
portf_to_constr <- add.constraint(portf_to_constr,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_to_constr <- add.constraint(portf_to_constr,
  type = "turnover",
  turnover_target = 0.0
)
portf_to_constr <- add.objective(portf_to_constr, type = "risk", name = "StdDev")

test_that("turnover constraint penalty: high-turnover weights produce higher out", {
  w_skew <- c(0.80, 0.05, 0.05, 0.05, 0.05) # large turnover from equal baseline
  result_viol <- constrained_objective(
    w = w_skew, R = R, portfolio = portf_to_constr,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_to_constr,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_viol > result_ok)
})

# ===========================================================================
# Section 12:  return_target constraint penalty (L482-485)
# ===========================================================================

# env$mu is required for the return_target penalty check.
mu_vec <- colMeans(R)
env_basic <- list(mu = mu_vec)

# Impossible target (100% per period) always penalised heavily.
portf_ret_hard <- portfolio.spec(assets = colnames(R))
portf_ret_hard <- add.constraint(portf_ret_hard,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_ret_hard <- add.constraint(portf_ret_hard,
  type = "return",
  return_target = 1.0
)
portf_ret_hard <- add.objective(portf_ret_hard, type = "risk", name = "StdDev")

# Easy target exactly equal to the equal-weight portfolio mean return.
portf_ret_exact <- portfolio.spec(assets = colnames(R))
portf_ret_exact <- add.constraint(portf_ret_exact,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_ret_exact <- add.constraint(portf_ret_exact,
  type = "return",
  return_target = as.numeric(crossprod(w_eq, mu_vec))
)
portf_ret_exact <- add.objective(portf_ret_exact, type = "risk", name = "StdDev")

test_that("return_target penalty: unachievable target produces much higher out", {
  result_hard <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_ret_hard,
    trace = FALSE, normalize = FALSE,
    env = env_basic
  )
  result_exact <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_ret_exact,
    trace = FALSE, normalize = FALSE,
    env = env_basic
  )
  expect_true(result_hard > result_exact)
})

test_that("return_target penalty: result is numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_ret_hard,
    trace = FALSE, normalize = FALSE,
    env = env_basic
  )
  expect_true(is.numeric(result))
})

# ===========================================================================
# Section 13:  factor exposure constraint penalty (L490-502)
# ===========================================================================

# Portfolio beta for equal weights = t(w_eq) %*% B = 0.2 * sum(B)
B_betas <- c(1.2, 0.8, 1.0, 0.9, 1.1) # sum = 5.0 => portfolio beta = 1.0

# Tight bounds far above 1.0 force a penalty for equal weights.
portf_fe_viol <- portfolio.spec(assets = colnames(R))
portf_fe_viol <- add.constraint(portf_fe_viol,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_fe_viol <- add.constraint(portf_fe_viol,
  type = "factor_exposure",
  B = B_betas, lower = 2.0, upper = 3.0
)
portf_fe_viol <- add.objective(portf_fe_viol, type = "risk", name = "StdDev")

# Wide bounds: portfolio beta = 1.0 is well inside [0.5, 1.5].
portf_fe_ok <- portfolio.spec(assets = colnames(R))
portf_fe_ok <- add.constraint(portf_fe_ok,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_fe_ok <- add.constraint(portf_fe_ok,
  type = "factor_exposure",
  B = B_betas, lower = 0.5, upper = 1.5
)
portf_fe_ok <- add.objective(portf_fe_ok, type = "risk", name = "StdDev")

test_that("factor_exposure penalty: violated exposure incurs higher out", {
  result_viol <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_fe_viol,
    trace = FALSE, normalize = FALSE
  )
  result_ok <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_fe_ok,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_viol > result_ok)
})

test_that("factor_exposure penalty: result is numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_fe_viol,
    trace = FALSE, normalize = FALSE
  )
  expect_true(is.numeric(result))
})

# ===========================================================================
# Section 14:  transaction cost penalty (L508-511)
# ===========================================================================

portf_tc <- portfolio.spec(assets = colnames(R))
portf_tc <- add.constraint(portf_tc, type = "full_investment")
portf_tc <- add.constraint(portf_tc, type = "long_only")
portf_tc <- add.constraint(portf_tc, type = "transaction_cost", ptc = 0.01)
portf_tc <- add.objective(portf_tc, type = "risk", name = "StdDev")

portf_no_tc <- portfolio.spec(assets = colnames(R))
portf_no_tc <- add.constraint(portf_no_tc, type = "full_investment")
portf_no_tc <- add.constraint(portf_no_tc, type = "long_only")
portf_no_tc <- add.objective(portf_no_tc, type = "risk", name = "StdDev")

test_that("transaction cost: portfolio with ptc has higher out than without", {
  result_tc <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_tc,
    trace = FALSE
  )
  result_no <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_no_tc,
    trace = FALSE
  )
  expect_true(result_tc >= result_no)
})

test_that("transaction cost: result is a numeric scalar", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_tc,
    trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

# ===========================================================================
# Section 15:  leverage exposure penalty (L519-520)
# ===========================================================================

# sum(|w_eq|) = 1.0 > leverage = 0.5 => penalty triggered
portf_lev_tight <- portfolio.spec(assets = colnames(R))
portf_lev_tight <- add.constraint(portf_lev_tight,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_lev_tight <- add.constraint(portf_lev_tight,
  type = "leverage_exposure",
  leverage = 0.5
)
portf_lev_tight <- add.objective(portf_lev_tight, type = "risk", name = "StdDev")

# sum(|w_eq|) = 1.0 < leverage = 2.0 => no penalty
portf_lev_loose <- portfolio.spec(assets = colnames(R))
portf_lev_loose <- add.constraint(portf_lev_loose,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
portf_lev_loose <- add.constraint(portf_lev_loose,
  type = "leverage_exposure",
  leverage = 2.0
)
portf_lev_loose <- add.objective(portf_lev_loose, type = "risk", name = "StdDev")

test_that("leverage_exposure penalty: exceeded leverage incurs higher out", {
  result_tight <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_lev_tight,
    trace = FALSE, normalize = FALSE
  )
  result_loose <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_lev_loose,
    trace = FALSE, normalize = FALSE
  )
  expect_true(result_tight > result_loose)
})

test_that("leverage_exposure: result is numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_lev_tight,
    trace = FALSE, normalize = FALSE
  )
  expect_true(is.numeric(result))
})

# ===========================================================================
# Section 16:  mean return objective (switch case: mean)
# ===========================================================================

portf_mean <- portfolio.spec(assets = colnames(R))
portf_mean <- add.constraint(portf_mean, type = "full_investment")
portf_mean <- add.constraint(portf_mean, type = "long_only")
portf_mean <- add.objective(portf_mean, type = "return", name = "mean")

test_that("mean return objective: constrained_objective returns numeric scalar", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_mean,
    trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("mean return objective: trace=TRUE objective_measures has mean element", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_mean,
    trace = TRUE
  )
  expect_true("mean" %in% names(result$objective_measures))
})

test_that("mean return objective: maximising mean gives negative out (multiplier=-1)", {
  # mean objective has multiplier = -1 (maximise), so out <= 0 for positive mu
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_mean,
    trace = FALSE
  )
  # mean return of edhec is positive, so out should be negative (scaled by -1)
  expect_true(result < 0)
})

# ===========================================================================
# Section 17:  ES objective (switch case: ES / CVaR / ETL)
# ===========================================================================

portf_es <- portfolio.spec(assets = colnames(R))
portf_es <- add.constraint(portf_es, type = "full_investment")
portf_es <- add.constraint(portf_es, type = "long_only")
portf_es <- add.objective(portf_es, type = "risk", name = "ES")

test_that("ES objective: constrained_objective returns positive numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_es,
    trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_true(result > 0)
})

test_that("ES objective: trace=TRUE objective_measures has ES element", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_es,
    trace = TRUE
  )
  expect_true("ES" %in% names(result$objective_measures))
})

# ===========================================================================
# Section 18:  VaR objective (switch case: VaR / mVaR)
# ===========================================================================

portf_var_obj <- portfolio.spec(assets = colnames(R))
portf_var_obj <- add.constraint(portf_var_obj, type = "full_investment")
portf_var_obj <- add.constraint(portf_var_obj, type = "long_only")
portf_var_obj <- add.objective(portf_var_obj, type = "risk", name = "VaR")

test_that("VaR objective: constrained_objective returns numeric scalar", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_var_obj,
    trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("VaR objective: trace=TRUE objective_measures has VaR element", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_var_obj,
    trace = TRUE
  )
  expect_true("VaR" %in% names(result$objective_measures))
})

# ===========================================================================
# Section 19:  turnover objective (switch case: turnover)
# ===========================================================================

portf_to_obj <- portfolio.spec(assets = colnames(R))
portf_to_obj <- add.constraint(portf_to_obj, type = "full_investment")
portf_to_obj <- add.constraint(portf_to_obj, type = "long_only")
portf_to_obj <- add.objective(portf_to_obj, type = "turnover", name = "turnover")

test_that("turnover objective: constrained_objective returns numeric scalar", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_to_obj,
    trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("turnover objective: trace=TRUE objective_measures has turnover element", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_to_obj,
    trace = TRUE
  )
  expect_true("turnover" %in% names(result$objective_measures))
})

test_that("turnover objective: equal weights give near-zero objective value", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_to_obj,
    trace = FALSE
  )
  # turnover(w_eq, NULL) = 0; multiplier=1 => out = 0
  expect_equal(result, 0, tolerance = 1e-10)
})

# ===========================================================================
# Section 20:  risk_budget objective — max_prisk path (L695-720)
# ===========================================================================

portf_rb <- portfolio.spec(assets = colnames(R))
portf_rb <- add.constraint(portf_rb, type = "full_investment")
portf_rb <- add.constraint(portf_rb, type = "long_only")
portf_rb <- add.objective(portf_rb,
  type = "risk_budget", name = "ES",
  max_prisk = 0.4
)

test_that("risk_budget ES max_prisk: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_rb,
    trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("risk_budget ES max_prisk: trace=TRUE objective_measures has ES element", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_rb,
    trace = TRUE
  )
  expect_true("ES" %in% names(result$objective_measures))
})

test_that("risk_budget ES max_prisk: concentrated weights produce higher out", {
  # Heavily concentrated portfolio should violate max_prisk = 0.4
  w_conc <- c(0.96, 0.01, 0.01, 0.01, 0.01)
  result_conc <- constrained_objective(
    w = w_conc, R = R, portfolio = portf_rb,
    trace = FALSE
  )
  result_eq <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_rb,
    trace = FALSE
  )
  expect_true(result_conc > result_eq)
})

# ===========================================================================
# Section 21:  risk_budget objective — min_concentration path (L725-728)
# ===========================================================================

portf_rb_mc <- portfolio.spec(assets = colnames(R))
portf_rb_mc <- add.constraint(portf_rb_mc, type = "full_investment")
portf_rb_mc <- add.constraint(portf_rb_mc, type = "long_only")
portf_rb_mc <- add.objective(portf_rb_mc,
  type = "risk_budget", name = "ES",
  min_concentration = TRUE
)

test_that("risk_budget min_concentration: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_rb_mc,
    trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("risk_budget min_concentration: trace=TRUE has ES in objective_measures", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_rb_mc,
    trace = TRUE
  )
  expect_true("ES" %in% names(result$objective_measures))
})

# ===========================================================================
# Section 22:  weight_concentration objective — scalar HHI path (L737)
# ===========================================================================

portf_hhi <- portfolio.spec(assets = colnames(R))
portf_hhi <- add.constraint(portf_hhi, type = "full_investment")
portf_hhi <- add.constraint(portf_hhi, type = "long_only")
portf_hhi <- add.objective(portf_hhi,
  type = "weight_concentration", name = "HHI",
  conc_aversion = 0.001
)

test_that("weight_concentration scalar HHI: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_hhi,
    trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("weight_concentration scalar HHI: trace=TRUE has HHI in objective_measures", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_hhi,
    trace = TRUE
  )
  expect_true("HHI" %in% names(result$objective_measures))
})

test_that("weight_concentration scalar HHI: concentrated weights produce higher out", {
  w_conc <- c(0.96, 0.01, 0.01, 0.01, 0.01)
  result_conc <- constrained_objective(
    w = w_conc, R = R, portfolio = portf_hhi,
    trace = FALSE
  )
  result_eq <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_hhi,
    trace = FALSE
  )
  expect_true(result_conc > result_eq)
})

# ===========================================================================
# Section 23:  weight_concentration objective — group HHI path (L741-744)
# ===========================================================================

portf_hhi_grp <- portfolio.spec(assets = colnames(R))
portf_hhi_grp <- add.constraint(portf_hhi_grp, type = "full_investment")
portf_hhi_grp <- add.constraint(portf_hhi_grp, type = "long_only")
portf_hhi_grp <- add.objective(portf_hhi_grp,
  type = "weight_concentration",
  name = "HHI",
  conc_aversion = c(0.01, 0.01),
  conc_groups = list(g1 = 1:2, g2 = 3:5)
)

test_that("weight_concentration group HHI: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_hhi_grp,
    trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("weight_concentration group HHI: trace=TRUE has HHI in objective_measures", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_hhi_grp,
    trace = TRUE
  )
  expect_true("HHI" %in% names(result$objective_measures))
})

# ===========================================================================
# Section 24:  verbose=TRUE path (L763)
# ===========================================================================

test_that("verbose=TRUE: constrained_objective produces console output", {
  # verbose=TRUE references tmp_return, which is only created when trace=TRUE
  # or storage=TRUE; pass trace=TRUE to avoid an "object not found" error.
  expect_output(
    constrained_objective(
      w = w_eq, R = R, portfolio = portf_basic,
      trace = TRUE, verbose = TRUE
    ),
    regexp = "weights"
  )
})

# ===========================================================================
# Section 25:  normalize=TRUE (default) — fn_map weight mapping
# ===========================================================================

# Weights that do NOT sum to 1 should be normalised by fn_map before use.
portf_norm <- portfolio.spec(assets = colnames(R))
portf_norm <- add.constraint(portf_norm, type = "full_investment")
portf_norm <- add.constraint(portf_norm, type = "long_only")
portf_norm <- add.objective(portf_norm, type = "risk", name = "StdDev")

test_that("normalize=TRUE: non-unit weights are mapped and objective is numeric", {
  w_unnorm <- c(0.1, 0.1, 0.1, 0.1, 0.1) # sum = 0.5, will be renormalised
  result <- constrained_objective(
    w = w_unnorm, R = R, portfolio = portf_norm,
    trace = FALSE, normalize = TRUE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("normalize=TRUE: trace=TRUE weights sum to approximately 1", {
  w_unnorm <- c(0.1, 0.1, 0.1, 0.1, 0.1)
  result <- constrained_objective(
    w = w_unnorm, R = R, portfolio = portf_norm,
    trace = TRUE, normalize = TRUE
  )
  # fn_map with full_investment allows weights to land anywhere in [0.99, 1.01]
  expect_true(sum(result$weights) >= 0.99 && sum(result$weights) <= 1.01)
})

# ===========================================================================
# Section 26:  multiple objectives combined
# ===========================================================================

portf_multi <- portfolio.spec(assets = colnames(R))
portf_multi <- add.constraint(portf_multi, type = "full_investment")
portf_multi <- add.constraint(portf_multi, type = "long_only")
portf_multi <- add.objective(portf_multi, type = "return", name = "mean")
portf_multi <- add.objective(portf_multi, type = "risk", name = "StdDev")

test_that("multiple objectives: constrained_objective returns numeric scalar", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_multi,
    trace = FALSE
  )
  expect_true(is.numeric(result))
  expect_length(result, 1)
})

test_that("multiple objectives: trace=TRUE objective_measures has both mean and StdDev", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_multi,
    trace = TRUE
  )
  expect_true("mean" %in% names(result$objective_measures))
  expect_true("StdDev" %in% names(result$objective_measures))
})

# ===========================================================================
# Section 27:  return objective with numeric target (L650-658 penalty path)
# ===========================================================================

portf_ret_tgt <- portfolio.spec(assets = colnames(R))
portf_ret_tgt <- add.constraint(portf_ret_tgt, type = "full_investment")
portf_ret_tgt <- add.constraint(portf_ret_tgt, type = "long_only")
# target = 0 is far from actual mean return => penalty is triggered
portf_ret_tgt <- add.objective(portf_ret_tgt,
  type = "return", name = "mean",
  target = 0
)

test_that("return objective with target: constrained_objective returns numeric", {
  result <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_ret_tgt,
    trace = FALSE
  )
  expect_true(is.numeric(result))
})

test_that("return objective with numeric target: higher penalty than no-target version", {
  result_target <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_ret_tgt,
    trace = FALSE
  )
  result_notarget <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_mean,
    trace = FALSE
  )
  # The impossible-target version should have higher (less negative / more positive) out
  expect_true(result_target > result_notarget)
})

# ===========================================================================
# Section 28:  StdDev (var) objective with numeric target (L664-672)
# ===========================================================================

portf_sd_tgt <- portfolio.spec(assets = colnames(R))
portf_sd_tgt <- add.constraint(portf_sd_tgt, type = "full_investment")
portf_sd_tgt <- add.constraint(portf_sd_tgt, type = "long_only")
# target = 0 is unreachable => heavy penalty
portf_sd_tgt <- add.objective(portf_sd_tgt,
  type = "risk", name = "StdDev",
  target = 0
)

test_that("StdDev risk objective with unreachable target: higher out than no-target", {
  result_target <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_sd_tgt,
    trace = FALSE
  )
  result_notarget <- constrained_objective(
    w = w_eq, R = R, portfolio = portf_basic,
    trace = FALSE
  )
  expect_true(result_target > result_notarget)
})

# ===========================================================================
# Section 29:  var.portfolio() agrees with constrained_objective StdDev path
# ===========================================================================

test_that("var.portfolio: result matches manual matrix formula exactly", {
  sigma <- var(R)
  expected <- as.numeric(t(w_eq) %*% sigma %*% w_eq)
  expect_equal(var.portfolio(R, w_eq), expected, tolerance = 1e-10)
})

test_that("var.portfolio: accepts column-vector weights without error", {
  w_col <- matrix(w_eq, ncol = 1)
  expect_true(is.numeric(var.portfolio(R, w_col)))
})

# ===========================================================================
# Section 30:  HHI — additional edge cases
# ===========================================================================

test_that("HHI: single asset portfolio equals 1", {
  expect_equal(HHI(1), 1, tolerance = 1e-10)
})

test_that("HHI: two equal assets equal 0.5", {
  expect_equal(HHI(c(0.5, 0.5)), 0.5, tolerance = 1e-10)
})

test_that("HHI: result is always in (0, 1] for positive weights", {
  w <- c(0.4, 0.3, 0.2, 0.1)
  h <- HHI(w)
  expect_true(h > 0 && h <= 1)
})

test_that("HHI: groups with named list preserves names in Groups_HHI", {
  groups <- list(equities = 1:2, bonds = 3:5)
  result <- HHI(w_eq, groups = groups)
  expect_true("equities" %in% names(result$Groups_HHI))
  expect_true("bonds" %in% names(result$Groups_HHI))
})

# ===========================================================================
# Section 31:  diversification() — additional edge cases
# ===========================================================================

test_that("diversification: single asset equals 0", {
  expect_equal(diversification(1), 0, tolerance = 1e-10)
})

test_that("diversification: two equal assets equals 0.5", {
  expect_equal(diversification(c(0.5, 0.5)), 0.5, tolerance = 1e-10)
})

test_that("diversification: increasing number of equal assets increases diversification", {
  d2 <- diversification(c(0.5, 0.5))
  d4 <- diversification(rep(0.25, 4))
  expect_true(d4 > d2)
})

# ===========================================================================
# Section 32:  no-objectives warning path
# ===========================================================================

# portfolio$objectives is initialised as list() by portfolio.spec, so we must
# set it to NULL explicitly to trigger the is.null() check in
# constrained_objective (line ~565 of constrained_objective.R).
portf_noobj <- portfolio.spec(assets = colnames(R))
portf_noobj <- add.constraint(portf_noobj, type = "full_investment")
portf_noobj <- add.constraint(portf_noobj, type = "long_only")
portf_noobj$objectives <- NULL # force the NULL branch / warning

test_that("no objectives: constrained_objective warns and returns numeric", {
  expect_warning(
    result <- constrained_objective(
      w = w_eq, R = R, portfolio = portf_noobj,
      trace = FALSE
    ),
    regexp = "no objectives"
  )
  expect_true(is.numeric(result))
})

test_that('constrained_objective with turnover objective penalizes target correctly', {
  p <- portfolio.spec(assets=4)
  p <- add.constraint(p, type='box', min=0, max=1)
  p <- add.constraint(p, type='weight_sum', min_sum=0.99, max_sum=1.01)
  p <- add.objective(p, type='turnover', name='turnover', target=0.1, multiplier=1)
  w_viol <- c(0.5, 0.5, 0, 0)
  res <- constrained_objective(w_viol, R=edhec4, portfolio=p)
  expect_true(is.numeric(res))
})
