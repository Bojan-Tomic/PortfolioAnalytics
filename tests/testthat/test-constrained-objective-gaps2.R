###############################################################################
# tests/testthat/test-constrained-objective-gaps2.R
#
# Targeted coverage for uncovered branches in R/constrained_objective.R
#
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)
library(PerformanceAnalytics)

utils::data(edhec)
edhec4 <- edhec[, 1:4]
funds4 <- colnames(edhec4)

# Create a valid v1 constraint object
cc_v1 <- constraint_v1(assets = funds4, min_sum = 0.99, max_sum = 1.01,
                       min = rep(0, 4), max = rep(1, 4))
w <- rep(1/4, 4)

test_that("constrained_objective_v1 verbose argument is parsed", {
  skip_on_cran()
  
  # Trigger verbose=TRUE lines 32 and 268-271
  cc <- add.objective_v1(cc_v1, type = "return", name = "mean")
  expect_output(
    res <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc, verbose = TRUE),
    "output of objective function"
  )
  expect_true(is.numeric(res))
})

test_that("constrained_objective_v1 warns when no objectives are specified", {
  skip_on_cran()
  
  # Remove any objectives if they exist
  cc_no_obj <- cc_v1
  cc_no_obj$objectives <- NULL
  
  # Trigger warning on line 106
  expect_warning(
    res <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc_no_obj),
    "no objectives specified in constraints"
  )
  expect_true(is.numeric(res))
})

test_that("constrained_objective_v1 VaR and ES with arguments", {
  skip_on_cran()
  
  # Trigger invert branch in VaR and ES
  cc <- add.objective_v1(cc_v1, type = "risk", name = "VaR", arguments=list(invert=FALSE))
  cc <- add.objective_v1(cc, type = "risk", name = "ES", arguments=list(invert=FALSE))
  
  res <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc)
  expect_true(is.numeric(res))
})

test_that("constrained_objective_v1 objective target", {
  skip_on_cran()
  
  # Trigger return objective target (lines 188)
  cc <- add.objective_v1(cc_v1, type = "return", name = "mean", target = 0.05)
  res <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc)
  expect_true(is.numeric(res))
})

test_that("constrained_objective_v1 turnover objective", {
  skip_on_cran()
  
  # Trigger turnover objective with target (lines 206-207)
  cc <- add.objective_v1(cc_v1, type = "turnover", name = "turnover", target = 0.5)
  res <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc)
  expect_true(is.numeric(res))
})

test_that("constrained_objective_v1 minmax objective", {
  skip_on_cran()
  
  # Trigger minmax_objective (lines 214-219)
  # minmax is a type that checks tmp_measure > max or tmp_measure < min
  cc <- add.objective_v1(cc_v1, type = "minmax", name = "mean", min = 0.01, max = 0.02)
  res <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc)
  expect_true(is.numeric(res))
  
  cc_low <- add.objective_v1(cc_v1, type = "minmax", name = "mean", min = 0.1, max = 0.2)
  res2 <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc_low)
  expect_true(is.numeric(res2))
})

test_that("constrained_objective_v1 risk_budget objective target and min_difference", {
  skip_on_cran()
  
  # Trigger risk_budget target (line 233) and min_difference (lines 252-258)
  cc <- add.objective_v1(cc_v1, type = "risk_budget", name = "ES", target = 0.05, min_difference = TRUE)
  res <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc)
  expect_true(is.numeric(res))
})

test_that("constrained_objective_v1 handles objective error/warning", {
  skip_on_cran()
  
  # Trigger inherits try-error (line 182)
  bad_fun <- function(...) stop("intentional error")
  cc <- add.objective_v1(cc_v1, type = "return", name = "bad_fun")
  
  # We have to mock match.fun because it will fail before evaluating the function
  # Actually, if match.fun succeeds but evaluation fails...
  # Let's just pass a non-existent name to cause match.fun to fail
  cc <- add.objective_v1(cc_v1, type = "return", name = "nonexistent_obj_fun_xyz")
  expect_message(
    try(PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc), silent = TRUE),
    "generated an error or warning"
  )
})

test_that("constrained_objective_v1 NA or NaN produced", {
  skip_on_cran()
  
  # Trigger is.na(out) warning (line 276)
  # A custom objective returning NA
  na_fun <- function(...) NA
  cc <- add.objective_v1(cc_v1, type = "return", name = "na_fun")
  
  expect_warning(
    res <- PortfolioAnalytics:::constrained_objective_v1(w = w, R = edhec4, constraints = cc),
    "NA or NaN produced in objective function"
  )
  expect_true(res == 1e4) # Returns penalty
})

test_that("constrained_objective_v2 median objective", {
  skip_on_cran()
  
  # Trigger v2 median branch (line 581)
  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum=0.99, max_sum=1.01)
  p <- add.objective(p, type = "return", name = "median")
  
  expect_warning(
    res <- PortfolioAnalytics:::constrained_objective_v2(w = w, R = edhec4, portfolio = p),
    "NA or NaN produced"
  )
  expect_true(is.numeric(res))
})

test_that("constrained_objective_v2 VaR and ES portfolio_method argument", {
  skip_on_cran()
  
  # Trigger VaR and ES portfolio_method='single' (lines 592, 603)
  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum=0.99, max_sum=1.01)
  p <- add.objective(p, type = "risk", name = "VaR", arguments=list(invert=FALSE))
  p <- add.objective(p, type = "risk", name = "ES", arguments=list(invert=FALSE))
  
  # The missing moments might cause ES to fail so let's just expect numeric/list
  res <- try(PortfolioAnalytics:::constrained_objective_v2(w = w, R = edhec4, portfolio = p), silent=TRUE)
  expect_true(!inherits(res, "try-error"))
})
