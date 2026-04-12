# Tests for fn_map() and .project_to_fe_feasible() factor exposure support
# Issue #52: fn_map() and rp_transform() do not enforce factor_exposure constraints

library(testthat)
library(PortfolioAnalytics)

# ---------- helper: factor_exposure_fail ----------

test_that("factor_exposure_fail returns FALSE when B is NULL", {
  skip_on_cran()
  expect_false(PortfolioAnalytics:::factor_exposure_fail(
    weights = c(0.5, 0.5),
    B = NULL, lower = NULL, upper = NULL
  ))
})

test_that("factor_exposure_fail detects violations and passes feasible weights", {
  skip_on_cran()
  B <- matrix(c(1.2, 0.8, 0.5,
                0.3, 0.7, 1.1), nrow = 3, ncol = 2)
  lower <- c(0.6, 0.5)
  upper <- c(1.0, 0.9)
  
  # feasible weights
  w_ok <- c(0.4, 0.3, 0.3)
  # exposures: B'w = (1.2*0.4 + 0.8*0.3 + 0.5*0.3, 0.3*0.4 + 0.7*0.3 + 1.1*0.3)
  #          = (0.48 + 0.24 + 0.15, 0.12 + 0.21 + 0.33) = (0.87, 0.66)
  # both within [0.6, 1.0] and [0.5, 0.9] => feasible
  expect_false(PortfolioAnalytics:::factor_exposure_fail(w_ok, B, lower, upper))
  
  # infeasible weights — push exposure 1 above upper
  w_bad <- c(0.6, 0.3, 0.1)
  # exposures: (1.2*0.6 + 0.8*0.3 + 0.5*0.1, 0.3*0.6 + 0.7*0.3 + 1.1*0.1)
  #          = (0.72 + 0.24 + 0.05, 0.18 + 0.21 + 0.11) = (1.01, 0.50)
  # exposure 1 = 1.01 > 1.0 => violated
  expect_true(PortfolioAnalytics:::factor_exposure_fail(w_bad, B, lower, upper))
})

# ---------- helper: .project_to_fe_feasible ----------

test_that(".project_to_fe_feasible finds nearest feasible point", {
  skip_on_cran()
  N <- 3
  B <- matrix(c(1.0, 0.0, 0.0,
                0.0, 1.0, 0.0), nrow = 3, ncol = 2)
  lower <- c(0.20, 0.20)
  upper <- c(0.50, 0.50)
  min_box <- rep(0, N)
  max_box <- rep(1, N)
  
  # Candidate violates factor exposure: w1 = 0.6 > 0.5
  w_cand <- c(0.6, 0.3, 0.1)
  
  w_proj <- PortfolioAnalytics:::.project_to_fe_feasible(
    w_cand, min_sum = 0.99, max_sum = 1.01,
    min_box = min_box, max_box = max_box,
    B = B, lower = lower, upper = upper
  )
  
  # projected weights should satisfy all constraints
  expect_true(sum(w_proj) >= 0.99 - 1e-8)
  expect_true(sum(w_proj) <= 1.01 + 1e-8)
  expect_true(all(w_proj >= min_box - 1e-8))
  expect_true(all(w_proj <= max_box + 1e-8))
  
  exposures <- as.numeric(crossprod(B, w_proj))
  expect_true(all(exposures >= lower - 1e-6))
  expect_true(all(exposures <= upper + 1e-6))
  
  # The projection should be close to the candidate (minimal displacement)
  expect_lt(sum((w_proj - w_cand)^2), 0.1)
})

test_that(".project_to_fe_feasible returns original weights when QP is infeasible", {
  skip_on_cran()
  N <- 3
  B <- matrix(c(1.0, 1.0, 1.0), nrow = 3, ncol = 1)
  # Impossible: exposure = sum(w), but sum(w) in [0.99, 1.01] vs lower_fe = 2.0
  lower <- 2.0
  upper <- 3.0
  
  w_cand <- c(0.4, 0.3, 0.3)
  w_proj <- PortfolioAnalytics:::.project_to_fe_feasible(
    w_cand, min_sum = 0.99, max_sum = 1.01,
    min_box = rep(0, N), max_box = rep(1, N),
    B = B, lower = lower, upper = upper
  )
  
  # Should return original weights when QP is infeasible
  expect_equal(w_proj, w_cand)
})

# ---------- fn_map integration ----------

test_that("fn_map enforces factor exposure constraints via QP projection", {
  skip_on_cran()
  data(edhec)
  R <- edhec[, 1:4]
  
  # Factor loading matrix: 4 assets, 1 factor
  B <- matrix(c(1.2, 0.8, 0.5, 1.0), nrow = 4, ncol = 1)
  
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.65)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.7, upper = 0.9)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  
  # Create a weight vector that violates factor exposure
  # w = c(0.55, 0.15, 0.15, 0.15) => B'w = 1.2*0.55 + 0.8*0.15 + 0.5*0.15 + 1.0*0.15
  #                                       = 0.66 + 0.12 + 0.075 + 0.15 = 1.005
  # exposure = 1.005 > 0.9 => violated
  w_violating <- c(0.55, 0.15, 0.15, 0.15)
  names(w_violating) <- colnames(R)
  
  result <- fn_map(weights = w_violating, portfolio = portf)
  w_repaired <- result$weights
  
  # Check factor exposure is now satisfied
  exposure <- as.numeric(crossprod(B, w_repaired))
  expect_true(exposure >= 0.7 - 1e-6, 
              label = paste("exposure", round(exposure, 4), ">= 0.7"))
  expect_true(exposure <= 0.9 + 1e-6, 
              label = paste("exposure", round(exposure, 4), "<= 0.9"))
  
  # Check other constraints still satisfied
  expect_true(sum(w_repaired) >= 0.99 - 1e-6)
  expect_true(sum(w_repaired) <= 1.01 + 1e-6)
  expect_true(all(w_repaired >= 0.05 - 1e-6))
  expect_true(all(w_repaired <= 0.65 + 1e-6))
})

test_that("fn_map passes through weights that already satisfy factor exposure", {
  skip_on_cran()
  data(edhec)
  R <- edhec[, 1:4]
  
  B <- matrix(c(0.5, 0.5, 0.5, 0.5), nrow = 4, ncol = 1)
  
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.1, max = 0.5)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.4, upper = 0.6)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  
  # w = c(0.25, 0.25, 0.25, 0.25) => B'w = 0.5 * 1.0 = 0.5 in [0.4, 0.6]
  w_ok <- c(0.25, 0.25, 0.25, 0.25)
  names(w_ok) <- colnames(R)
  
  result <- fn_map(weights = w_ok, portfolio = portf)
  
  # Weights should pass through unchanged (or very close)
  expect_equal(result$weights, w_ok, tolerance = 1e-6)
})

# ---------- DEoptim end-to-end with factor exposure ----------

test_that("DEoptim with factor exposure constraints finds feasible solution", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:36, 1:4]  # 3 years, 4 assets for speed
  
  # Factor loading matrix: single market beta factor
  B <- matrix(c(0.8, 1.1, 0.6, 0.9), nrow = 4, ncol = 1)
  
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.60)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.75, upper = 0.95)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  
  # Use small population / iterations for test speed
  result <- optimize.portfolio(R, portf, optimize_method = "DEoptim",
                               search_size = 500, NP = 20, itermax = 30,
                               trace = FALSE)
  
  w <- result$weights
  exposure <- as.numeric(crossprod(B, w))
  
  # Factor exposure should be within bounds (or very close — penalty + fn_map together)
  expect_true(exposure >= 0.75 - 0.02,
              label = paste("DEoptim exposure", round(exposure, 4), ">= 0.73"))
  expect_true(exposure <= 0.95 + 0.02,
              label = paste("DEoptim exposure", round(exposure, 4), "<= 0.97"))
  
  # Basic constraint satisfaction
  expect_true(abs(sum(w) - 1.0) < 0.02)
  expect_true(all(w >= 0.05 - 0.01))
})

# ---------- GenSA end-to-end with factor exposure ----------

test_that("GenSA with factor exposure constraints finds feasible solution", {
  skip_on_cran()
  data(edhec)
  R <- edhec[1:36, 1:4]
  
  B <- matrix(c(0.8, 1.1, 0.6, 0.9), nrow = 4, ncol = 1)
  
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.60)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = 0.75, upper = 0.95)
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  
  result <- optimize.portfolio(R, portf, optimize_method = "GenSA",
                               maxit = 50, trace = FALSE)
  
  w <- result$weights
  exposure <- as.numeric(crossprod(B, w))
  
  expect_true(exposure >= 0.75 - 0.02,
              label = paste("GenSA exposure", round(exposure, 4), ">= 0.73"))
  expect_true(exposure <= 0.95 + 0.02,
              label = paste("GenSA exposure", round(exposure, 4), "<= 0.97"))
  expect_true(abs(sum(w) - 1.0) < 0.02)
})

# ---------- Multi-factor test ----------

test_that("fn_map handles multi-factor exposure constraints", {
  skip_on_cran()
  data(edhec)
  R <- edhec[, 1:5]
  
  # 5 assets, 2 factors
  B <- matrix(c(1.2, 0.8, 0.5, 1.0, 0.7,   # factor 1 loadings
                0.3, 0.9, 1.1, 0.4, 0.6),   # factor 2 loadings
              nrow = 5, ncol = 2)
  
  portf <- portfolio.spec(assets = colnames(R))
  portf <- add.constraint(portf, type = "full_investment")
  portf <- add.constraint(portf, type = "box", min = 0.05, max = 0.50)
  portf <- add.constraint(portf, type = "factor_exposure",
                          B = B, lower = c(0.7, 0.5), upper = c(0.9, 0.8))
  portf <- add.objective(portf, type = "risk", name = "StdDev")
  
  # Weights that violate both factors
  w_bad <- c(0.45, 0.10, 0.10, 0.25, 0.10)
  names(w_bad) <- colnames(R)
  # factor 1: 1.2*0.45 + 0.8*0.10 + 0.5*0.10 + 1.0*0.25 + 0.7*0.10 
  #         = 0.54 + 0.08 + 0.05 + 0.25 + 0.07 = 0.99 > 0.9
  
  result <- fn_map(weights = w_bad, portfolio = portf)
  w_repaired <- result$weights
  
  exposures <- as.numeric(crossprod(B, w_repaired))
  expect_true(all(exposures >= c(0.7, 0.5) - 1e-6),
              label = paste("multi-factor lower:", paste(round(exposures, 4), collapse=", ")))
  expect_true(all(exposures <= c(0.9, 0.8) + 1e-6),
              label = paste("multi-factor upper:", paste(round(exposures, 4), collapse=", ")))
  
  # Other constraints
  expect_true(abs(sum(w_repaired) - 1.0) < 0.02)
  expect_true(all(w_repaired >= 0.05 - 1e-6))
  expect_true(all(w_repaired <= 0.50 + 1e-6))
})
