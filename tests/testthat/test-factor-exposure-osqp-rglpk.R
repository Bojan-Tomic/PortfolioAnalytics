# Tests for osqp and Rglpk factor exposure constraint support (#50)
# Verifies that factor_exposure constraints are correctly enforced
# across all solver/sub-case combinations.

library(PortfolioAnalytics)

# ── Shared data setup ──────────────────────────────────────────────────────

data(edhec)
R <- edhec["2008::2012", 1:6]
funds <- colnames(R)
N <- length(funds)
T_obs <- nrow(R)

# Factor loading matrix: 6 assets × 2 factors
B <- matrix(c(
  1.2, 0.8, 0.3, 0.5, 0.1, 0.9,
  0.2, 0.5, 0.9, 0.7, 0.3, 0.4
), ncol = 2, byrow = FALSE)
rownames(B) <- funds
colnames(B) <- c("Equity.Beta", "Credit.Spread")

fe.lower <- c(0.4, 0.3)
fe.upper <- c(0.8, 0.6)

# Helper: check factor exposure bounds
check_fe <- function(w, B, lower, upper, tol = 1e-4) {
  exposures <- as.numeric(t(B) %*% w)
  for (k in seq_along(exposures)) {
    expect_true(exposures[k] >= lower[k] - tol,
                label = paste0("FE lower bound factor ", k))
    expect_true(exposures[k] <= upper[k] + tol,
                label = paste0("FE upper bound factor ", k))
  }
}

# ── osqp: min-variance + factor exposure ───────────────────────────────────

test_that("osqp min-variance respects factor exposure constraints", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R, port, optimize_method = "osqp")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-4))
  check_fe(w, B, fe.lower, fe.upper)
})

# ── osqp: maxSR + factor exposure (Charnes-Cooper) ────────────────────────

test_that("osqp maxSR respects factor exposure constraints", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "return", name = "mean")
  port <- add.objective(port, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R, port, optimize_method = "osqp", maxSR = TRUE)
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-4))
  check_fe(w, B, fe.lower, fe.upper)
})

# ── osqp vs CVXR cross-validation ─────────────────────────────────────────

test_that("osqp min-variance + FE matches CVXR within tolerance", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "risk", name = "StdDev")

  result_osqp <- optimize.portfolio(R, port, optimize_method = "osqp")
  result_cvxr <- optimize.portfolio(R, port, optimize_method = "CVXR")

  # Weights should be close (both solve the same convex problem)
  expect_equal(as.numeric(result_osqp$weights),
               as.numeric(result_cvxr$weights),
               tolerance = 1e-3)
})

# ── Rglpk A1: maxReturn + factor exposure (no positions) ──────────────────

test_that("Rglpk maxReturn respects factor exposure constraints", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "return", name = "mean")

  result <- optimize.portfolio(R, port, optimize_method = "Rglpk")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-4))
  check_fe(w, B, fe.lower, fe.upper)
})

# ── Rglpk A2: minES + factor exposure (no positions) ──────────────────────

test_that("Rglpk minES respects factor exposure constraints", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "risk", name = "ES",
                        arguments = list(p = 0.95))

  result <- optimize.portfolio(R, port, optimize_method = "Rglpk")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-4))
  check_fe(w, B, fe.lower, fe.upper)
})

# ── Rglpk A3: maxSTARR + factor exposure (no positions) ───────────────────

test_that("Rglpk maxSTARR respects factor exposure constraints", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "return", name = "mean")
  port <- add.objective(port, type = "risk", name = "ES",
                        arguments = list(p = 0.95))

  result <- optimize.portfolio(R, port, optimize_method = "Rglpk")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-4))
  check_fe(w, B, fe.lower, fe.upper)
})

# ── Rglpk B1: maxReturn + factor exposure + position limit ────────────────

test_that("Rglpk maxReturn + positions respects factor exposure constraints", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "position_limit", max_pos = 5)
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "return", name = "mean")

  result <- optimize.portfolio(R, port, optimize_method = "Rglpk")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-4))
  expect_true(sum(w > 1e-4) <= 5, label = "position limit")
  check_fe(w, B, fe.lower, fe.upper)
})

# ── Rglpk B2: minES + factor exposure + position limit ────────────────────

test_that("Rglpk minES + positions respects factor exposure constraints", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "position_limit", max_pos = 5)
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "risk", name = "ES",
                        arguments = list(p = 0.95))

  result <- optimize.portfolio(R, port, optimize_method = "Rglpk")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-4))
  expect_true(sum(w > 1e-4) <= 5, label = "position limit")
  check_fe(w, B, fe.lower, fe.upper)
})
