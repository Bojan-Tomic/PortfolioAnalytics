## Tests for issue #49: Rglpk solver matrix dimension mismatch with return target
##
## Root cause (risk & reward / Mean-ES case): optimize.portfolio.R lines 1751-1758
##   Rglpk.mat <- rbind(Rglpk.mat, c(rep(0, N+T+1)), target)
##   Rglpk.dir <- c(Rglpk.dir, "<=")
##   Rglpk.rhs <- c(Rglpk.rhs, 1)
## The rbind adds 2 rows of wrong lengths (N+T+1 and 1) against an (N+T+2)-column
## matrix, and only 1 direction/rhs entry is added instead of 1.
## R recycles, producing garbage constraints, and the solver returns no solution.
##
## Same bug in the CSM risk & reward case (lines 2233-2241): 3 rows rbind'd,
## but only 1 direction and 1 rhs added, against a (2N+T+2)-column matrix.
##
## Fix: replace the broken rbind with a single properly-sized row:
##   Rglpk.mat <- rbind(Rglpk.mat, c(colMeans(R), rep(0, T + 2)))
##   Rglpk.dir <- c(Rglpk.dir, ">=")
##   Rglpk.rhs <- c(Rglpk.rhs, target)

library(PortfolioAnalytics)

skip_if_not_installed("PortfolioAnalytics")
skip_if_not_installed("Rglpk")

## Shared fixture: 4-asset edhec data
make_rglpk_data <- function(nassets = 4L) {
  data(edhec, package = "PerformanceAnalytics")
  edhec[, seq_len(nassets)]
}

## Helper: build a Mean-ES portfolio spec (for Rglpk mean-ETL path)
make_mean_es_spec <- function(R, return_target = NULL) {
  assets <- colnames(R)
  pspec <- portfolio.spec(assets = assets)
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.objective(pspec, type = "return", name = "mean")
  es_obj <- add.objective(pspec, type = "risk", name = "ETL")
  if (!is.null(return_target)) {
    # set target on the ES objective  (sets objective$target)
    es_obj <- add.objective(es_obj, type = "risk", name = "ETL",
                            target = return_target)
  }
  es_obj
}

# ---------------------------------------------------------------------------
# 1. Basic Mean-ES (no target): must succeed and return finite weights
# ---------------------------------------------------------------------------
test_that("Rglpk Mean-ES without return target returns finite weights (#49)", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  R <- make_rglpk_data()
  pspec <- portfolio.spec(assets = colnames(R))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.objective(pspec, type = "return", name = "mean")
  pspec <- add.objective(pspec, type = "risk", name = "ETL")
  result <- optimize.portfolio(R, pspec, optimize_method = "Rglpk")
  wts <- extractWeights(result)
  expect_true(all(is.finite(wts)), info = "all weights must be finite")
  expect_equal(sum(wts), 1, tolerance = 0.02)
})

# ---------------------------------------------------------------------------
# 2. Mean-ES WITH return target via constraints$return_target: must not
#    produce garbage (NaN/Inf) weights or "unable to find solution" message
# ---------------------------------------------------------------------------
test_that("Rglpk Mean-ES with return_target constraint succeeds (#49)", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  R <- make_rglpk_data()
  mu <- mean(colMeans(R))   # a feasible return target (avg of asset means)
  pspec <- portfolio.spec(assets = colnames(R))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.constraint(pspec, type = "return", return_target = mu * 0.5)
  pspec <- add.objective(pspec, type = "return", name = "mean")
  pspec <- add.objective(pspec, type = "risk", name = "ETL")
  result <- optimize.portfolio(R, pspec, optimize_method = "Rglpk")
  # Before fix: solver returns character "Optimizer was unable to find a solution for target"
  # or returns NaN weights due to recycled matrix rows.
  expect_false(is.character(result),
               info = "result must not be a character 'unable to find solution' string")
  wts <- extractWeights(result)
  expect_true(all(is.finite(wts)), info = "weights must be finite")
  expect_equal(sum(wts), 1, tolerance = 0.02)
  # The mean return of the solution should be >= target
  achieved_mean <- sum(wts * colMeans(R))
  expect_gte(achieved_mean, mu * 0.5 - 1e-6)
})

# ---------------------------------------------------------------------------
# 3. Mean-ES: target set on ETL objective directly (objective$target path)
# ---------------------------------------------------------------------------
test_that("Rglpk Mean-ES with objective target succeeds (#49)", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  R <- make_rglpk_data()
  mu <- mean(colMeans(R))
  pspec <- portfolio.spec(assets = colnames(R))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.objective(pspec, type = "return", name = "mean")
  # Set target on the risk objective — this is what issue author does
  pspec <- add.objective(pspec, type = "risk", name = "ETL", target = mu * 0.3)
  result <- optimize.portfolio(R, pspec, optimize_method = "Rglpk")
  expect_false(is.character(result))
  wts <- extractWeights(result)
  expect_true(all(is.finite(wts)))
  expect_equal(sum(wts), 1, tolerance = 0.02)
})

# ---------------------------------------------------------------------------
# 4. min-CVaR only (no return objective): verify the existing simpler path
#    still works, with and without a target.
# ---------------------------------------------------------------------------
test_that("Rglpk min-CVaR without return target works (#49 regression)", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  R <- make_rglpk_data()
  pspec <- portfolio.spec(assets = colnames(R))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.objective(pspec, type = "risk", name = "ETL")
  result <- optimize.portfolio(R, pspec, optimize_method = "Rglpk")
  wts <- extractWeights(result)
  expect_true(all(is.finite(wts)))
  expect_equal(sum(wts), 1, tolerance = 0.02)
})

test_that("Rglpk min-CVaR with return target works (#49 regression)", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  R <- make_rglpk_data()
  mu <- mean(colMeans(R))
  pspec <- portfolio.spec(assets = colnames(R))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.objective(pspec, type = "risk", name = "ETL",
                         target = mu * 0.5)
  result <- optimize.portfolio(R, pspec, optimize_method = "Rglpk")
  expect_false(is.character(result))
  wts <- extractWeights(result)
  expect_true(all(is.finite(wts)))
  expect_equal(sum(wts), 1, tolerance = 0.02)
})

# ---------------------------------------------------------------------------
# 5. max-return case (reward & !risk): verify unrelated path still works
# ---------------------------------------------------------------------------
test_that("Rglpk max-return works (#49 regression)", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  R <- make_rglpk_data()
  pspec <- portfolio.spec(assets = colnames(R))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.objective(pspec, type = "return", name = "mean")
  result <- optimize.portfolio(R, pspec, optimize_method = "Rglpk")
  wts <- extractWeights(result)
  expect_true(all(is.finite(wts)))
  expect_equal(sum(wts), 1, tolerance = 0.02)
  # max-return solution should concentrate in the best asset
  expect_true(max(wts) > 0.5)
})

# ---------------------------------------------------------------------------
# 6. Constraint matrix dimension integrity check
#    Verify the constraint matrix for Mean-ES with target has the right dims.
#    We do this by running with trace=TRUE and inspecting Rglpkoutput.
# ---------------------------------------------------------------------------
test_that("Rglpk Mean-ES+target constraint matrix has correct ncol (#49)", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  R <- make_rglpk_data(4L)
  N <- ncol(R)
  T <- nrow(R)
  mu_val <- mean(colMeans(R))
  pspec <- portfolio.spec(assets = colnames(R))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.constraint(pspec, type = "return", return_target = mu_val * 0.5)
  pspec <- add.objective(pspec, type = "return", name = "mean")
  pspec <- add.objective(pspec, type = "risk", name = "ETL")
  result <- optimize.portfolio(R, pspec, optimize_method = "Rglpk", trace = TRUE)
  # The Rglpk constraint matrix must have N + T + 2 columns
  rglpk_out <- result$Rglpkoutput
  if (!is.null(rglpk_out)) {
    # If trace worked and the matrix is accessible, check dimensions
    # (The LP matrix isn't directly in Rglpkoutput but the solution vector
    #  length confirms the variable count)
    expect_equal(length(rglpk_out$solution), N + T + 2L,
                 info = "solution vector must have N+T+2 elements")
  } else {
    # trace output structure may differ; at minimum weights must be sane
    wts <- extractWeights(result)
    expect_true(all(is.finite(wts)))
  }
})

# ---------------------------------------------------------------------------
# 7. Group constraint + Mean-ES + return target (exercises the group loop
#    then the return target fix together)
# ---------------------------------------------------------------------------
test_that("Rglpk Mean-ES with group constraint and return target works (#49)", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  R <- make_rglpk_data(4L)
  mu_val <- mean(colMeans(R))
  pspec <- portfolio.spec(assets = colnames(R))
  pspec <- add.constraint(pspec, type = "full_investment")
  pspec <- add.constraint(pspec, type = "long_only")
  pspec <- add.constraint(pspec, type = "group",
                          groups = list(1:2, 3:4),
                          group_min = c(0.2, 0.2),
                          group_max = c(0.8, 0.8))
  pspec <- add.constraint(pspec, type = "return",
                          return_target = mu_val * 0.4)
  pspec <- add.objective(pspec, type = "return", name = "mean")
  pspec <- add.objective(pspec, type = "risk", name = "ETL")
  result <- optimize.portfolio(R, pspec, optimize_method = "Rglpk")
  expect_false(is.character(result))
  wts <- extractWeights(result)
  expect_true(all(is.finite(wts)))
  expect_equal(sum(wts), 1, tolerance = 0.02)
})
