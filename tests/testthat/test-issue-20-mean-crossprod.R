### Regression tests for GitHub issue #20
### "objective name mean generated an error or warning: Error in crossprod(x, y)"
###
### Two bugs:
###  1. port.mean(weights, mu) fails with a cryptic error when mu is NULL or
###     missing (custom momentFUN that doesn't return mu).
###  2. search[i] <- ifelse(try(rp_objective_results[[i]]$out), ...) produces
###     "replacement has length zero" when $out is NULL because every
###     constrained_objective call errored out.
###
### Fix: guard mu in port.mean(); fix the ifelse pattern in the random loop.

library(testthat)
library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_if_not_installed("PortfolioAnalytics")
skip_if_not_installed("PerformanceAnalytics")

# Helper: build a simple 4-asset spec with mean + StdDev objectives
make_spec <- function(R) {
  asset_names <- colnames(R)
  ps <- portfolio.spec(asset_names)
  ps <- add.constraint(ps, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "return", name = "mean")
  ps <- add.objective(ps, type = "risk",   name = "StdDev")
  ps
}

test_that("port.mean returns NA (not an error) when mu is NULL", {
  skip_on_cran()
  # Direct unit test: port.mean should handle NULL mu gracefully
  w <- c(0.25, 0.25, 0.25, 0.25)
  # With valid mu it should work
  mu <- matrix(c(0.01, 0.02, 0.015, 0.005), ncol = 1)
  expect_no_error(PortfolioAnalytics:::port.mean(w, mu))
  expect_equal(PortfolioAnalytics:::port.mean(w, mu), 0.0125)

  # With NULL mu it should return NA, not throw
  result <- PortfolioAnalytics:::port.mean(w, NULL)
  expect_true(is.na(result))
})

test_that("optimize.portfolio random method succeeds when momentFUN omits mu", {
  skip_on_cran()
  data(edhec)
  R <- edhec[, 1:4]
  ps <- make_spec(R)

  # Custom momentFUN that intentionally omits mu (simulates a poorly-written
  # custom moment function, which was the root cause of issue #20).
  # Assigned to .GlobalEnv so match.fun() can resolve it.
  assign("my_moments_no_mu_20", function(R, portfolio) {
    list(sigma = cov(R))  # no mu!
  }, envir = .GlobalEnv)

  set.seed(42)
  # Should complete without error (objective may be NA/penalised but no crash)
  expect_no_error(
    opt <- optimize.portfolio(
      R = R, portfolio = ps,
      optimize_method = "random",
      momentFUN = "my_moments_no_mu_20",
      search_size = 200L,
      trace = TRUE
    )
  )
  expect_true(is.list(opt))
  expect_true(!is.null(opt$weights))
})

test_that("optimize.portfolio random method succeeds with standard momentFUN (mean obj)", {
  skip_on_cran()
  data(edhec)
  R <- edhec[, 1:4]
  ps <- make_spec(R)

  set.seed(42)
  expect_no_error(
    opt <- optimize.portfolio(
      R = R, portfolio = ps,
      optimize_method = "random",
      search_size = 200L,
      trace = TRUE
    )
  )
  expect_true(!is.null(opt$weights))
  expect_equal(length(opt$weights), 4L)
})

test_that("optimize.portfolio random+trace works with mean+StdDev+risk_budget objectives", {
  skip_on_cran()
  data(edhec)
  R <- edhec[, 1:4]
  ps <- portfolio.spec(colnames(R))
  ps <- add.constraint(ps, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "return",      name = "mean")
  ps <- add.objective(ps, type = "risk",         name = "StdDev")
  ps <- add.objective(ps, type = "risk_budget",  name = "StdDev",
                       min_prisk = 0.05, max_prisk = 0.40)

  set.seed(42)
  expect_no_error(
    opt <- optimize.portfolio(
      R = R, portfolio = ps,
      optimize_method = "random",
      search_size = 200L,
      trace = TRUE
    )
  )
  expect_true(!is.null(opt$weights))
})
