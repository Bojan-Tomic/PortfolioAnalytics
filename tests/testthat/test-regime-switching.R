###############################################################################
# tests/testthat/test-regime-switching.R
#
# Source files covered:
#   R/portfolio.R          — regime.portfolios() constructor, is.portfolio()
#   R/optimize.portfolio.R — regime switching detection and dispatch path
#   R/generics.R           — print.regime.portfolios()
#   R/random_portfolios.R  — rp.regime.portfolios()
#
# Tests the full regime-switching workflow: constructing a regime.portfolios
# object, printing it, optimizing with it (random method), and generating
# random portfolios that respect the regime structure.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("DEoptim")   # guard for the optimization tests

# ---------------------------------------------------------------------------
# Shared data and portfolio specs (file scope)
# ---------------------------------------------------------------------------

utils::data(edhec)
R   <- edhec[, 1:6]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EMN", "ED")
nms <- colnames(R)

# Deterministic regime: first 60 % of observations → regime 1,
# last 40 % → regime 2.  Integer codes ensure reproducibility with no
# randomness in the regime assignment.
n_obs       <- nrow(R)
regime_vals <- c(rep(1L, ceiling(n_obs * 0.6)),
                 rep(2L, n_obs - ceiling(n_obs * 0.6)))
regime <- xts::xts(regime_vals, order.by = zoo::index(R))

# Portfolio for regime 1 — conservative (tighter box constraints)
port1 <- portfolio.spec(nms)
port1 <- add.constraint(port1, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
port1 <- add.constraint(port1, type = "box", min = 0.05, max = 0.50)
port1 <- add.objective(port1,  type = "risk", name = "StdDev")

# Portfolio for regime 2 — less constrained
port2 <- portfolio.spec(nms)
port2 <- add.constraint(port2, type = "weight_sum",
                        min_sum = 0.99, max_sum = 1.01)
port2 <- add.constraint(port2, type = "box", min = 0.00, max = 0.60)
port2 <- add.objective(port2,  type = "risk", name = "StdDev")

portfolios  <- combine.portfolios(list(port1, port2))
regime_port <- regime.portfolios(regime, portfolios)

# Optimization using the random method.
# The last observation in R falls in regime 2 (last 40 % of obs),
# so the optimizer selects port2 for this run.
set.seed(42)
opt_regime <- optimize.portfolio(
  R,
  regime_port,
  optimize_method = "random",
  search_size     = 500,
  trace           = FALSE
)

# ---------------------------------------------------------------------------
# 1–4  regime.portfolios constructor — class and structure
# ---------------------------------------------------------------------------

test_that("regime.portfolios has class 'regime.portfolios'", {
  expect_s3_class(regime_port, "regime.portfolios")
})

test_that("regime.portfolios also inherits class 'portfolio'", {
  expect_true(inherits(regime_port, "portfolio"))
})

test_that("regime_port$regime slot is an xts object", {
  expect_s3_class(regime_port$regime, "xts")
})

test_that("regime_port$portfolio.list has 2 elements, one per unique regime", {
  expect_equal(length(regime_port$portfolio.list), 2L)
})

# ---------------------------------------------------------------------------
# 5–6  regime.portfolios constructor — error conditions
# ---------------------------------------------------------------------------

test_that("regime.portfolios errors when portfolio count != unique regime count", {
  # Passing only 1 portfolio for a 2-regime series must fail
  expect_error(
    regime.portfolios(regime, combine.portfolios(list(port1)))
  )
})

test_that("regime.portfolios errors when regime is not an xts or zoo object", {
  expect_error(
    regime.portfolios(as.numeric(regime_vals), portfolios)
  )
})

# ---------------------------------------------------------------------------
# 7  print.regime.portfolios
# ---------------------------------------------------------------------------

test_that("print.regime.portfolios produces output containing 'Regime Switching'", {
  expect_output(print(regime_port), regexp = "Regime Switching")
})

# ---------------------------------------------------------------------------
# 8–12  optimize.portfolio with regime switching
# ---------------------------------------------------------------------------

test_that("regime optimization result has class 'optimize.portfolio.random'", {
  expect_s3_class(opt_regime, "optimize.portfolio.random")
})

test_that("extractWeights on regime result returns a named numeric vector", {
  w <- extractWeights(opt_regime)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
})

test_that("extractWeights on regime result has length 6 (one per asset)", {
  w <- extractWeights(opt_regime)
  expect_equal(length(w), 6L)
})

test_that("regime optimization weights sum to approximately 1 (within 0.02)", {
  w <- extractWeights(opt_regime)
  # weight_sum constraint allows [0.99, 1.01]; allow 0.02 total slack
  expect_equal(sum(w), 1, tolerance = 0.02)
})

test_that("opt_regime$regime records which regime was active (numeric 1 or 2)", {
  expect_true(is.numeric(opt_regime$regime))
  expect_true(opt_regime$regime %in% 1:2)
})

test_that("opt_regime$regime equals 2 because last observation is in regime 2", {
  # Our deterministic setup ends with regime 2 (last 40 % of obs)
  expect_equal(as.integer(opt_regime$regime), 2L)
})

# ---------------------------------------------------------------------------
# 13–16  rp.regime.portfolios
# ---------------------------------------------------------------------------

test_that("rp.regime.portfolios returns a matrix", {
  set.seed(1)
  rp_out <- PortfolioAnalytics:::rp.regime.portfolios(regime_port, permutations = 50)
  expect_true(is.matrix(rp_out))
})

test_that("rp.regime.portfolios matrix has 6 columns (one per asset)", {
  set.seed(1)
  rp_out <- PortfolioAnalytics:::rp.regime.portfolios(regime_port, permutations = 50)
  expect_equal(ncol(rp_out), 6L)
})

test_that("rp.regime.portfolios matrix rows sum to approximately 1", {
  set.seed(1)
  rp_out   <- PortfolioAnalytics:::rp.regime.portfolios(regime_port, permutations = 50)
  row_sums <- rowSums(rp_out)
  # weight_sum constraint allows [0.99, 1.01]; allow 0.02 total slack
  expect_true(all(abs(row_sums - 1) < 0.02))
})

test_that("rp.regime.portfolios errors if argument is not a regime.portfolios object", {
  # portfolios is a portfolio.list, not regime.portfolios
  expect_error(PortfolioAnalytics:::rp.regime.portfolios(portfolios, permutations = 10))
})

# ---------------------------------------------------------------------------
# Additional structural integrity checks
# ---------------------------------------------------------------------------

test_that("regime xts has the same number of rows as R", {
  expect_equal(nrow(regime_port$regime), nrow(R))
})

test_that("regime contains only the integer values 1 and 2", {
  unique_vals <- sort(unique(as.integer(coredata(regime_port$regime))))
  expect_equal(unique_vals, c(1L, 2L))
})

test_that("both portfolios in portfolio.list use identical asset names", {
  assets1 <- names(regime_port$portfolio.list[[1]]$assets)
  assets2 <- names(regime_port$portfolio.list[[2]]$assets)
  expect_equal(assets1, assets2)
})

test_that("regime_port$assets is a named numeric vector of length 6", {
  expect_true(is.numeric(regime_port$assets))
  expect_equal(length(regime_port$assets), 6L)
  expect_equal(names(regime_port$assets), nms)
})
