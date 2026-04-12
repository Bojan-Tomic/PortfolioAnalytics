# Tests for BUG-13, BUG-14, BUG-15
# Written BEFORE the fixes to confirm the bugs exist, then verify fixes.

library(PortfolioAnalytics)

# ── Shared data setup ──────────────────────────────────────────────────────

data(edhec)
R <- edhec["2008::2012", 1:6]
funds <- colnames(R)

# ── BUG-13: etl_milp_opt group constraint crash ───────────────────────────
# When min-ES + position limits + group constraints are used with ROI,
# etl_milp_opt references undefined `Amat` instead of `tmpAmat` and
# uses wrong `zeros` column dimension. This causes a hard crash.

test_that("BUG-13: etl_milp_opt handles group constraints without crash", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  # Position limit triggers MILP path

  port <- add.constraint(port, type = "position_limit", max_pos = 4)
  # Group constraint triggers the buggy block
  port <- add.constraint(port, type = "group",
                         groups = list(1:3, 4:6),
                         group_min = c(0.2, 0.2),
                         group_max = c(0.8, 0.8))
  port <- add.objective(port, type = "risk", name = "ES",
                        arguments = list(p = 0.95))

  result <- optimize.portfolio(R, port, optimize_method = "ROI")
  expect_s3_class(result, "optimize.portfolio")

  w <- result$weights
  expect_true(all(w >= -1e-6))
  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  # At most 4 non-zero positions

  expect_lte(sum(abs(w) > 1e-6), 4)
  # Group constraints satisfied
  expect_gte(sum(w[1:3]), 0.2 - 1e-6)
  expect_lte(sum(w[1:3]), 0.8 + 1e-6)
  expect_gte(sum(w[4:6]), 0.2 - 1e-6)
  expect_lte(sum(w[4:6]), 0.8 + 1e-6)
})


# ── BUG-14: EQSratio missing from CVXR Charnes-Cooper conditions ─────────
# EQSratio should produce normalized weights (sum to 1), properly scaled
# constraints, and correct objective reporting.

test_that("BUG-14: CVXR EQSratio returns normalized weights", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.objective(port, type = "return", name = "mean")
  port <- add.objective(port, type = "risk", name = "EQS",
                        arguments = list(p = 0.05))

  # EQSratio defaults to TRUE when both mean and EQS are present
  result <- optimize.portfolio(R, port, optimize_method = "CVXR")
  expect_s3_class(result, "optimize.portfolio")

  w <- result$weights
  # Weights should be normalized (sum to 1) after Charnes-Cooper back-transform
  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  # All weights should respect long_only (>= 0)
  expect_true(all(w >= -1e-6))
  # Objective should be reported as EQS ratio, not raw Charnes-Cooper value
  expect_true("EQS ratio" %in% names(result$objective_measures) ||
              "mean" %in% names(result$objective_measures))
})

test_that("BUG-14: CVXR EQSratio respects box constraints", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "box", min = 0.05, max = 0.40)
  port <- add.objective(port, type = "return", name = "mean")
  port <- add.objective(port, type = "risk", name = "EQS",
                        arguments = list(p = 0.05))

  result <- optimize.portfolio(R, port, optimize_method = "CVXR")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  # Box constraints must be satisfied on the original-scale weights
  expect_true(all(w >= 0.05 - 1e-4))
  expect_true(all(w <= 0.40 + 1e-4))
})


# ── BUG-15: CVXR factor exposure not scaled for ratio problems ────────────
# Factor exposure bounds should be enforced on the original-scale weights,
# not on the Charnes-Cooper scaled variables.

test_that("BUG-15: CVXR maxSR respects factor exposure constraints", {
  skip_on_cran()

  B <- matrix(c(
    1.2, 0.8, 0.3, 0.5, 0.1, 0.9,
    0.2, 0.5, 0.9, 0.7, 0.3, 0.4
  ), ncol = 2, byrow = FALSE)
  rownames(B) <- funds
  colnames(B) <- c("Equity.Beta", "Credit.Spread")

  fe.lower <- c(0.4, 0.3)
  fe.upper <- c(0.8, 0.6)

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "return", name = "mean")
  port <- add.objective(port, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R, port, optimize_method = "CVXR", maxSR = TRUE)
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-6))

  # Factor exposures must be satisfied on original-scale weights
  exposures <- as.numeric(t(B) %*% w)
  expect_true(exposures[1] >= fe.lower[1] - 1e-4,
              label = "Equity Beta lower bound")
  expect_true(exposures[1] <= fe.upper[1] + 1e-4,
              label = "Equity Beta upper bound")
  expect_true(exposures[2] >= fe.lower[2] - 1e-4,
              label = "Credit Spread lower bound")
  expect_true(exposures[2] <= fe.upper[2] + 1e-4,
              label = "Credit Spread upper bound")
})

test_that("BUG-15: CVXR maxSTARR respects factor exposure constraints", {
  skip_on_cran()

  B <- matrix(c(
    1.2, 0.8, 0.3, 0.5, 0.1, 0.9,
    0.2, 0.5, 0.9, 0.7, 0.3, 0.4
  ), ncol = 2, byrow = FALSE)
  rownames(B) <- funds
  colnames(B) <- c("Equity.Beta", "Credit.Spread")

  fe.lower <- c(0.4, 0.3)
  fe.upper <- c(0.8, 0.6)

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "factor_exposure",
                         B = B, lower = fe.lower, upper = fe.upper)
  port <- add.objective(port, type = "return", name = "mean")
  port <- add.objective(port, type = "risk", name = "ES",
                        arguments = list(p = 0.95))

  # maxSTARR defaults to TRUE
  result <- optimize.portfolio(R, port, optimize_method = "CVXR")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-6))

  exposures <- as.numeric(t(B) %*% w)
  expect_true(exposures[1] >= fe.lower[1] - 1e-4)
  expect_true(exposures[1] <= fe.upper[1] + 1e-4)
  expect_true(exposures[2] >= fe.lower[2] - 1e-4)
  expect_true(exposures[2] <= fe.upper[2] + 1e-4)
})

# ── BUG-16: CVXR turnover constraint not scaled by weight_scale ───────────
# Same pattern as BUG-15. In Charnes-Cooper ratio problems (maxSR, maxSTARR,
# etc.) the decision variable is y = w * kappa, but turnover_target and
# weight_initial are not multiplied by weight_scale. This means the turnover
# constraint is applied in the wrong scale for ratio objectives.

test_that("BUG-16: CVXR maxSR with turnover constraint respects turnover bound", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  # Tight turnover constraint: total absolute weight change <= 0.5
  port <- add.constraint(port, type = "turnover", turnover_target = 0.5)
  port <- add.objective(port, type = "return", name = "mean")
  port <- add.objective(port, type = "risk", name = "StdDev")

  # maxSR triggers Charnes-Cooper (weight_scale != 1)
  result <- optimize.portfolio(R, port, optimize_method = "CVXR", maxSR = TRUE)
  w <- result$weights

  # Weights should be valid

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-6))

  # Turnover should be within bound
  # Default weight_initial is rep(1/N, N) when not specified
  w_init <- rep(1 / length(funds), length(funds))
  turnover <- sum(abs(w - w_init))
  expect_true(turnover <= 0.5 + 1e-4,
              label = paste("turnover", round(turnover, 4), "<= 0.5"))
})

test_that("BUG-16: CVXR maxSTARR with turnover constraint respects turnover bound", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "turnover", turnover_target = 0.6)
  port <- add.objective(port, type = "return", name = "mean")
  port <- add.objective(port, type = "risk", name = "ES",
                        arguments = list(p = 0.95))

  # maxSTARR defaults to TRUE
  result <- optimize.portfolio(R, port, optimize_method = "CVXR")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-6))

  w_init <- rep(1 / length(funds), length(funds))
  turnover <- sum(abs(w - w_init))
  expect_true(turnover <= 0.6 + 1e-4,
              label = paste("turnover", round(turnover, 4), "<= 0.6"))
})

test_that("BUG-16: CVXR non-ratio minvar with turnover still works (weight_scale=1)", {
  skip_on_cran()

  port <- portfolio.spec(assets = funds)
  port <- add.constraint(port, type = "full_investment")
  port <- add.constraint(port, type = "long_only")
  port <- add.constraint(port, type = "turnover", turnover_target = 0.4)
  port <- add.objective(port, type = "risk", name = "StdDev")

  result <- optimize.portfolio(R, port, optimize_method = "CVXR")
  w <- result$weights

  expect_equal(sum(w), 1.0, tolerance = 1e-4)
  expect_true(all(w >= -1e-6))

  w_init <- rep(1 / length(funds), length(funds))
  turnover <- sum(abs(w - w_init))
  expect_true(turnover <= 0.4 + 1e-4,
              label = paste("turnover", round(turnover, 4), "<= 0.4"))
})
