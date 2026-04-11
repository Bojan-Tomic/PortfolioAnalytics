###############################################################################
# tests/testthat/test-random-portfolios.R
#
# Migrated from inst/tests/test_rp_sample.R.
# Tests random_portfolios() with the "sample" method under various constraint
# combinations, plus two additional method tests (simplex, grid).
#
# Bugs fixed vs. the original:
#   - rp1 check used portfolio=group.portf before group.portf was defined;
#     corrected to portfolio=init.portf.
#   - rp3/rp4/rp5 checks all used portfolio=group.portf; corrected to
#     lev.portf, pos1.portf, and pos2.portf respectively.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# Fixture: 4-asset portfolio with weight_sum and box constraints
# ---------------------------------------------------------------------------

utils::data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(init.portf,
  type = "weight_sum",
  min_sum = 0.99, max_sum = 1.01
)
init.portf <- add.constraint(init.portf,
  type = "box",
  min = -0.3, max = 0.65
)

# ---------------------------------------------------------------------------
# weight_sum + box constraints (eliminate=TRUE: only feasible rows kept)
# ---------------------------------------------------------------------------

set.seed(1234)
rp1 <- random_portfolios(init.portf, 1000, eliminate = TRUE)

test_that("at least 1 feasible portfolio satisfies weight_sum and box constraints", {
  expect_true(nrow(rp1) >= 1)
})

# ---------------------------------------------------------------------------
# weight_sum + box + group constraints (eliminate=TRUE)
# ---------------------------------------------------------------------------

group.portf <- add.constraint(init.portf,
  type = "group",
  groups = list(1:2, 3:4),
  group_min = c(0.08, 0.05),
  group_max = c(0.55, 0.85),
  group_pos = c(2, 2)
)

set.seed(1234)
rp2 <- random_portfolios(group.portf, 1000, eliminate = TRUE)

test_that("at least 1 feasible portfolio satisfies weight_sum, box, and group constraints", {
  expect_true(nrow(rp2) >= 1)
})

# ---------------------------------------------------------------------------
# weight_sum + box + leverage_exposure constraints (eliminate=FALSE)
# ---------------------------------------------------------------------------

lev.portf <- add.constraint(init.portf,
  type = "leverage_exposure",
  leverage = 1.6
)

set.seed(1234)
rp3 <- random_portfolios(lev.portf, 1000, eliminate = FALSE)

test_that("at least 1 feasible portfolio satisfies weight_sum, box, and leverage constraints", {
  expect_true(any(apply(rp3, 1, PortfolioAnalytics:::check_constraints,
    portfolio = lev.portf
  )))
})

# ---------------------------------------------------------------------------
# weight_sum + box + position_limit (max_pos) constraints (eliminate=FALSE)
# ---------------------------------------------------------------------------

pos1.portf <- add.constraint(init.portf,
  type = "position_limit",
  max_pos = 3
)

set.seed(1234)
rp4 <- random_portfolios(pos1.portf, 1000, eliminate = FALSE)

test_that("at least 1 feasible portfolio satisfies weight_sum, box, and position limit constraints", {
  expect_true(any(apply(rp4, 1, PortfolioAnalytics:::check_constraints,
    portfolio = pos1.portf
  )))
})

# ---------------------------------------------------------------------------
# weight_sum + box + position_limit (long/short) constraints (eliminate=FALSE)
# ---------------------------------------------------------------------------

pos2.portf <- add.constraint(init.portf,
  type = "position_limit",
  max_pos_long = 3, max_pos_short = 1
)

set.seed(1234)
rp5 <- random_portfolios(pos2.portf, 1000, eliminate = FALSE)

test_that("at least 1 feasible portfolio satisfies weight_sum, box, and long/short position limit constraints", {
  expect_true(any(apply(rp5, 1, PortfolioAnalytics:::check_constraints,
    portfolio = pos2.portf
  )))
})

# ---------------------------------------------------------------------------
# Additional: simplex method
# ---------------------------------------------------------------------------

test_that("simplex method returns a matrix with ncol=4 and unit-sum rows", {
  set.seed(1234)
  rp_simplex <- random_portfolios(init.portf, 50, method = "simplex")
  expect_true(is.matrix(rp_simplex))
  expect_equal(ncol(rp_simplex), 4)
  # Simplex draws sum exactly to 1; allow for the 0.99-1.01 weight_sum band
  expect_true(all(abs(rowSums(rp_simplex) - 1) < 0.02))
})

# ---------------------------------------------------------------------------
# Additional: grid method
# ---------------------------------------------------------------------------

test_that("grid method returns a matrix with ncol=4", {
  set.seed(1234)
  rp_grid <- random_portfolios(init.portf, 50, method = "grid")
  expect_true(is.matrix(rp_grid))
  expect_equal(ncol(rp_grid), 4)
})
