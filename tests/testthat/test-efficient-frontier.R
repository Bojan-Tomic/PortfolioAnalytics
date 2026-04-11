###############################################################################
# tests/testthat/test-efficient-frontier.R
#
# Migrated from inst/tests/test_demo_efficient_frontier.R
#  - Removed require(testthat) / library(testthat)
#  - Removed all context() calls
#  - Reproduced demo/demo_efficient_frontier.R setup inline (no sourcing)
#  - Added skip guards
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("ROI.plugin.glpk")

# ---------------------------------------------------------------------------
# Data and portfolio setup
# Reproduces demo/demo_efficient_frontier.R inline (no sourcing).
# ---------------------------------------------------------------------------

utils::data(edhec)
R <- edhec[, 1:5]
colnames(R) <- c("CA", "CTAG", "DS", "EM", "EQM")
funds <- colnames(R)

init <- portfolio.spec(assets = funds)
init <- add.constraint(portfolio = init, type = "full_investment")
init <- add.constraint(portfolio = init, type = "box", min = 0.15, max = 0.45)
init <- add.constraint(
  portfolio = init, type = "group",
  groups = list(c(1, 3), c(2, 4, 5)),
  group_min = 0.05,
  group_max = 0.7
)

# Mean-variance efficient frontier
meanvar.ef <- create.EfficientFrontier(
  R = R, portfolio = init,
  type = "mean-StdDev"
)

# Mean-ES efficient frontier
meanetl.ef <- create.EfficientFrontier(
  R = R, portfolio = init,
  type = "mean-ES"
)


# ---------------------------------------------------------------------------
# Tests: mean-variance efficient frontier
# ---------------------------------------------------------------------------

test_that("meanvar.ef$frontier has 25 rows", {
  expect_equal(nrow(meanvar.ef$frontier), 25)
})

test_that("colnames(meanvar.ef$frontier) are consistent", {
  expect_equal(
    colnames(meanvar.ef$frontier),
    c("mean", "StdDev", "out", "w.CA", "w.CTAG", "w.DS", "w.EM", "w.EQM")
  )
})

test_that("first row of meanvar.ef$frontier is consistent", {
  expect_equal(
    as.numeric(meanvar.ef$frontier[1, ]),
    c(0.005283925, 0.012275839, 0.012275839, 0.15, 0.15, 0.15, 0.15, 0.4),
    tolerance = 1e-6
  )
})

test_that("last row of meanvar.ef$frontier is consistent", {
  expect_equal(
    as.numeric(meanvar.ef$frontier[25, ]),
    c(0.005906312, 0.014822355, 0.014822355, 0.149997829, 0.149995349, 0.400022534, 0.149991112, 0.149993176),
    tolerance = 1e-6
  )
})


# ---------------------------------------------------------------------------
# Tests: mean-ES efficient frontier
# ---------------------------------------------------------------------------

test_that("meanetl.ef$frontier has 25 rows", {
  expect_equal(nrow(meanetl.ef$frontier), 25)
})

test_that("colnames(meanetl.ef$frontier) are consistent", {
  expect_equal(
    colnames(meanetl.ef$frontier),
    c("ES", "mean", "out", "w.CA", "w.CTAG", "w.DS", "w.EM", "w.EQM")
  )
})

test_that("first row of meanetl.ef$frontier is consistent", {
  expect_equal(
    as.numeric(meanetl.ef$frontier[1, ]),
    c(0.02626391, 0.00527968, 0.02626391, 0.15, 0.38464869, 0.15, 0.15, 0.16535131),
    tolerance = 1e-6
  )
})

test_that("last row of meanetl.ef$frontier is consistent", {
  expect_equal(
    as.numeric(meanetl.ef$frontier[25, ]),
    c(0.033300674, 0.005906279, 0.033300674, 0.15, 0.150000211, 0.399999788, 0.15, 0.150000001),
    tolerance = 1e-6
  )
})


# ---------------------------------------------------------------------------
# Tests: print and summary methods for efficient frontier objects
# These blocks exercise generics.R: print.efficient.frontier and
# summary.efficient.frontier.
# ---------------------------------------------------------------------------

test_that("print.efficient.frontier produces output without error", {
  expect_output(print(meanvar.ef))
})

test_that("summary.efficient.frontier produces output without error", {
  expect_output(print(summary(meanvar.ef)))
})

test_that("summary.efficient.frontier returns a list with weights and metrics", {
  s <- summary(meanvar.ef)
  expect_true(is.list(s))
  expect_true(!is.null(s$weights))
  expect_true(!is.null(s$metrics))
})
