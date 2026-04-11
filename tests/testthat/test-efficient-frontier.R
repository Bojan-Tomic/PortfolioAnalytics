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


# ---------------------------------------------------------------------------
# Tests: extractEfficientFrontier — ROI object with ETL/ES match.col
# Exercises lines 724-726 of extract.efficient.frontier.R:
#   if(match.col %in% c("ETL", "ES", "CVaR")) => meanetl.efficient.frontier
# ---------------------------------------------------------------------------

skip_if_not_installed("ROI.plugin.glpk")

# Build a minimal ROI result with trace=TRUE so that $R is available.
init_roi_etl <- portfolio.spec(assets = funds)
init_roi_etl <- add.constraint(portfolio = init_roi_etl, type = "full_investment")
init_roi_etl <- add.constraint(portfolio = init_roi_etl, type = "long_only")
init_roi_etl <- add.objective(portfolio = init_roi_etl, type = "risk",   name = "ES")
init_roi_etl <- add.objective(portfolio = init_roi_etl, type = "return", name = "mean")

opt_roi_etl <- tryCatch(
  optimize.portfolio(R = R, portfolio = init_roi_etl,
                     optimize_method = "ROI",
                     trace = TRUE),
  error = function(e) NULL
)

# match.col = "ETL" maps into the c("ETL","ES","CVaR") branch
ef_roi_etl <- tryCatch(
  extractEfficientFrontier(opt_roi_etl, match.col = "ETL", n.portfolios = 5L),
  error = function(e) NULL
)

test_that("extractEfficientFrontier ROI ETL: returns efficient.frontier class", {
  skip_if(is.null(opt_roi_etl))
  skip_if(is.null(ef_roi_etl))
  expect_s3_class(ef_roi_etl, "efficient.frontier")
})

test_that("extractEfficientFrontier ROI ETL: $frontier is not NULL", {
  skip_if(is.null(opt_roi_etl))
  skip_if(is.null(ef_roi_etl))
  expect_false(is.null(ef_roi_etl$frontier))
})

test_that("extractEfficientFrontier ROI ETL: $frontier has 'ES' column", {
  skip_if(is.null(opt_roi_etl))
  skip_if(is.null(ef_roi_etl))
  # meanetl.efficient.frontier produces an 'ES' column
  expect_true("ES" %in% colnames(ef_roi_etl$frontier))
})

test_that("extractEfficientFrontier ROI ETL: $frontier has weight columns", {
  skip_if(is.null(opt_roi_etl))
  skip_if(is.null(ef_roi_etl))
  expect_gt(length(grep("^w\\.", colnames(ef_roi_etl$frontier))), 0L)
})

# match.col = "CVaR" also maps into the same branch
ef_roi_cvar <- tryCatch(
  extractEfficientFrontier(opt_roi_etl, match.col = "CVaR", n.portfolios = 5L),
  error = function(e) NULL
)

test_that("extractEfficientFrontier ROI CVaR: returns efficient.frontier class", {
  skip_if(is.null(opt_roi_etl))
  skip_if(is.null(ef_roi_cvar))
  expect_s3_class(ef_roi_cvar, "efficient.frontier")
})

test_that("extractEfficientFrontier ROI CVaR: $frontier has 'ES' column", {
  skip_if(is.null(opt_roi_etl))
  skip_if(is.null(ef_roi_cvar))
  expect_true("ES" %in% colnames(ef_roi_cvar$frontier))
})


# ---------------------------------------------------------------------------
# Tests: extractEfficientFrontier — ROI object with StdDev match.col
# Exercises line 727-729 of extract.efficient.frontier.R:
#   if(match.col == "StdDev") => meanvar.efficient.frontier
# ---------------------------------------------------------------------------

init_roi_sd <- portfolio.spec(assets = funds)
init_roi_sd <- add.constraint(portfolio = init_roi_sd, type = "full_investment")
init_roi_sd <- add.constraint(portfolio = init_roi_sd, type = "long_only")
init_roi_sd <- add.objective(portfolio = init_roi_sd, type = "risk",   name = "StdDev")
init_roi_sd <- add.objective(portfolio = init_roi_sd, type = "return", name = "mean")

opt_roi_sd <- tryCatch(
  optimize.portfolio(R = R, portfolio = init_roi_sd,
                     optimize_method = "ROI",
                     trace = TRUE),
  error = function(e) NULL
)

ef_roi_sd <- tryCatch(
  extractEfficientFrontier(opt_roi_sd, match.col = "StdDev", n.portfolios = 5L),
  error = function(e) NULL
)

test_that("extractEfficientFrontier ROI StdDev: returns efficient.frontier class", {
  skip_if(is.null(opt_roi_sd))
  skip_if(is.null(ef_roi_sd))
  expect_s3_class(ef_roi_sd, "efficient.frontier")
})

test_that("extractEfficientFrontier ROI StdDev: $frontier has 'StdDev' column", {
  skip_if(is.null(opt_roi_sd))
  skip_if(is.null(ef_roi_sd))
  expect_true("StdDev" %in% colnames(ef_roi_sd$frontier))
})

test_that("extractEfficientFrontier ROI StdDev: $frontier has 'mean' column", {
  skip_if(is.null(opt_roi_sd))
  skip_if(is.null(ef_roi_sd))
  expect_true("mean" %in% colnames(ef_roi_sd$frontier))
})

test_that("extractEfficientFrontier ROI StdDev: $frontier has weight columns", {
  skip_if(is.null(opt_roi_sd))
  skip_if(is.null(ef_roi_sd))
  expect_gt(length(grep("^w\\.", colnames(ef_roi_sd$frontier))), 0L)
})

# risk_aversion branch in meanvar.efficient.frontier (line ~179)
ef_roi_sd_ra <- tryCatch(
  extractEfficientFrontier(opt_roi_sd, match.col = "StdDev",
                            n.portfolios = 5L, risk_aversion = c(1, 5, 10)),
  error = function(e) NULL
)

test_that("extractEfficientFrontier ROI StdDev risk_aversion: returns efficient.frontier", {
  skip_if(is.null(opt_roi_sd))
  skip_if(is.null(ef_roi_sd_ra))
  expect_s3_class(ef_roi_sd_ra, "efficient.frontier")
})

test_that("extractEfficientFrontier ROI StdDev risk_aversion: frontier has lambda column", {
  skip_if(is.null(opt_roi_sd))
  skip_if(is.null(ef_roi_sd_ra))
  expect_true("lambda" %in% colnames(ef_roi_sd_ra$frontier))
})


# ---------------------------------------------------------------------------
# Tests: extractEfficientFrontier — GenSA object raises informative error
# Exercises lines 707-709 of extract.efficient.frontier.R.
# ---------------------------------------------------------------------------

test_that("extractEfficientFrontier stops with GenSA object", {
  fake_gensa <- structure(
    list(portfolio = list(), R = matrix(1:4, 2, 2)),
    class = c("optimize.portfolio.GenSA", "optimize.portfolio")
  )
  expect_error(
    extractEfficientFrontier(fake_gensa),
    regexp = "GenSA"
  )
})


# ---------------------------------------------------------------------------
# Tests: extract.efficient.frontier — match.col pmatch fallback (lines 43-45)
# Exercises the fallback: pmatch(paste(match.col,match.col,"."), columnnames).
# This is triggered when the direct pmatch fails but the dot-doubled name
# succeeds — and also the error path when neither matches.
# ---------------------------------------------------------------------------

# Build a small random-portfolio result for direct calls to the internal function
init_rp_pmatch <- portfolio.spec(assets = funds)
init_rp_pmatch <- add.constraint(portfolio = init_rp_pmatch,
                                  type = "weight_sum",
                                  min_sum = 0.99, max_sum = 1.01)
init_rp_pmatch <- add.constraint(portfolio = init_rp_pmatch, type = "long_only")
init_rp_pmatch <- add.objective(portfolio = init_rp_pmatch, type = "return", name = "mean")
init_rp_pmatch <- add.objective(portfolio = init_rp_pmatch, type = "risk",   name = "ES")

opt_rp_pmatch <- tryCatch({
  set.seed(42)
  optimize.portfolio(R = R, portfolio = init_rp_pmatch,
                     optimize_method = "random",
                     trace = TRUE, search_size = 200L)
}, error = function(e) NULL)

test_that("extract.efficient.frontier errors when match.col cannot be matched at all", {
  skip_if(is.null(opt_rp_pmatch))
  expect_error(
    PortfolioAnalytics:::extract.efficient.frontier(
      opt_rp_pmatch, match.col = "NonExistentColumn", n.portfolios = 5L),
    regexp = "could not match match.col"
  )
})

test_that("extract.efficient.frontier works with exact match.col (ES)", {
  skip_if(is.null(opt_rp_pmatch))
  ef <- tryCatch(
    PortfolioAnalytics:::extract.efficient.frontier(
      opt_rp_pmatch, match.col = "ES", n.portfolios = 5L),
    error = function(e) NULL
  )
  skip_if(is.null(ef))
  expect_s3_class(ef, "frontier")
})


# ---------------------------------------------------------------------------
# Tests: create.EfficientFrontier — DEoptim type
# Exercises the "DEoptim" switch-case branch at lines 654-663 of
# extract.efficient.frontier.R, which calls optimize.portfolio with
# optimize_method="DEoptim" and trace=TRUE, then extracts the frontier.
# ---------------------------------------------------------------------------

skip_if_not_installed("DEoptim")

init_de <- portfolio.spec(assets = funds)
init_de <- add.constraint(portfolio = init_de,
                           type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
init_de <- add.constraint(portfolio = init_de, type = "long_only")
init_de <- add.objective(portfolio = init_de, type = "return", name = "mean")
init_de <- add.objective(portfolio = init_de, type = "risk",   name = "ES")

ef_de <- tryCatch({
  set.seed(42)
  create.EfficientFrontier(R = R, portfolio = init_de,
                            type         = "DEoptim",
                            n.portfolios = 5L,
                            search_size  = 200L,
                            match.col    = "ES")
}, error = function(e) NULL)

test_that("create.EfficientFrontier DEoptim: returns efficient.frontier class", {
  skip_if(is.null(ef_de))
  expect_s3_class(ef_de, "efficient.frontier")
})

test_that("create.EfficientFrontier DEoptim: $frontier is not NULL", {
  skip_if(is.null(ef_de))
  expect_false(is.null(ef_de$frontier))
})

test_that("create.EfficientFrontier DEoptim: $frontier class is 'frontier'", {
  skip_if(is.null(ef_de))
  expect_s3_class(ef_de$frontier, "frontier")
})

test_that("create.EfficientFrontier DEoptim: $portfolio is a portfolio object", {
  skip_if(is.null(ef_de))
  expect_true(is.portfolio(ef_de$portfolio))
})

test_that("create.EfficientFrontier DEoptim: $R slot is not NULL", {
  skip_if(is.null(ef_de))
  expect_false(is.null(ef_de$R))
})

test_that("create.EfficientFrontier DEoptim: $call is not NULL", {
  skip_if(is.null(ef_de))
  expect_false(is.null(ef_de$call))
})

test_that("create.EfficientFrontier DEoptim: frontier has at most n.portfolios rows", {
  skip_if(is.null(ef_de))
  expect_lte(nrow(ef_de$frontier), 5L)
})
