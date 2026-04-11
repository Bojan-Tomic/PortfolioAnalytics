###############################################################################
# tests/testthat/test-optimize-portfolio-v1-gensa-portflist.R
#
# Coverage targets (R/optimize.portfolio.R — 74.44% baseline, ~400 uncovered):
#
#   optimize.portfolio_v1  L17-L486  (almost entirely uncovered)
#     - DEoptim path        L92-L209
#     - random path         L210-L319
#     - GenSA path          L425-L474
#
#   optimize.portfolio_v2  L697-L720  (portfolio.list dispatch)
#   optimize.portfolio_v2  L1349-L1402 (GenSA path)
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("PerformanceAnalytics")

utils::data(edhec)
R4 <- edhec[, 1:4]
nms <- colnames(R4)

# ===========================================================================
# Helper: build a v1 constraint object for use with optimize.portfolio_v1
# ===========================================================================

make_v1_con <- function(assets     = nms,
                        add_return = TRUE,
                        add_risk   = TRUE,
                        weight_seq = NULL) {
  con <- suppressMessages(
    constraint_v1(assets = assets, min = 0, max = 1,
                  min_sum = 0.99, max_sum = 1.01,
                  weight_seq = weight_seq)
  )
  if (add_return)
    con <- add.objective_v1(con, type = "return", name = "mean", enabled = TRUE)
  if (add_risk)
    con <- add.objective_v1(con, type = "risk",   name = "StdDev", enabled = TRUE)
  con
}

# ===========================================================================
# Section 1: optimize.portfolio_v1 — DEoptim path (L92-L209)
# ===========================================================================

set.seed(42)
opt_v1_de <- suppressMessages(suppressWarnings(
  optimize.portfolio_v1(
    R            = R4,
    constraints  = make_v1_con(),
    optimize_method = "DEoptim",
    search_size  = 200L,
    itermax      = 5L,
    momentFUN    = PortfolioAnalytics:::set.portfolio.moments_v1
  )
))

test_that("optimize.portfolio_v1 DEoptim: returns optimize.portfolio.DEoptim object", {
  expect_s3_class(opt_v1_de, "optimize.portfolio.DEoptim")
  expect_s3_class(opt_v1_de, "optimize.portfolio")
})

test_that("optimize.portfolio_v1 DEoptim: weights sum to approximately 1", {
  w <- opt_v1_de$weights
  expect_equal(sum(w), 1, tolerance = 0.02)
})

test_that("optimize.portfolio_v1 DEoptim: all weights within [0, 1]", {
  w <- opt_v1_de$weights
  expect_true(all(w >= -1e-6) && all(w <= 1 + 1e-6))
})

test_that("optimize.portfolio_v1 DEoptim: objective_measures is a list", {
  expect_true(is.list(opt_v1_de$objective_measures))
})

test_that("optimize.portfolio_v1 DEoptim: constraints slot is present", {
  expect_false(is.null(opt_v1_de$constraints))
})

test_that("optimize.portfolio_v1 DEoptim: elapsed_time is set", {
  expect_false(is.null(opt_v1_de$elapsed_time))
})

# ===========================================================================
# Section 2: optimize.portfolio_v1 — random portfolios path (L210-L319)
# ===========================================================================

set.seed(42)
opt_v1_rnd <- suppressMessages(suppressWarnings(
  optimize.portfolio_v1(
    R               = R4,
    constraints     = make_v1_con(weight_seq = generatesequence(min = 0, max = 1, by = 0.01)),
    optimize_method = "random",
    search_size     = 200L,
    momentFUN       = PortfolioAnalytics:::set.portfolio.moments_v1
  )
))

test_that("optimize.portfolio_v1 random: returns optimize.portfolio.random object", {
  expect_s3_class(opt_v1_rnd, "optimize.portfolio.random")
})

test_that("optimize.portfolio_v1 random: weights sum to approximately 1", {
  w <- opt_v1_rnd$weights
  expect_equal(sum(w), 1, tolerance = 0.02)
})

test_that("optimize.portfolio_v1 random: all weights within box constraints", {
  w <- opt_v1_rnd$weights
  expect_true(all(w >= -1e-6) && all(w <= 1 + 1e-6))
})

# trace=TRUE collects per-trial statistics
set.seed(42)
opt_v1_rnd_trace <- suppressMessages(suppressWarnings(
  optimize.portfolio_v1(
    R               = R4,
    constraints     = make_v1_con(weight_seq = generatesequence(min = 0, max = 1, by = 0.01)),
    optimize_method = "random",
    search_size     = 200L,
    trace           = TRUE,
    momentFUN       = PortfolioAnalytics:::set.portfolio.moments_v1
  )
))

test_that("optimize.portfolio_v1 random trace=TRUE: random_portfolio_objective_results is non-null", {
  expect_false(is.null(opt_v1_rnd_trace$random_portfolio_objective_results))
})

# ===========================================================================
# Section 3: optimize.portfolio_v1 — GenSA path (L425-L474)
# ===========================================================================

skip_if_not_installed("GenSA")

set.seed(42)
opt_v1_gsa <- suppressMessages(suppressWarnings(
  optimize.portfolio_v1(
    R               = R4,
    constraints     = make_v1_con(),
    optimize_method = "GenSA",
    momentFUN       = PortfolioAnalytics:::set.portfolio.moments_v1
  )
))

test_that("optimize.portfolio_v1 GenSA: returns optimize.portfolio.GenSA object", {
  skip_if(is.null(opt_v1_gsa))
  expect_s3_class(opt_v1_gsa, "optimize.portfolio.GenSA")
})

test_that("optimize.portfolio_v1 GenSA: weights sum to approximately 1", {
  skip_if(is.null(opt_v1_gsa))
  w <- opt_v1_gsa$weights
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# trace=TRUE adds GenSAoutput slot (L471-L473)
set.seed(42)
opt_v1_gsa_trace <- suppressMessages(suppressWarnings(
  optimize.portfolio_v1(
    R               = R4,
    constraints     = make_v1_con(),
    optimize_method = "GenSA",
    trace           = TRUE,
    momentFUN       = PortfolioAnalytics:::set.portfolio.moments_v1
  )
))

test_that("optimize.portfolio_v1 GenSA trace=TRUE: GenSAoutput slot present", {
  skip_if(is.null(opt_v1_gsa_trace))
  expect_false(is.null(opt_v1_gsa_trace$GenSAoutput))
})

# ===========================================================================
# Section 4: optimize.portfolio_v2 — GenSA path (L1349-L1402)
# ===========================================================================

portf_gsa <- portfolio.spec(nms)
portf_gsa <- add.constraint(portf_gsa, type = "full_investment")
portf_gsa <- add.constraint(portf_gsa, type = "long_only")
portf_gsa <- add.objective(portf_gsa, type = "risk",   name = "StdDev")
portf_gsa <- add.objective(portf_gsa, type = "return", name = "mean")

set.seed(42)
opt_v2_gsa <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_gsa, optimize_method = "GenSA", trace = FALSE)
))

test_that("optimize.portfolio_v2 GenSA: returns optimize.portfolio.GenSA object", {
  expect_s3_class(opt_v2_gsa, "optimize.portfolio.GenSA")
})

test_that("optimize.portfolio_v2 GenSA: weights sum to 1", {
  w <- opt_v2_gsa$weights
  expect_equal(sum(w), 1, tolerance = 0.02)
})

test_that("optimize.portfolio_v2 GenSA: objective_measures is a list", {
  expect_true(is.list(opt_v2_gsa$objective_measures))
})

# trace=TRUE adds GenSAoutput slot (L1395-L1397)
set.seed(42)
opt_v2_gsa_trace <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_gsa, optimize_method = "GenSA", trace = TRUE)
))

test_that("optimize.portfolio_v2 GenSA trace=TRUE: GenSAoutput slot is present", {
  expect_false(is.null(opt_v2_gsa_trace$GenSAoutput))
})

# Single-objective (only risk): exercises the else branch of moments check
portf_gsa_risk <- portfolio.spec(nms)
portf_gsa_risk <- add.constraint(portf_gsa_risk, type = "full_investment")
portf_gsa_risk <- add.constraint(portf_gsa_risk, type = "long_only")
portf_gsa_risk <- add.objective(portf_gsa_risk, type = "risk", name = "StdDev")

set.seed(42)
opt_v2_gsa_risk <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_gsa_risk, optimize_method = "GenSA", trace = FALSE)
))

test_that("optimize.portfolio_v2 GenSA single risk objective: valid result", {
  expect_s3_class(opt_v2_gsa_risk, "optimize.portfolio.GenSA")
  w <- opt_v2_gsa_risk$weights
  expect_equal(sum(w), 1, tolerance = 0.02)
})

# ===========================================================================
# Section 5: optimize.portfolio_v2 — portfolio.list dispatch (L697-L720)
# ===========================================================================

portf_pl1 <- portfolio.spec(nms)
portf_pl1 <- add.constraint(portf_pl1, type = "full_investment")
portf_pl1 <- add.constraint(portf_pl1, type = "long_only")
portf_pl1 <- add.objective(portf_pl1, type = "risk", name = "StdDev")

portf_pl2 <- portfolio.spec(nms)
portf_pl2 <- add.constraint(portf_pl2, type = "full_investment")
portf_pl2 <- add.constraint(portf_pl2, type = "long_only")
portf_pl2 <- add.objective(portf_pl2, type = "return", name = "mean")

plist <- combine.portfolios(list(portf_pl1, portf_pl2))

opt_plist <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, plist, optimize_method = "ROI")
))

test_that("portfolio.list dispatch: returns opt.list object", {
  expect_s3_class(opt_plist, "opt.list")
})

test_that("portfolio.list dispatch: length equals number of portfolios", {
  expect_equal(length(opt_plist), 2L)
})

test_that("portfolio.list dispatch: each element is optimize.portfolio object", {
  for (i in seq_along(opt_plist)) {
    expect_s3_class(opt_plist[[i]], "optimize.portfolio")
  }
})

test_that("portfolio.list dispatch: first portfolio optimized StdDev", {
  expect_false(is.null(opt_plist[[1]]$weights))
  expect_equal(sum(opt_plist[[1]]$weights), 1, tolerance = 1e-6)
})

test_that("portfolio.list dispatch: second portfolio optimized return", {
  expect_false(is.null(opt_plist[[2]]$weights))
  expect_equal(sum(opt_plist[[2]]$weights), 1, tolerance = 1e-6)
})
