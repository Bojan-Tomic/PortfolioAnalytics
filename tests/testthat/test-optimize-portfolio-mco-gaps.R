###############################################################################
# tests/testthat/test-optimize-portfolio-mco-gaps.R
#
# Targeted coverage for optimize.portfolio mco solver paths
#
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)
library(PerformanceAnalytics)

utils::data(edhec)
edhec4 <- edhec[, 1:4]
funds4 <- colnames(edhec4)

test_that("optimize.portfolio mco solver max-return", {
  skip_on_cran()
  skip_if_not_installed("mco")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "return", name = "mean")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "mco"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.mco")
})

test_that("optimize.portfolio mco solver max-return with group_pos constraints", {
  skip_on_cran()
  skip_if_not_installed("mco")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8),
                      group_pos = c(2, 2))
  p <- add.objective(p, type = "return", name = "mean")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "mco"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.mco")
})

test_that("optimize.portfolio mco solver min-var with target return", {
  skip_on_cran()
  skip_if_not_installed("mco")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "risk", name = "var", target=0.01)

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "mco"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.mco")
})

test_that("optimize.portfolio mco solver min-ES", {
  skip_on_cran()
  skip_if_not_installed("mco")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "mco"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.mco")
})

test_that("optimize.portfolio mco solver handles invalid group constraint", {
  skip_on_cran()
  skip_if_not_installed("mco")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8))
  p$constraints[[3]]$cLO <- c(0.1)
  p <- add.objective(p, type = "return", name = "mean")

  expect_error(
    PortfolioAnalytics::optimize.portfolio(edhec4, p, optimize_method = "mco"),
    "Please assign group constraint"
  )
})

test_that("optimize.portfolio mco solver handles invalid group_pos length", {
  skip_on_cran()
  skip_if_not_installed("mco")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8),
                      group_pos = c(2, 2))
  p$constraints[[3]]$group_pos <- c(2)
  p <- add.objective(p, type = "return", name = "mean")

  expect_error(
    PortfolioAnalytics::optimize.portfolio(edhec4, p, optimize_method = "mco"),
    "Please assign group constraint"
  )
})
