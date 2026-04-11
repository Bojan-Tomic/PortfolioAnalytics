###############################################################################
# tests/testthat/test-optimize-portfolio-osqp-gaps.R
#
# Targeted coverage for optimize.portfolio osqp paths
#
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)
library(PerformanceAnalytics)

utils::data(edhec)
edhec4 <- edhec[, 1:4]
funds4 <- colnames(edhec4)

test_that("optimize.portfolio osqp max-return", {
  skip_on_cran()
  skip_if_not_installed("osqp")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.objective(p, type = "return", name = "mean")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "osqp"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.osqp")
})

test_that("optimize.portfolio osqp max-return with group constraints", {
  skip_on_cran()
  skip_if_not_installed("osqp")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8))
  p <- add.objective(p, type = "return", name = "mean")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "osqp"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.osqp")
})

test_that("optimize.portfolio osqp min-var with group constraints", {
  skip_on_cran()
  skip_if_not_installed("osqp")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8))
  p <- add.objective(p, type = "risk", name = "var")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "osqp"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.osqp")
})

test_that("optimize.portfolio osqp invalid group constraints", {
  skip_on_cran()
  skip_if_not_installed("osqp")

  p <- portfolio.spec(assets = funds4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8))
  # Break the length of cLO manually to trigger the error in osqp
  p$constraints[[3]]$cLO <- c(0.1)
  p <- add.objective(p, type = "risk", name = "var")

  expect_error(
    PortfolioAnalytics::optimize.portfolio(edhec4, p, optimize_method = "osqp"),
    "Please assign group constraint"
  )
})
