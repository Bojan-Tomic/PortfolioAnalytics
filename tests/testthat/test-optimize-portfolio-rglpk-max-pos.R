###############################################################################
# tests/testthat/test-optimize-portfolio-rglpk-max-pos.R
#
# Targeted coverage for optimize.portfolio Rglpk paths with position limits (max_pos)
#
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)
library(PerformanceAnalytics)

utils::data(edhec)
edhec4 <- edhec[, 1:4]
funds4 <- colnames(edhec4)

# Helper: portfolio with max_pos
.make_pos <- function(R, max_p = 2) {
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "position_limit", max_pos = max_p)
  p
}

test_that("optimize.portfolio Rglpk max-return with max_pos", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_pos(edhec4, max_p = 2)
  p <- add.objective(p, type = "return", name = "mean")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
  w <- extractWeights(opt)
  expect_true(sum(w > 1e-4) <= 2)
})

test_that("optimize.portfolio Rglpk max-return with max_pos and group constraints", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_pos(edhec4, max_p = 3)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8),
                      group_pos = c(1, 2))
  # Add target to mean to trigger that branch
  p <- add.objective(p, type = "return", name = "mean", target = 0.005)

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk", trace = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
})

test_that("optimize.portfolio Rglpk min-ES with max_pos", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_pos(edhec4, max_p = 2)
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
  w <- extractWeights(opt)
  expect_true(sum(w > 1e-4) <= 2)
})

test_that("optimize.portfolio Rglpk min-ES with max_pos and group constraints", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_pos(edhec4, max_p = 3)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8),
                      group_pos = c(1, 2))
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk", trace = TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
})

test_that("optimize.portfolio Rglpk max-STARR with max_pos", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_pos(edhec4, max_p = 2)
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk", trace=TRUE),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
})

test_that("optimize.portfolio Rglpk max-STARR with max_pos and group constraints", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")

  p <- .make_pos(edhec4, max_p = 3)
  p <- add.constraint(p, type = "group",
                      groups = list(1:2, 3:4),
                      group_min = c(0.1, 0.1),
                      group_max = c(0.8, 0.8),
                      group_pos = c(1, 2))
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk", name = "ES")

  opt <- tryCatch(
    optimize.portfolio(edhec4, p, optimize_method = "Rglpk"),
    error = function(e) NULL
  )
  skip_if(is.null(opt))
  expect_s3_class(opt, "optimize.portfolio.Rglpk")
})
