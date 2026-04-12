###############################################################################
# tests/testthat/test-optFUN-gaps2.R
#
# Targeted coverage for uncovered branches in R/optFUN.R
#
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)
library(PerformanceAnalytics)

utils::data(edhec)
edhec4 <- edhec[, 1:4]
funds4 <- colnames(edhec4)

# Helper function to mock constraints
.make_bare_constraints <- function() {
  list(
    assets = structure(rep(1/4, 4), names = funds4),
    min_sum = 0.99, max_sum = 1.01,
    min = rep(0, 4), max = rep(1, 4)
  )
}

test_that("gmv_opt handles cleanR in moments", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  m_list <- list(
    cleanR = edhec4,
    mean = rep(0, 4),
    var = cov(edhec4)
  )
  
  opt <- tryCatch(
    PortfolioAnalytics:::gmv_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                 lambda = 1, target = NA, lambda_hhi = NULL, 
                                 conc_groups = NULL, solver = "quadprog"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("gmv_opt handles factor exposures (B)", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  c_list$B <- matrix(1, nrow = 4, ncol = 2)
  c_list$lower <- c(-0.5, -0.5)
  c_list$upper <- c(0.5, 0.5)
  
  m_list <- list(
    mean = rep(0, 4),
    var = cov(edhec4)
  )
  
  opt <- tryCatch(
    PortfolioAnalytics:::gmv_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                 lambda = 1, target = NA, lambda_hhi = NULL, 
                                 conc_groups = NULL, solver = "quadprog"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("gmv_opt throws No solution found for invalid solver", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  m_list <- list(mean = rep(0, 4), var = cov(edhec4))
  
  expect_error(
    PortfolioAnalytics:::gmv_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                 lambda = 1, target = NA, lambda_hhi = NULL, 
                                 conc_groups = NULL, solver = "nonexistent_solver"),
    "ROI solver plugin 'ROI.plugin.nonexistent_solver' is not installed"
  )
})

test_that("maxret_opt handles cleanR", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  m_list <- list(
    cleanR = edhec4,
    mean = colMeans(edhec4)
  )
  
  opt <- tryCatch(
    PortfolioAnalytics:::maxret_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                    target = NA, solver = "glpk"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("maxret_opt handles missing cLO/cUP in group constraint", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  c_list$groups <- list(1:2, 3:4)
  # Purposefully omit cLO and cUP to hit lines 229-230
  
  m_list <- list(mean = colMeans(edhec4))
  
  opt <- tryCatch(
    PortfolioAnalytics:::maxret_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                    target = NA, solver = "glpk"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("maxret_opt throws No solution found for invalid solver", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  m_list <- list(mean = colMeans(edhec4))
  
  expect_error(
    PortfolioAnalytics:::maxret_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                    target = NA, solver = "nonexistent_solver"),
    "ROI solver plugin 'ROI.plugin.nonexistent_solver' is not installed"
  )
})

test_that("maxret_milp_opt handles cleanR, groups, and factor exposures", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  
  c_list <- .make_bare_constraints()
  c_list$max_pos <- 2
  c_list$groups <- list(1:2, 3:4)
  c_list$cLO <- c(0.1, 0.1)
  c_list$cUP <- c(0.8, 0.8)
  
  c_list$B <- matrix(1, nrow = 4, ncol = 2)
  c_list$lower <- c(-0.5, -0.5)
  c_list$upper <- c(0.5, 0.5)
  
  m_list <- list(
    cleanR = edhec4,
    mean = colMeans(edhec4)
  )
  
  opt <- tryCatch(
    PortfolioAnalytics:::maxret_milp_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                         target = NA, solver = "glpk"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("maxret_milp_opt handles missing cLO/cUP in group constraint", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  
  c_list <- .make_bare_constraints()
  c_list$max_pos <- 2
  c_list$groups <- list(1:2, 3:4)
  # Omit cLO/cUP
  
  m_list <- list(mean = colMeans(edhec4))
  
  opt <- tryCatch(
    PortfolioAnalytics:::maxret_milp_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                         target = NA, solver = "glpk"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("etl_opt handles cleanR, B, and infeasible", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  c_list$B <- matrix(1, nrow = 4, ncol = 1)
  c_list$lower <- 0.99
  c_list$upper <- 1.01
  
  m_list <- list(cleanR = edhec4, mean = colMeans(edhec4), ES = NA)
  
  opt <- tryCatch(
    PortfolioAnalytics:::etl_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                 target = NA, alpha = 0.05, solver = "glpk"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
  
  expect_error(
    PortfolioAnalytics:::etl_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                 target = NA, alpha = 0.05, solver = "nonexistent_solver"),
    "ROI solver plugin"
  )
})

test_that("etl_milp_opt handles cleanR, B, and infeasible", {
  skip_on_cran()
  skip_if_not_installed("Rglpk")
  
  c_list <- .make_bare_constraints()
  c_list$max_pos <- 2
  c_list$B <- matrix(1, nrow = 4, ncol = 1)
  c_list$lower <- 0.99
  c_list$upper <- 1.01
  
  m_list <- list(cleanR = edhec4, mean = colMeans(edhec4), ES = NA)
  
  opt <- tryCatch(
    PortfolioAnalytics:::etl_milp_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                      target = NA, alpha = 0.05, solver = "glpk"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
  
  expect_error(
    PortfolioAnalytics:::etl_milp_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                      target = NA, alpha = 0.05, solver = "nonexistent_solver"),
    "ROI solver plugin"
  )
})

test_that("max_sr_opt handles cleanR, B, and infeasible", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  c_list$B <- matrix(1, nrow = 4, ncol = 1)
  c_list$lower <- 0.99
  c_list$upper <- 1.01
  
  m_list <- list(cleanR = edhec4, mean = colMeans(edhec4), var = cov(edhec4), ES = NA)
  
  opt <- tryCatch(
    PortfolioAnalytics:::max_sr_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                    lambda_hhi=NULL, conc_groups=NULL, solver = "quadprog", control=NULL),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
  
  expect_error(
    PortfolioAnalytics:::max_sr_opt(R = edhec4, constraints = c_list, moments = m_list, 
                                    lambda_hhi=NULL, conc_groups=NULL, solver = "nonexistent_solver", control=NULL),
    "ROI solver plugin"
  )
})

test_that("gmv_opt_ptc handles cleanR, B, and infeasible", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  c_list$B <- matrix(1, nrow = 4, ncol = 1)
  c_list$lower <- 0.99
  c_list$upper <- 1.01
  c_list$ptc <- 0.01
  
  m_list <- list(cleanR = edhec4, mean = rep(0, 4), var = cov(edhec4), ES = NA)
  
  opt <- tryCatch(
    PortfolioAnalytics:::gmv_opt_ptc(R = edhec4, constraints = c_list, moments = m_list, 
                                     lambda = 1, target = NA, init_weights = rep(1/4, 4), solver = "quadprog"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
  
  expect_error(
    PortfolioAnalytics:::gmv_opt_ptc(R = edhec4, constraints = c_list, moments = m_list, 
                                     lambda = 1, target = NA, init_weights = rep(1/4, 4), solver = "nonexistent_solver"),
    "ROI solver plugin"
  )
})

test_that("gmv_opt_toc handles cleanR, B, and infeasible", {
  skip_on_cran()
  
  c_list <- .make_bare_constraints()
  c_list$B <- matrix(1, nrow = 4, ncol = 1)
  c_list$lower <- 0.99
  c_list$upper <- 1.01
  c_list$turnover_target <- 0.5
  
  m_list <- list(cleanR = edhec4, mean = rep(0, 4), var = cov(edhec4), ES = NA)
  
  opt <- tryCatch(
    PortfolioAnalytics:::gmv_opt_toc(R = edhec4, constraints = c_list, moments = m_list, 
                                     lambda = 1, target = NA, init_weights = rep(1/4, 4), solver = "quadprog"),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
  
  expect_error(
    PortfolioAnalytics:::gmv_opt_toc(R = edhec4, constraints = c_list, moments = m_list, 
                                     lambda = 1, target = NA, init_weights = rep(1/4, 4), solver = "nonexistent_solver"),
    "ROI solver plugin"
  )
})

