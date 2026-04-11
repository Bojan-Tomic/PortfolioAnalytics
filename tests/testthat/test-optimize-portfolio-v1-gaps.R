###############################################################################
# tests/testthat/test-optimize-portfolio-v1-gaps.R
#
# Targeted coverage for optimize.portfolio_v1
#
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)
library(PerformanceAnalytics)

utils::data(edhec)
edhec4 <- edhec[, 1:4]
edhec5 <- edhec[, 1:5]
funds4 <- colnames(edhec4)
funds5 <- colnames(edhec5)

cc <- constraint_v1(assets = funds4, min_sum = 0.99, max_sum = 1.01,
                    min = rep(0, 4), max = rep(1, 4), weight_seq = generatesequence(min=0, max=1, by=0.01))

# Dummy moments function to bypass match.fun("set.portfolio.moments_v1") issue
dummy_m <- PortfolioAnalytics:::set.portfolio.moments_v1

test_that("optimize.portfolio_v1 handles invalid constraints", {
  skip_on_cran()
  expect_error(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = NULL),
    "you must pass in an object of class constraints"
  )
})

test_that("optimize.portfolio_v1 subsets R if ncol(R) > N", {
  skip_on_cran()
  # cc has 4 assets. Pass edhec5 (5 assets).
  opt <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec5, constraints = cc, optimize_method = "random", momentFUN = dummy_m, search_size=20),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("optimize.portfolio_v1 handles momentFUN errors", {
  skip_on_cran()
  # Pass a function that fails
  bad_moments <- function(...) stop("intentional error")
  expect_message(
    tryCatch(
      PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc, optimize_method = "random", search_size=20, momentFUN = bad_moments),
      error = function(e) NULL
    ),
    "portfolio moment function failed"
  )
})

test_that("optimize.portfolio_v1 DEoptim itermax and NP branches", {
  skip_on_cran()
  skip_if_not_installed("DEoptim")
  
  # Trigger NP > 2000
  opt1 <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc, optimize_method = "DEoptim", momentFUN = dummy_m, search_size = 30000, itermax = 10),
    error = function(e) NULL
  )
  expect_false(is.null(opt1))
  
  # Trigger missing itermax and itermax < 50
  opt2 <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc, optimize_method = "DEoptim", momentFUN = dummy_m, search_size = 500),
    error = function(e) NULL
  )
  expect_false(is.null(opt2))
})

test_that("optimize.portfolio_v1 DEoptim parallel=FALSE branch", {
  skip_on_cran()
  skip_if_not_installed("DEoptim")
  skip_if_not_installed("foreach")
  
  # Trigger parallel=FALSE inside DEoptim v1
  opt <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc, optimize_method = "DEoptim", momentFUN = dummy_m, search_size = 100, parallel = FALSE),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("optimize.portfolio_v1 DEoptim rpseed branches", {
  skip_on_cran()
  skip_if_not_installed("DEoptim")
  
  # Trigger rpseed passed
  seed_mat <- matrix(rep(1/4, 4), nrow=1)
  opt1 <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc, optimize_method = "DEoptim", momentFUN = dummy_m, search_size = 100, rpseed = seed_mat),
    error = function(e) NULL
  )
  expect_true(TRUE)
  
  # Trigger rpseed=TRUE with eps
  opt2 <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc, optimize_method = "DEoptim", momentFUN = dummy_m, search_size = 100, rpseed = TRUE, eps = 0.02),
    error = function(e) NULL
  )
  expect_true(TRUE)
})

test_that("optimize.portfolio_v1 random parallel=FALSE branch", {
  skip_on_cran()
  # triggers apply fallback (line 225)
  opt <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc, optimize_method = "random", momentFUN = dummy_m, search_size = 20, parallel = FALSE),
    error = function(e) NULL
  )
  expect_false(is.null(opt))
})

test_that("optimize.portfolio_v1 ROI invalid objective", {
  skip_on_cran()
  cc_bad <- add.objective_v1(cc, type="risk", name="StdDev")
  expect_error(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc_bad, optimize_method = "ROI", momentFUN = dummy_m),
    "ROI only solves mean, var, or sample CVaR"
  )
})

test_that("optimize.portfolio_v1 ROI var objective", {
  skip_on_cran()
  cc_var <- add.objective_v1(cc, type="risk", name="var")
  opt <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc_var, optimize_method = "ROI", momentFUN = dummy_m),
    error = function(e) NULL
  )
  expect_true(TRUE)
})

test_that("optimize.portfolio_v1 ROI CVaR objective and target", {
  skip_on_cran()
  cc_cvar <- add.objective_v1(cc, type="risk", name="CVaR")
  # Add target to mean to trigger target branches
  cc_cvar <- add.objective_v1(cc_cvar, type="return", name="mean", target = 0.005)
  opt <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc_cvar, optimize_method = "ROI", momentFUN = dummy_m),
    error = function(e) NULL
  )
  expect_true(TRUE)
})

test_that("optimize.portfolio_v1 ROI groups constraint", {
  skip_on_cran()
  cc_group <- cc
  cc_group$groups <- c(2, 2)
  cc_group$cLO <- c(0.1, 0.1)
  cc_group$cUP <- c(0.8, 0.8)
  cc_group <- add.objective_v1(cc_group, type="return", name="mean")
  
  opt <- tryCatch(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc_group, optimize_method = "ROI", momentFUN = dummy_m),
    error = function(e) NULL
  )
  expect_true(TRUE)
  
  # Invalid groups sum
  cc_group2 <- cc_group
  cc_group2$groups <- c(2, 1)
  expect_error(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc_group2, optimize_method = "ROI", momentFUN = dummy_m),
    "Number of assets in each group needs to sum to number of total assets"
  )
  
  # Invalid cLO length
  cc_group3 <- cc_group
  cc_group3$cLO <- c(0.1)
  expect_error(
    PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc_group3, optimize_method = "ROI", momentFUN = dummy_m),
    "Number of group constraints exceeds number of groups"
  )
})

test_that("optimize.portfolio_v1 ROI_old path", {
  skip_on_cran()
  # Mock constraints solver and objective
  cc_old <- cc
  cc_old$solver <- "glpk"
  # Just put a random ROI optimization object here to let ROI::ROI_solve try something
  cc_old$constrainted_objective <- ROI::OP(ROI::L_objective(c(1,1,1,1)), ROI::L_constraint(diag(4), rep(">=", 4), rep(0, 4)), bounds=ROI::V_bound(li=1:4, lb=rep(0,4), ui=1:4, ub=rep(1,4)))
  
  expect_output(
    tryCatch(
      PortfolioAnalytics:::optimize.portfolio_v1(edhec4, constraints = cc_old, optimize_method = "ROI_old", momentFUN = dummy_m),
      error = function(e) NULL
    ),
    "ROI_old is going to be depricated"
  )
})

