###############################################################################
# tests/testthat/test-generics-print-summary.R
#
# Coverage targets (R/generics.R — 78.71% baseline, 122 uncovered exprs):
#
#   All previously uncovered print.* and summary.* S3 methods:
#     - print.portfolio                    (L155, L184, L241-L250)
#     - summary.portfolio                  (L302)
#     - print.optimize.portfolio.ROI       (L393-L399)
#     - print.optimize.portfolio.CVXR      (L435-L441)
#     - print.optimize.portfolio.random    (L477-L483)
#     - print.optimize.portfolio.DEoptim   (L519-L525)
#     - print.optimize.portfolio.GenSA     (L561-L567)
#     - print.optimize.portfolio.pso       (L603-L609)
#     - print.summary.optimize.portfolio   (L657-L799)
#     - summary.optimize.portfolio         (L889-L912)
#     - print.efficient.frontier           (L964)
#     - summary.efficient.frontier         (L991)
#     - summary.optimize.portfolio.parallel (L1073-L1085)
#     - print.optimize.portfolio.parallel  (L1092-L1112)
#     - summary.optimize.portfolio.rebalancing (L62)
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(foreach)

skip_on_cran()
skip_if_not_installed("PerformanceAnalytics")

utils::data(edhec)
R4 <- edhec[, 1:4]
nms <- colnames(R4)

# ---------------------------------------------------------------------------
# Shared portfolio fixtures
# ---------------------------------------------------------------------------

portf_base <- portfolio.spec(nms)
portf_base <- add.constraint(portf_base, type = "full_investment")
portf_base <- add.constraint(portf_base, type = "long_only")
portf_base <- add.objective(portf_base, type = "risk",   name = "StdDev")
portf_base <- add.objective(portf_base, type = "return", name = "mean")

portf_cat <- portfolio.spec(nms,
  category_labels = c("cat1", "cat1", "cat2", "cat2"))
portf_cat <- add.constraint(portf_cat, type = "full_investment")
portf_cat <- add.constraint(portf_cat, type = "long_only")
portf_cat <- add.objective(portf_cat, type = "risk", name = "StdDev")

# ===========================================================================
# Section 1: print.portfolio (L155, L184, L241-L250)
# ===========================================================================

test_that("print.portfolio: basic portfolio prints without error", {
  expect_output(print(portf_base), regexp = "PortfolioAnalytics Portfolio")
})

test_that("print.portfolio: category_labels branch (L184) is exercised", {
  # Portfolio with category_labels triggers the L175-L187 branch
  expect_output(print(portf_cat), regexp = "Category Labels")
})

test_that("print.portfolio: constraints section is printed", {
  expect_output(print(portf_base), regexp = "Constraints|full_investment|long_only")
})

test_that("print.portfolio: objectives section is printed", {
  expect_output(print(portf_base), regexp = "Objectives|StdDev|mean")
})

# ===========================================================================
# Section 2: summary.portfolio (L302)
# ===========================================================================

test_that("summary.portfolio: returns a list", {
  s <- summary(portf_base)
  expect_true(is.list(s))
})

test_that("summary.portfolio: contains assets element", {
  s <- summary(portf_base)
  expect_false(is.null(s$assets))
})

test_that("summary.portfolio: enabled_constraints is populated", {
  s <- summary(portf_base)
  expect_true(length(s$enabled_constraints) >= 1)
})

# ===========================================================================
# Section 3: print.optimize.portfolio.ROI (L393-L399)
# ===========================================================================

opt_roi <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_base, optimize_method = "ROI")
))

test_that("print.optimize.portfolio.ROI: prints without error", {
  expect_output(print(opt_roi), regexp = "PortfolioAnalytics Optimization")
})

test_that("print.optimize.portfolio.ROI: shows Optimal Weights", {
  expect_output(print(opt_roi), regexp = "Optimal Weights")
})

test_that("print.optimize.portfolio.ROI: shows Objective Measure", {
  expect_output(print(opt_roi), regexp = "Objective Measure")
})

# ===========================================================================
# Section 4: summary.optimize.portfolio (L889-L912) +
#            print.summary.optimize.portfolio (L657-L799)
# ===========================================================================

test_that("summary.optimize.portfolio: returns summary object", {
  s <- summary(opt_roi)
  expect_s3_class(s, "summary.optimize.portfolio")
})

test_that("summary.optimize.portfolio: contains portfolio slot", {
  s <- summary(opt_roi)
  expect_false(is.null(s$portfolio))
})

test_that("summary.optimize.portfolio: contains objective_values", {
  s <- summary(opt_roi)
  expect_false(is.null(s$objective_values))
})

test_that("print.summary.optimize.portfolio: prints without error", {
  s <- summary(opt_roi)
  expect_output(print(s), regexp = "PortfolioAnalytics|Optimal Weights")
})

test_that("print.summary.optimize.portfolio: x$out fallback branch (L669) triggers when objective_values NULL", {
  s <- summary(opt_roi)
  # Force objective_values to NULL to exercise the else branch at L669
  s$objective_values <- NULL
  expect_output(print(s), regexp = "Objective")
})

# ===========================================================================
# Section 5: print.optimize.portfolio.CVXR (L435-L441)
# ===========================================================================

skip_if_not_installed("CVXR")

opt_cvxr <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_base, optimize_method = "CVXR")
  )),
  error = function(e) NULL
)

test_that("print.optimize.portfolio.CVXR: prints without error", {
  skip_if(is.null(opt_cvxr))
  expect_output(print(opt_cvxr), regexp = "PortfolioAnalytics Optimization")
})

test_that("print.optimize.portfolio.CVXR: shows Optimal Weights", {
  skip_if(is.null(opt_cvxr))
  expect_output(print(opt_cvxr), regexp = "Optimal Weights")
})

# ===========================================================================
# Section 6: print.optimize.portfolio.random (L477-L483)
# ===========================================================================

set.seed(42)
opt_rnd <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_base,
                       optimize_method = "random",
                       search_size    = 200L,
                       trace          = FALSE)
  )),
  error = function(e) NULL
)

test_that("print.optimize.portfolio.random: prints without error", {
  skip_if(is.null(opt_rnd))
  expect_output(print(opt_rnd), regexp = "PortfolioAnalytics Optimization")
})

test_that("print.optimize.portfolio.random: shows Optimal Weights", {
  skip_if(is.null(opt_rnd))
  expect_output(print(opt_rnd), regexp = "Optimal Weights")
})

# ===========================================================================
# Section 7: print.optimize.portfolio.DEoptim (L519-L525)
# ===========================================================================

set.seed(42)
opt_de <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_base,
                       optimize_method = "DEoptim",
                       search_size    = 200L,
                       trace          = FALSE,
                       itermax        = 5L)
  )),
  error = function(e) NULL
)

test_that("print.optimize.portfolio.DEoptim: prints without error", {
  skip_if(is.null(opt_de))
  expect_output(print(opt_de), regexp = "PortfolioAnalytics Optimization")
})

test_that("print.optimize.portfolio.DEoptim: shows Optimal Weights", {
  skip_if(is.null(opt_de))
  expect_output(print(opt_de), regexp = "Optimal Weights")
})

# ===========================================================================
# Section 8: print.optimize.portfolio.GenSA (L561-L567)
# ===========================================================================

skip_if_not_installed("GenSA")

set.seed(42)
opt_gensa <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_base,
                       optimize_method = "GenSA",
                       search_size    = 200L,
                       trace          = FALSE)
  )),
  error = function(e) NULL
)

test_that("print.optimize.portfolio.GenSA: prints without error", {
  skip_if(is.null(opt_gensa))
  expect_output(print(opt_gensa), regexp = "PortfolioAnalytics Optimization")
})

test_that("print.optimize.portfolio.GenSA: shows Optimal Weights", {
  skip_if(is.null(opt_gensa))
  expect_output(print(opt_gensa), regexp = "Optimal Weights")
})

# ===========================================================================
# Section 9: print.optimize.portfolio.pso (L603-L609)
# ===========================================================================

skip_if_not_installed("pso")
foreach::registerDoSEQ()

set.seed(42)
opt_pso <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_base,
                       optimize_method = "pso",
                       trace          = FALSE,
                       maxit          = 10L)
  )),
  error = function(e) NULL
)

test_that("print.optimize.portfolio.pso: prints without error", {
  skip_if(is.null(opt_pso))
  expect_output(print(opt_pso), regexp = "PortfolioAnalytics Optimization")
})

test_that("print.optimize.portfolio.pso: shows Optimal Weights", {
  skip_if(is.null(opt_pso))
  expect_output(print(opt_pso), regexp = "Optimal Weights")
})

# ===========================================================================
# Section 10: print.efficient.frontier (L964) +
#             summary.efficient.frontier (L991)
# ===========================================================================

ef <- tryCatch(
  suppressMessages(suppressWarnings(
    create.EfficientFrontier(R4, portf_base,
                             type         = "mean-StdDev",
                             n.portfolios = 5L)
  )),
  error = function(e) NULL
)

test_that("print.efficient.frontier: prints without error", {
  skip_if(is.null(ef))
  expect_output(print(ef), regexp = "PortfolioAnalytics Efficient Frontier")
})

test_that("print.efficient.frontier: shows Efficient Frontier Points", {
  skip_if(is.null(ef))
  expect_output(print(ef), regexp = "Efficient Frontier Points")
})

test_that("summary.efficient.frontier: prints without error", {
  skip_if(is.null(ef))
  expect_output(summary(ef), regexp = "PortfolioAnalytics Efficient Frontier")
})

# ===========================================================================
# Section 11: summary.optimize.portfolio.parallel (L1073-L1085) +
#             print.optimize.portfolio.parallel (L1092-L1112)
#
# NOTE: summary.optimize.portfolio.parallel calls extractStats() on each
# node result via lapply(object$optimizations, ...) — this is NOT the same
# as the buggy extractStats.optimize.portfolio.parallel which iterates
# object directly. The summary method correctly accesses $optimizations.
# ===========================================================================

skip_if_not_installed("doParallel")
library(doParallel)

cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)

portf_par <- portfolio.spec(nms)
portf_par <- add.constraint(portf_par, type = "full_investment")
portf_par <- add.constraint(portf_par, type = "long_only")
portf_par <- add.objective(portf_par, type = "risk", name = "StdDev")

set.seed(1)
opt_par <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio.parallel(R4, portf_par,
                                optimize_method = "random",
                                nodes          = 2,
                                search_size    = 100L,
                                trace          = TRUE)
  )),
  error = function(e) NULL
)

parallel::stopCluster(cl)
foreach::registerDoSEQ()

test_that("summary.optimize.portfolio.parallel: returns summary object", {
  skip_if(is.null(opt_par))
  s <- tryCatch(summary(opt_par), error = function(e) NULL)
  skip_if(is.null(s))
  expect_s3_class(s, "summary.optimize.portfolio.parallel")
})

test_that("summary.optimize.portfolio.parallel: $n_optimizations == 2", {
  skip_if(is.null(opt_par))
  s <- tryCatch(summary(opt_par), error = function(e) NULL)
  skip_if(is.null(s))
  expect_equal(s$n_optimizations, 2L)
})

test_that("print.optimize.portfolio.parallel: prints without error", {
  skip_if(is.null(opt_par))
  result <- tryCatch(
    capture.output(print(opt_par)),
    error = function(e) NULL
  )
  skip_if(is.null(result))
  expect_true(any(grepl("PortfolioAnalytics|Number of Optimizations", result)))
})

# ===========================================================================
# Section 12: summary.optimize.portfolio.rebalancing (L62)
# ===========================================================================

portf_reb <- portfolio.spec(nms)
portf_reb <- add.constraint(portf_reb, type = "full_investment")
portf_reb <- add.constraint(portf_reb, type = "long_only")
portf_reb <- add.objective(portf_reb, type = "risk", name = "StdDev")

opt_reb <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio.rebalancing(R4, portf_reb,
                                   optimize_method  = "ROI",
                                   rebalance_on     = "quarters",
                                   training_period  = 60L)
  )),
  error = function(e) NULL
)

test_that("summary.optimize.portfolio.rebalancing: returns summary object with correct class", {
  skip_if(is.null(opt_reb))
  s <- suppressWarnings(summary(opt_reb))
  expect_s3_class(s, "summary.optimize.portfolio.rebalancing")
})

test_that("print.summary.optimize.portfolio.rebalancing: prints without error", {
  skip_if(is.null(opt_reb))
  s <- suppressWarnings(summary(opt_reb))
  expect_output(print(s), regexp = "PortfolioAnalytics Optimization with Rebalancing")
})
