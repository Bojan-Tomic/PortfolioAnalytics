###############################################################################
# tests/testthat/test-issue-10-helpful-plugin-error.R
#
# Regression tests for GitHub issue #10:
#   "The first example of the vignette doesn't work reproducibly"
#
# Root cause: stopifnot(paste0("package:", plugin) %in% search() || ...)
# produces an opaque error like "is not TRUE" without naming the missing
# package.
#
# Fix: Replace all 8 stopifnot() checks in optFUN.R with explicit stop()
# calls that name the plugin and provide install instructions.
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(testthat)
library(PortfolioAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")

# ---------------------------------------------------------------------------
# Error messages must name the missing plugin (#10)
# ---------------------------------------------------------------------------

test_that("#10 gmv_opt: missing plugin produces helpful error naming the package", {
  library(ROI)
  data(edhec)
  R <- edhec[, 1:4]
  ps <- portfolio.spec(colnames(R))
  ps <- add.constraint(ps, type = "full_investment")
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "risk", name = "StdDev")

  constraints <- PortfolioAnalytics:::get_constraints(ps)
  moments <- PortfolioAnalytics:::set.portfolio.moments(R, ps)
  moments$mean <- as.vector(moments$mu)
  moments$var  <- moments$sigma

  err <- tryCatch(
    PortfolioAnalytics:::gmv_opt(
      R = R, constraints = constraints, moments = moments,
      lambda = 1, target = NA, lambda_hhi = 0, conc_groups = NULL,
      solver = "nonexistent_plugin_xyz"
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "ROI.plugin.nonexistent_plugin_xyz",
               info = "error must name the missing plugin package")
  expect_match(err, "install.packages",
               info = "error must suggest install.packages()")
})

test_that("#10 maxret_opt: missing plugin produces helpful error naming the package", {
  library(ROI)
  data(edhec)
  R <- edhec[, 1:4]
  ps <- portfolio.spec(colnames(R))
  ps <- add.constraint(ps, type = "full_investment")
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "return", name = "mean")

  constraints <- PortfolioAnalytics:::get_constraints(ps)
  moments <- PortfolioAnalytics:::set.portfolio.moments(R, ps)
  moments$mean <- as.vector(moments$mu)
  moments$var  <- moments$sigma

  err <- tryCatch(
    PortfolioAnalytics:::maxret_opt(
      R = R, moments = moments, constraints = constraints,
      target = NA, solver = "nonexistent_plugin_xyz", control = list()
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "ROI.plugin.nonexistent_plugin_xyz")
  expect_match(err, "install.packages")
})

test_that("#10 error message does NOT appear with correctly installed plugin", {
  skip_if_not_installed("ROI.plugin.quadprog")
  library(ROI)
  library(ROI.plugin.quadprog)

  data(edhec)
  R <- edhec[, 1:4]
  ps <- portfolio.spec(colnames(R))
  ps <- add.constraint(ps, type = "full_investment")
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "risk", name = "StdDev")

  # Should succeed without errors about missing plugins
  expect_no_error(
    optimize.portfolio(R, ps, optimize_method = "ROI", trace = FALSE)
  )
})
