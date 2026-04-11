###############################################################################
# tests/testthat/test-chart-concentration.R
#
# Tests for chart.Concentration covering missing return/risk columns
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()
library(PortfolioAnalytics)

data(edhec)
edhec4 <- edhec[, 1:4]

p <- portfolio.spec(assets = colnames(edhec4))
p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p <- add.constraint(p, type = "box", min = 0, max = 1)
p <- add.objective(p, type = "risk", name = "StdDev")

# Optimize to generate an object that DOES NOT have a "mean" objective
opt <- tryCatch(
  optimize.portfolio(edhec4, p, optimize_method = "random", search_size=200, trace=TRUE),
  error = function(e) NULL
)

test_that("chart.Concentration works when return.col is not in objective_measures", {
  skip_if(is.null(opt))
  expect_no_error({
    png(tempfile(fileext = ".png"))
    on.exit(dev.off(), add = TRUE)
    # Use "mean" as return.col even though it wasn't an objective
    expect_warning(chart.Concentration(opt, return.col = "mean", risk.col = "StdDev"))
  })
})

# Create an object with return but no risk objective
p2 <- portfolio.spec(assets = colnames(edhec4))
p2 <- add.constraint(p2, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p2 <- add.constraint(p2, type = "box", min = 0, max = 1)
p2 <- add.objective(p2, type = "return", name = "mean")

opt2 <- tryCatch(
  optimize.portfolio(edhec4, p2, optimize_method = "random", search_size=200, trace=TRUE),
  error = function(e) NULL
)

test_that("chart.Concentration works when risk.col is not in objective_measures", {
  skip_if(is.null(opt2))
  expect_no_error({
    png(tempfile(fileext = ".png"))
    on.exit(dev.off(), add = TRUE)
    # Use "StdDev" as risk.col even though it wasn't an objective
    expect_warning(chart.Concentration(opt2, return.col = "mean", risk.col = "StdDev"))
  })
})
