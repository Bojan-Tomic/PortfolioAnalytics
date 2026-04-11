skip_on_cran()
library(PortfolioAnalytics)

data(edhec)
edhec4 <- edhec[, 1:4]

p <- portfolio.spec(assets = colnames(edhec4))
p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
p <- add.constraint(p, type = "box", min = 0, max = 1)
# Only add ONE objective
p <- add.objective(p, type = "risk", name = "StdDev")

opt_rp <- tryCatch(optimize.portfolio(edhec4, p, optimize_method = "random", search_size=200, trace=TRUE), error = function(e) NULL)

opt_de <- tryCatch({
    if(!requireNamespace("DEoptim", quietly=TRUE)) stop()
    optimize.portfolio(edhec4, p, optimize_method = "DEoptim", search_size=200, itermax=10, trace=TRUE)
}, error = function(e) NULL)

opt_pso <- tryCatch({
    if(!requireNamespace("pso", quietly=TRUE)) stop()
    optimize.portfolio(edhec4, p, optimize_method = "pso", search_size=200, maxit=10, trace=TRUE)
}, error = function(e) NULL)


test_that("chart.RiskReward.random handles missing return.col", {
  skip_if(is.null(opt_rp))
  expect_no_error({
    png(tempfile(fileext = ".png"))
    on.exit(dev.off(), add = TRUE)
    expect_warning(chart.RiskReward(opt_rp, return.col = "mean", risk.col = "StdDev"))
  })
})

test_that("chart.RiskReward.DEoptim handles missing return.col", {
  skip_if(is.null(opt_de))
  expect_no_error({
    png(tempfile(fileext = ".png"))
    on.exit(dev.off(), add = TRUE)
    expect_warning(chart.RiskReward(opt_de, return.col = "mean", risk.col = "StdDev"))
  })
})

test_that("chart.RiskReward.pso handles missing return.col", {
  skip_if(is.null(opt_pso))
  expect_no_error({
    png(tempfile(fileext = ".png"))
    on.exit(dev.off(), add = TRUE)
    expect_warning(chart.RiskReward(opt_pso, return.col = "mean", risk.col = "StdDev"))
  })
})
