library(PortfolioAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")
skip_if_not_installed("DEoptim")

# --- Setup ---

utils::data(edhec)
R <- edhec[, 1:8]
funds <- colnames(R)

init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
init.portf <- add.objective(
  portfolio = init.portf, type = "risk", name = "ES",
  arguments = list(p = 0.925)
)

# ROI (LP) - deterministic, maximizes STARR (mean / ES)
maxSTARR.lo.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# Relax sum constraints for stochastic optimizers
init.portf$constraints[[1]]$min_sum <- 0.99
init.portf$constraints[[1]]$max_sum <- 1.01

maxSTARR.lo.RP <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "random",
  search_size = 2000, trace = TRUE
)

maxSTARR.lo.DE <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "DEoptim",
  search_size = 2000, trace = TRUE
)

# --- Tests: init.portf structure ---

test_that("init.portf contains mean as an objective", {
  expect_equal(init.portf$objectives[[1]]$name, "mean")
})

test_that("init.portf contains ES as an objective", {
  expect_equal(init.portf$objectives[[2]]$name, "ES")
})

test_that("init.portf contains ES as an objective with p=0.925", {
  expect_equal(init.portf$objectives[[2]]$arguments$p, 0.925)
})

# --- Tests: maxSTARR.lo.ROI (deterministic -- exact values) ---

test_that("maxSTARR.lo.ROI objective measure mean = 0.004933296", {
  expect_equal(as.numeric(extractObjectiveMeasures(maxSTARR.lo.ROI)$mean),
    0.004933296,
    tolerance = 1e-6
  )
})

test_that("maxSTARR.lo.ROI objective measure ES = 0.01369158", {
  expect_equal(as.numeric(extractObjectiveMeasures(maxSTARR.lo.ROI)$ES),
    0.01369158,
    tolerance = 1e-6
  )
})

# --- Tests: maxSTARR.lo.RP (stochastic -- numeric checks only) ---

test_that("maxSTARR.lo.RP objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxSTARR.lo.RP)$mean))
})

test_that("maxSTARR.lo.RP objective measure ES is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxSTARR.lo.RP)$ES))
})

# --- Tests: maxSTARR.lo.DE (stochastic -- numeric checks only) ---

test_that("maxSTARR.lo.DE objective measure mean is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxSTARR.lo.DE)$mean))
})

test_that("maxSTARR.lo.DE objective measure ES is numeric", {
  expect_true(is.numeric(extractObjectiveMeasures(maxSTARR.lo.DE)$ES))
})
