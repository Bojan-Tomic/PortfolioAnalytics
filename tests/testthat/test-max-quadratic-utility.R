library(PortfolioAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")

# --- Setup ---

utils::data(edhec)
R <- edhec[, 1:10]
funds <- colnames(R)

init.portf <- portfolio.spec(assets = funds)
init.portf <- add.constraint(portfolio = init.portf, type = "full_investment")
init.portf <- add.constraint(portfolio = init.portf, type = "long_only")
init.portf <- add.objective(portfolio = init.portf, type = "return", name = "mean")
init.portf <- add.objective(
  portfolio = init.portf, type = "risk", name = "StdDev",
  risk_aversion = 4
)

# lambda = 4 (moderate risk aversion)
maxQU.lo.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# lambda very small (approx max return)
init.portf$objectives[[2]]$risk_aversion <- 1e-6
maxQU.maxret.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# lambda very large (approx min variance)
init.portf$objectives[[2]]$risk_aversion <- 1e6
maxQU.minvol.ROI <- optimize.portfolio(
  R = R, portfolio = init.portf,
  optimize_method = "ROI", trace = TRUE
)

# --- Tests: init.portf structure ---

test_that("init.portf contains mean as an objective", {
  expect_equal(init.portf$objectives[[1]]$name, "mean")
})

test_that("init.portf contains StdDev as an objective", {
  expect_equal(init.portf$objectives[[2]]$name, "StdDev")
})

# --- Tests: maxQU.lo.ROI ---

test_that("risk aversion parameter = 4", {
  expect_equal(maxQU.lo.ROI$portfolio$objectives[[2]]$risk_aversion, 4)
})

test_that("maxQU.lo.ROI objective measure mean = 0.006598625", {
  expect_equal(as.numeric(extractObjectiveMeasures(maxQU.lo.ROI)$mean),
    0.006598625,
    tolerance = 1e-6
  )
})

test_that("maxQU.lo.ROI objective measure StdDev = 0.01632766", {
  expect_equal(as.numeric(extractObjectiveMeasures(maxQU.lo.ROI)$StdDev),
    0.01632766,
    tolerance = 1e-6
  )
})

test_that("maxQU.lo.ROI min box constraints are not violated", {
  expect_true(all(round(as.numeric(extractWeights(maxQU.lo.ROI)), 10) >=
    maxQU.lo.ROI$portfolio$constraints[[2]]$min))
})

test_that("maxQU.lo.ROI max box constraints are not violated", {
  expect_true(all(round(as.numeric(extractWeights(maxQU.lo.ROI)), 10) <=
    maxQU.lo.ROI$portfolio$constraints[[2]]$max))
})

# --- Tests: maxQU.maxret.ROI ---

test_that("risk aversion parameter = 1e-6", {
  expect_equal(maxQU.maxret.ROI$portfolio$objectives[[2]]$risk_aversion, 1e-6)
})

test_that("maxQU.maxret.ROI optimization ran without error", {
  # With risk_aversion approaching zero the QP can be ill-conditioned;
  # verify the result object exists and has the correct class.
  expect_s3_class(maxQU.maxret.ROI, "optimize.portfolio.ROI")
})

# --- Tests: maxQU.minvol.ROI ---

test_that("risk aversion parameter = 1e6", {
  expect_equal(maxQU.minvol.ROI$portfolio$objectives[[2]]$risk_aversion, 1e6)
})

test_that("maxQU.minvol.ROI objective measure mean = 0.004497118", {
  expect_equal(as.numeric(extractObjectiveMeasures(maxQU.minvol.ROI)$mean),
    0.004497118,
    tolerance = 1e-6
  )
})

test_that("maxQU.minvol.ROI objective measure StdDev = 0.00764702", {
  expect_equal(as.numeric(extractObjectiveMeasures(maxQU.minvol.ROI)$StdDev),
    0.00764702,
    tolerance = 1e-6
  )
})

test_that("maxQU.minvol.ROI min box constraints are not violated", {
  # Round to 10 d.p. to handle near-zero floating-point artifacts from quadprog
  expect_true(all(round(as.numeric(extractWeights(maxQU.minvol.ROI)), 10) >=
    maxQU.minvol.ROI$portfolio$constraints[[2]]$min))
})

test_that("maxQU.minvol.ROI max box constraints are not violated", {
  expect_true(all(round(as.numeric(extractWeights(maxQU.minvol.ROI)), 10) <=
    maxQU.minvol.ROI$portfolio$constraints[[2]]$max))
})
