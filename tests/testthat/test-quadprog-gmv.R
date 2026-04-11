library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(quadprog)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("quadprog")

# Test that PortfolioAnalytics with ROI.plugin.quadprog solutions equal
# direct quadprog solutions for global minimum variance (GMV) portfolios.

##### Data #####
utils::data(edhec)
R <- edhec[, 1:5]
funds <- colnames(R)
m <- ncol(R)

##### Parameters #####
portf <- portfolio.spec(funds)
portf <- add.constraint(portf, type = "full_investment")
portf <- add.constraint(portf, type = "box", min = -Inf, max = Inf)
portf <- add.objective(portf, type = "risk", name = "var")

# Quadratic part of objective function
objQ <- 2 * cov(R)

# Linear part of objective function
objL <- rep(0, m)

# Constraints matrix
Amat <- matrix(1, nrow = 1, ncol = m)

# Right-hand side of constraints
rhs <- 1


# ----- Unconstrained -----

# Solve optimization with quadprog
opt.qp <- solve.QP(Dmat = objQ, dvec = objL, Amat = t(Amat), bvec = rhs, meq = 1)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method = "quadprog")
weights <- as.numeric(extractWeights(opt.pa))

test_that("Unconstrained: PortfolioAnalytics and quadprog solution weights are equal", {
  expect_equal(weights, opt.qp$solution)
})

test_that("Unconstrained: PortfolioAnalytics and quadprog solution objective values are equal", {
  expect_equal(opt.pa$out, opt.qp$value)
})


# ----- Long Only -----

# Upper and lower bounds (i.e. box constraints)
lb <- rep(0, m)
ub <- rep(1, m)

Amat <- rbind(1, diag(m), -diag(m))
rhs <- c(1, lb, -ub)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with quadprog
opt.qp <- solve.QP(Dmat = objQ, dvec = objL, Amat = t(Amat), bvec = rhs, meq = 1)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method = "quadprog")
# Some of the optimal weights are slightly negative (-7.602276e-18), but they
# are essentially zero; round to 10 decimal places to handle the artifact.
weights <- round(as.numeric(extractWeights(opt.pa)), 10)

test_that("Long Only: PortfolioAnalytics and quadprog solution weights are equal", {
  expect_equal(weights, opt.qp$solution)
})

test_that("Long Only: PortfolioAnalytics bounds are respected", {
  expect_true(all(weights >= lb) & all(weights <= ub))
})

test_that("Long Only: quadprog bounds are respected", {
  expect_true(all(round(opt.qp$solution, 10) >= lb) & all(round(opt.qp$solution, 10) <= ub))
})

test_that("Long Only: PortfolioAnalytics and quadprog solution objective values are equal", {
  expect_equal(opt.pa$out, opt.qp$value)
})


# ----- Box (lb = 0.05, ub = 0.55) -----

# Upper and lower bounds (i.e. box constraints)
lb <- rep(0.05, m)
ub <- rep(0.55, m)

Amat <- rbind(1, diag(m), -diag(m))
rhs <- c(1, lb, -ub)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with quadprog
opt.qp <- solve.QP(Dmat = objQ, dvec = objL, Amat = t(Amat), bvec = rhs, meq = 1)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method = "quadprog")
# Round to 10 decimal places to handle floating-point artifacts at bounds
weights <- round(as.numeric(extractWeights(opt.pa)), 10)

test_that("Box: PortfolioAnalytics and quadprog solution weights are equal", {
  expect_equal(weights, opt.qp$solution)
})

test_that("Box: PortfolioAnalytics bounds are respected", {
  expect_true(all(weights >= lb) & all(weights <= ub))
})

test_that("Box: quadoprog bounds are respected", {
  expect_true(all(round(opt.qp$solution, 10) >= lb) & all(round(opt.qp$solution, 10) <= ub))
})

test_that("Box: PortfolioAnalytics and quadprog solution objective values are equal", {
  expect_equal(opt.pa$out, opt.qp$value)
})


# ----- Box with Shorting (lb = -0.05, ub = 0.55) -----

# Upper and lower bounds (i.e. box constraints)
lb <- rep(-0.05, m)
ub <- rep(0.55, m)

Amat <- rbind(1, diag(m), -diag(m))
rhs <- c(1, lb, -ub)

# Update box constraints in portfolio
portf$constraints[[2]]$min <- lb
portf$constraints[[2]]$max <- ub

# Solve optimization with quadprog
opt.qp <- solve.QP(Dmat = objQ, dvec = objL, Amat = t(Amat), bvec = rhs, meq = 1)

# Solve optimization with PortfolioAnalytics
opt.pa <- optimize.portfolio(R, portf, optimize_method = "quadprog")
# Round to 10 decimal places to handle floating-point artifacts at bounds
weights <- round(as.numeric(extractWeights(opt.pa)), 10)

test_that("Box with Shorting: PortfolioAnalytics and quadprog solution weights are equal", {
  expect_equal(weights, opt.qp$solution)
})

test_that("Box with Shorting: PortfolioAnalytics bounds are respected", {
  expect_true(all(weights >= lb) & all(weights <= ub))
})

test_that("Box with Shorting: quadprog bounds are respected", {
  expect_true(all(round(opt.qp$solution, 10) >= lb) & all(round(opt.qp$solution, 10) <= ub))
})

test_that("Box with Shorting: PortfolioAnalytics and quadprog solution objective values are equal", {
  expect_equal(opt.pa$out, opt.qp$value)
})
