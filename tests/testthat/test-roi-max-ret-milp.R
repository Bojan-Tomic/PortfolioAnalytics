# maximum return with position limit constraints
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(Rglpk)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")
skip_if_not_installed("Rglpk")

utils::data(edhec)
R <- edhec[, 1:5]
m <- ncol(R)

constraints <- list()
constraints$min_sum <- 0.99
constraints$max_sum <- 1.01
constraints$min <- rep(0.2, m)
constraints$max <- rep(1, m)
constraints$max_pos <- 3

moments <- list()
moments$mu <- colMeans(R)
moments$mean <- colMeans(R)

target <- NA

max_pos <- constraints$max_pos
min_pos <- 2

# Number of assets
N <- ncol(R)

# Upper and lower bounds on weights
LB <- as.numeric(constraints$min)
UB <- as.numeric(constraints$max)

##### ROI #####

# Check for target return
if (!is.na(target)) {
  # We have a target
  targetcon <- rbind(
    c(moments$mean, rep(0, N)),
    c(-moments$mean, rep(0, N))
  )
  targetdir <- c("<=", "==")
  targetrhs <- c(Inf, -target)
} else {
  # No target specified, just maximize
  targetcon <- NULL
  targetdir <- NULL
  targetrhs <- NULL
}

# weight_sum constraint
Amat <- rbind(
  c(rep(1, N), rep(0, N)),
  c(rep(1, N), rep(0, N))
)

# Target return constraint
Amat <- rbind(Amat, targetcon)

# Bounds and position limit constraints
Amat <- rbind(Amat, cbind(-diag(N), diag(LB)))
Amat <- rbind(Amat, cbind(diag(N), -diag(UB)))
Amat <- rbind(Amat, c(rep(0, N), rep(-1, N)))
Amat <- rbind(Amat, c(rep(0, N), rep(1, N)))

dir <- c("<=", ">=", targetdir, rep("<=", 2 * N), "<=", "<=")
rhs <- c(1, 1, targetrhs, rep(0, 2 * N), -min_pos, max_pos)

# Only seems to work if I do not specify bounds
bnds <- NULL

# Set up the types vector with continuous and binary variables
types <- c(rep("C", N), rep("B", N))

# Set up the linear objective to maximize mean return
ROI_objective <- L_objective(L = c(-moments$mean, rep(0, N)))

# Set up the optimization problem and solve
opt.prob <- OP(
  objective = ROI_objective,
  constraints = L_constraint(L = Amat, dir = dir, rhs = rhs),
  bounds = bnds, types = types
)
roi.result <- ROI_solve(x = opt.prob, solver = "glpk")

##### Rglpk #####

objL <- c(-moments$mean, rep(0, N))

result <- Rglpk_solve_LP(
  obj = objL, mat = Amat, dir = dir, rhs = rhs,
  bounds = bnds, types = types
)

test_that("Objective values are equal", {
  expect_equal(roi.result$objval, result$optimum)
})

test_that("Solutions (optimal weights) are equal", {
  expect_equal(roi.result$solution[1:m], result$solution[1:m])
})
