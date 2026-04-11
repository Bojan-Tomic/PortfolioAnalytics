###############################################################################
# tests/testthat/test-objective-constructors.R
#
# Tests for the lower-level objective constructors and helpers in R/objective.R
# that are NOT already exercised by test-objectives.R:
#
#   is.objective()
#   turnover_objective()
#   minmax_objective()
#   quadratic_utility_objective()
#   weight_concentration_objective()  (edge-case branches)
#   insert_objectives()
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)

skip_on_cran()

# ---------------------------------------------------------------------------
# is.objective()
# ---------------------------------------------------------------------------

test_that("is.objective() returns TRUE for a return_objective", {
  p <- portfolio.spec(assets = 4)
  p <- add.objective(p, type = "return", name = "mean")
  expect_true(is.objective(p$objectives[[1]]))
})

test_that("is.objective() returns FALSE for a plain list", {
  expect_false(is.objective(list(name = "foo")))
})

test_that("is.objective() returns FALSE for NULL", {
  expect_false(is.objective(NULL))
})

test_that("is.objective() returns FALSE for a numeric scalar", {
  expect_false(is.objective(42))
})


# ---------------------------------------------------------------------------
# turnover_objective()
# ---------------------------------------------------------------------------

test_that("turnover_objective() returns correct class", {
  obj <- turnover_objective(name = "turnover")
  expect_s3_class(obj, "turnover_objective")
  expect_s3_class(obj, "objective")
})

test_that("turnover_objective() stores NULL target by default", {
  obj <- turnover_objective(name = "turnover")
  expect_null(obj$target)
})

test_that("turnover_objective() stores a scalar target when supplied", {
  obj <- turnover_objective(name = "turnover", target = 0.2)
  expect_equal(obj$target, 0.2)
})

test_that("turnover_objective() stores the name correctly", {
  obj <- turnover_objective(name = "my_turnover")
  expect_equal(obj$name, "my_turnover")
})

test_that("turnover_objective() is enabled by default", {
  obj <- turnover_objective(name = "turnover")
  expect_true(obj$enabled)
})

test_that("add.objective with type='turnover' creates a turnover_objective", {
  p <- portfolio.spec(assets = 4)
  p <- add.objective(p, type = "turnover", name = "turnover", turnover_target = 0.1)
  expect_s3_class(p$objectives[[1]], "turnover_objective")
})


# ---------------------------------------------------------------------------
# minmax_objective()
# ---------------------------------------------------------------------------

test_that("minmax_objective() returns correct class", {
  obj <- minmax_objective(name = "mean", min = 0.005, max = 0.02)
  expect_s3_class(obj, "minmax_objective")
  expect_s3_class(obj, "objective")
})

test_that("minmax_objective() stores min and max correctly", {
  obj <- minmax_objective(name = "mean", min = 0.005, max = 0.02)
  expect_equal(obj$min, 0.005)
  expect_equal(obj$max, 0.02)
})

test_that("minmax_objective() stores NULL target by default", {
  obj <- minmax_objective(name = "mean", min = 0.005, max = 0.02)
  expect_null(obj$target)
})

test_that("minmax_objective() stores a target when supplied", {
  obj <- minmax_objective(name = "mean", target = 0.01, min = 0.005, max = 0.02)
  expect_equal(obj$target, 0.01)
})

test_that("minmax_objective() is enabled by default", {
  obj <- minmax_objective(name = "mean", min = 0.005, max = 0.02)
  expect_true(obj$enabled)
})


# ---------------------------------------------------------------------------
# quadratic_utility_objective()
# ---------------------------------------------------------------------------

test_that("quadratic_utility_objective() returns a list of length 2", {
  qu <- quadratic_utility_objective(risk_aversion = 2)
  expect_true(is.list(qu))
  expect_equal(length(qu), 2L)
})

test_that("quadratic_utility_objective() first element is a return_objective", {
  qu <- quadratic_utility_objective(risk_aversion = 2)
  expect_s3_class(qu[[1]], "return_objective")
})

test_that("quadratic_utility_objective() second element is a portfolio_risk_objective", {
  qu <- quadratic_utility_objective(risk_aversion = 2)
  expect_s3_class(qu[[2]], "portfolio_risk_objective")
})

test_that("quadratic_utility_objective() passes risk_aversion to the risk objective", {
  qu <- quadratic_utility_objective(risk_aversion = 5)
  expect_equal(qu[[2]]$risk_aversion, 5)
})

test_that("quadratic_utility_objective() passes target to the return objective", {
  qu <- quadratic_utility_objective(risk_aversion = 1, target = 0.01)
  expect_equal(qu[[1]]$target, 0.01)
})

test_that("quadratic_utility_objective() uses risk_aversion=1 as default", {
  qu <- quadratic_utility_objective()
  expect_equal(qu[[2]]$risk_aversion, 1)
})


# ---------------------------------------------------------------------------
# weight_concentration_objective() — edge-case branches
# ---------------------------------------------------------------------------

test_that("weight_concentration_objective() with scalar conc_aversion and no groups", {
  obj <- weight_concentration_objective(name = "HHI", conc_aversion = 0.1)
  expect_s3_class(obj, "weight_concentration_objective")
  expect_equal(obj$conc_aversion, 0.1)
  expect_null(obj$conc_groups)
})

test_that("weight_concentration_objective() errors if conc_aversion is vector and conc_groups is NULL", {
  expect_error(
    weight_concentration_objective(name = "HHI", conc_aversion = c(0.1, 0.2)),
    regexp = "scalar"
  )
})

test_that("weight_concentration_objective() with conc_groups replicates scalar conc_aversion", {
  obj <- weight_concentration_objective(
    name = "HHI",
    conc_aversion = 0.1,
    conc_groups = list(c(1, 2), c(3, 4))
  )
  expect_equal(obj$conc_aversion, c(0.1, 0.1))
})

test_that("weight_concentration_objective() errors on length mismatch between conc_aversion and conc_groups", {
  expect_error(
    weight_concentration_objective(
      name = "HHI",
      conc_aversion = c(0.1, 0.2, 0.3),
      conc_groups = list(c(1, 2), c(3, 4))
    )
  )
})

test_that("weight_concentration_objective() errors if conc_groups is not a list", {
  expect_error(
    weight_concentration_objective(
      name = "HHI",
      conc_aversion = 0.1,
      conc_groups = c(1, 2, 3, 4)
    )
  )
})


# ---------------------------------------------------------------------------
# insert_objectives()
# ---------------------------------------------------------------------------

test_that("insert_objectives() replaces the objectives slot", {
  p <- portfolio.spec(assets = 4)
  p <- add.objective(p, type = "risk", name = "StdDev")
  # Build a fresh objective list
  new_obj <- list(return_objective(name = "mean"))
  result <- insert_objectives(p, new_obj)
  expect_equal(length(result$objectives), 1L)
  expect_s3_class(result$objectives[[1]], "return_objective")
})

test_that("insert_objectives() returns a portfolio object", {
  p <- portfolio.spec(assets = 4)
  obj_list <- list(return_objective(name = "mean"))
  result <- insert_objectives(p, obj_list)
  expect_true(is.portfolio(result))
})

test_that("insert_objectives() errors on non-portfolio input", {
  expect_error(insert_objectives(list(), list(return_objective(name = "mean"))))
})

test_that("insert_objectives() errors when objectives is not a list", {
  p <- portfolio.spec(assets = 4)
  obj <- return_objective(name = "mean")
  expect_error(insert_objectives(p, obj))
})

test_that("insert_objectives() errors when a list element is not an objective", {
  p <- portfolio.spec(assets = 4)
  expect_error(insert_objectives(p, list(list(name = "not_an_obj"))))
})
