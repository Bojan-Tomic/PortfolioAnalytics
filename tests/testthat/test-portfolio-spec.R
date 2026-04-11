###############################################################################
# tests/testthat/test-portfolio-spec.R
#
# Tests for portfolio.spec(), is.portfolio(), print/summary methods on
# portfolio objects, regime.portfolios(), and combine.portfolios().
#
# Source files covered (primary):
#   R/portfolio.R      — portfolio.spec(), is.portfolio(), regime.portfolios()
#   R/generics.R       — print.portfolio() branches, summary.portfolio()
#                        branches, print.portfolio.list(),
#                        print.regime.portfolios()
#   R/utility.combine.R — combine.portfolios() (partial; full coverage in
#                         test-combine-utilities.R)
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()

utils::data(edhec)

# ---------------------------------------------------------------------------
# 1. portfolio.spec() — three input forms
# ---------------------------------------------------------------------------

test_that("portfolio.spec() with integer N creates auto-named equal-weight portfolio", {
  p <- portfolio.spec(assets = 5)
  expect_s3_class(p, "portfolio.spec")
  expect_s3_class(p, "portfolio")
  expect_equal(length(p$assets), 5)
  expect_equal(as.numeric(p$assets), rep(0.2, 5))
  expect_equal(names(p$assets), paste0("Asset.", 1:5))
  expect_equal(p$name, "portfolio")
  expect_equal(length(p$constraints), 0)
  expect_equal(length(p$objectives), 0)
})

test_that("portfolio.spec() with character vector preserves asset names", {
  nms <- c("AAPL", "GOOG", "MSFT")
  p <- portfolio.spec(assets = nms)
  expect_s3_class(p, "portfolio.spec")
  expect_equal(names(p$assets), nms)
  expect_equal(as.numeric(p$assets), rep(1 / 3, 3))
})

test_that("portfolio.spec() with named numeric vector uses provided weights", {
  w <- c(A = 0.5, B = 0.3, C = 0.2)
  p <- portfolio.spec(assets = w)
  expect_s3_class(p, "portfolio.spec")
  expect_equal(p$assets, w)
  expect_equal(names(p$assets), c("A", "B", "C"))
})

test_that("portfolio.spec() custom name is stored", {
  p <- portfolio.spec(assets = 3, name = "my_portf")
  expect_equal(p$name, "my_portf")
})

# ---------------------------------------------------------------------------
# 2. portfolio.spec() — optional arguments
# ---------------------------------------------------------------------------

test_that("portfolio.spec() with weight_seq stores the sequence", {
  ws <- generatesequence(min = 0.01, max = 1, by = 0.01)
  p <- portfolio.spec(assets = 4, weight_seq = ws)
  expect_equal(p$weight_seq, ws)
})

test_that("portfolio.spec() with category_labels stores them as a named list", {
  p <- portfolio.spec(
    assets          = c("A", "B", "C", "D"),
    category_labels = c("equity", "equity", "fixed", "fixed")
  )
  expect_true(is.list(p$category_labels))
  expect_equal(sort(names(p$category_labels)), c("equity", "fixed"))
  expect_equal(sort(p$category_labels$equity), c(1L, 2L))
  expect_equal(sort(p$category_labels$fixed), c(3L, 4L))
})

# ---------------------------------------------------------------------------
# 3. portfolio.spec() — error handling
# ---------------------------------------------------------------------------

test_that("portfolio.spec() errors on NULL assets", {
  expect_error(portfolio.spec(assets = NULL))
})

test_that("portfolio.spec() errors when category_labels length != number of assets", {
  expect_error(
    portfolio.spec(
      assets = c("A", "B", "C"),
      category_labels = c("equity", "fixed")
    )
  )
})

test_that("portfolio.spec() errors when category_labels is not character", {
  expect_error(
    portfolio.spec(
      assets = c("A", "B", "C"),
      category_labels = c(1, 2, 3)
    )
  )
})

# ---------------------------------------------------------------------------
# 4. is.portfolio()
# ---------------------------------------------------------------------------

test_that("is.portfolio() returns TRUE for a portfolio.spec object", {
  p <- portfolio.spec(assets = 4)
  expect_true(is.portfolio(p))
})

test_that("is.portfolio() returns FALSE for non-portfolio objects", {
  expect_false(is.portfolio(list(a = 1)))
  expect_false(is.portfolio(42))
  expect_false(is.portfolio("portfolio"))
  expect_false(is.portfolio(NULL))
})

# ---------------------------------------------------------------------------
# 5. add.constraint() and add.objective() — basic increments
# (deeper constraint tests live in test-constraints.R)
# ---------------------------------------------------------------------------

test_that("add.constraint() increments constraint count and returns portfolio", {
  p <- portfolio.spec(assets = 4)
  expect_equal(length(p$constraints), 0)
  p <- add.constraint(p, type = "full_investment")
  expect_s3_class(p, "portfolio")
  expect_equal(length(p$constraints), 1)
  p <- add.constraint(p, type = "long_only")
  expect_equal(length(p$constraints), 2)
})

test_that("add.objective() increments objective count and returns portfolio", {
  p <- portfolio.spec(assets = 4)
  expect_equal(length(p$objectives), 0)
  p <- add.objective(p, type = "risk", name = "StdDev")
  expect_s3_class(p, "portfolio")
  expect_equal(length(p$objectives), 1)
  p <- add.objective(p, type = "return", name = "mean")
  expect_equal(length(p$objectives), 2)
})

# ---------------------------------------------------------------------------
# 6. print.portfolio() — branch coverage for generics.R
#    test-constraints.R already covers: enabled constraints (full_investment,
#    box long-only, group, turnover, diversification, position_limit, return,
#    factor_exposure), no disabled constraints, no objectives.
#    Here we cover the remaining branches.
# ---------------------------------------------------------------------------

test_that("print.portfolio() handles portfolio with > 10 assets", {
  p <- portfolio.spec(assets = 13)
  expect_output(print(p), "More than 10 assets")
})

test_that("print.portfolio() handles portfolio with category_labels", {
  p <- portfolio.spec(
    assets          = c("A", "B", "C", "D"),
    category_labels = c("equity", "equity", "fixed", "fixed")
  )
  expect_output(print(p), "Category Labels")
})

test_that("print.portfolio() handles box (unconstrained) label", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = -Inf, max = Inf)
  expect_output(print(p), "unconstrained")
})

test_that("print.portfolio() handles box (with shorting) label", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = -0.2, max = 0.8)
  expect_output(print(p), "with shorting")
})

test_that("print.portfolio() shows both enabled and disabled constraint sections", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  # Manually disable the second constraint to trigger the disabled branch
  p$constraints[[2]]$enabled <- FALSE
  expect_output(print(p), "Enabled constraint")
  expect_output(print(p), "Disabled constraint")
})

test_that("print.portfolio() shows both enabled and disabled objective sections", {
  p <- portfolio.spec(assets = 4)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "return", name = "mean")
  # Manually disable the second objective to trigger the disabled branch
  p$objectives[[2]]$enabled <- FALSE
  expect_output(print(p), "Enabled objective")
  expect_output(print(p), "Disabled objective")
})

test_that("print.portfolio() handles a portfolio with no constraints", {
  p <- portfolio.spec(assets = 4)
  # Should not error even with empty constraint list
  expect_output(print(p), "Number of assets: 4")
})

# ---------------------------------------------------------------------------
# 7. summary.portfolio() — branch coverage for generics.R
#    test-constraints.R covers the enabled-constraint and no-objective path.
#    Here we cover disabled constraints and disabled objectives.
# ---------------------------------------------------------------------------

test_that("summary.portfolio() with disabled constraint populates disabled_constraints", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p$constraints[[2]]$enabled <- FALSE
  s <- summary(p)
  expect_true(is.list(s))
  expect_equal(length(s$enabled_constraints), 1L)
  expect_equal(length(s$disabled_constraints), 1L)
})

test_that("summary.portfolio() with disabled objective populates disabled_objectives", {
  p <- portfolio.spec(assets = 4)
  p <- add.objective(p, type = "risk", name = "StdDev")
  p <- add.objective(p, type = "return", name = "mean")
  p$objectives[[2]]$enabled <- FALSE
  s <- summary(p)
  expect_true(is.list(s))
  expect_equal(length(s$enabled_objectives), 1L)
  expect_equal(length(s$disabled_objectives), 1L)
})

test_that("summary.portfolio() with no constraints and no objectives returns valid list", {
  p <- portfolio.spec(assets = 3)
  s <- summary(p)
  expect_true(is.list(s))
  expect_equal(length(s$enabled_constraints), 0)
  expect_equal(length(s$enabled_objectives), 0)
  expect_equal(names(s$assets), paste0("Asset.", 1:3))
})

# ---------------------------------------------------------------------------
# 8. combine.portfolios() and print.portfolio.list()
#    (covers utility.combine.R and the print.portfolio.list method)
# ---------------------------------------------------------------------------

test_that("combine.portfolios() returns a portfolio.list object", {
  p1 <- portfolio.spec(assets = colnames(edhec[, 1:4]))
  p2 <- portfolio.spec(assets = colnames(edhec[, 1:4]))
  p2 <- add.constraint(p2, type = "full_investment")
  pl <- combine.portfolios(list(p1, p2))
  expect_s3_class(pl, "portfolio.list")
  expect_equal(length(pl), 2)
})

test_that("combine.portfolios() errors when list contains non-portfolio", {
  p1 <- portfolio.spec(assets = 3)
  expect_error(combine.portfolios(list(p1, list(a = 1))))
})

test_that("combine.portfolios() errors when x is not a list", {
  expect_error(combine.portfolios(42))
})

test_that("print.portfolio.list() produces output without error", {
  p1 <- portfolio.spec(assets = colnames(edhec[, 1:4]))
  p2 <- portfolio.spec(assets = colnames(edhec[, 1:4]))
  pl <- combine.portfolios(list(p1, p2))
  expect_output(print(pl))
})

# ---------------------------------------------------------------------------
# 9. regime.portfolios() and print.regime.portfolios()
# ---------------------------------------------------------------------------

test_that("regime.portfolios() returns a regime.portfolios object", {
  R <- edhec[, 1:4]
  nms <- colnames(R)

  p1 <- portfolio.spec(assets = nms)
  p1 <- add.constraint(p1, type = "full_investment")
  p1 <- add.constraint(p1, type = "long_only")

  p2 <- portfolio.spec(assets = nms)
  p2 <- add.constraint(p2, type = "full_investment")
  p2 <- add.constraint(p2, type = "box", min = 0.1, max = 0.4)

  pl <- combine.portfolios(list(p1, p2))

  # Two-regime series covering all edhec dates
  regime <- xts::xts(rep(c(1L, 2L), length.out = nrow(R)), zoo::index(R))

  rp <- regime.portfolios(regime = regime, portfolios = pl)

  expect_s3_class(rp, "regime.portfolios")
  expect_equal(length(rp$portfolio.list), 2)
  expect_equal(names(rp$assets), nms)
})

test_that("regime.portfolios() errors when regime is not xts/zoo", {
  p1 <- portfolio.spec(assets = 4)
  p2 <- portfolio.spec(assets = 4)
  pl <- combine.portfolios(list(p1, p2))
  expect_error(regime.portfolios(regime = c(1, 2, 1, 2), portfolios = pl))
})

test_that("regime.portfolios() errors when portfolios is not a portfolio.list", {
  R <- edhec[, 1:4]
  regime <- xts::xts(rep(c(1L, 2L), length.out = nrow(R)), zoo::index(R))
  p1 <- portfolio.spec(assets = colnames(R))
  # pass a plain list, not a portfolio.list
  expect_error(regime.portfolios(regime = regime, portfolios = list(p1, p1)))
})

test_that("regime.portfolios() errors when number of portfolios != number of regimes", {
  R <- edhec[, 1:4]
  nms <- colnames(R)
  p1 <- portfolio.spec(assets = nms)
  p2 <- portfolio.spec(assets = nms)
  p3 <- portfolio.spec(assets = nms)
  pl <- combine.portfolios(list(p1, p2, p3))
  # regime has only 2 unique values but we pass 3 portfolios
  regime <- xts::xts(rep(c(1L, 2L), length.out = nrow(R)), zoo::index(R))
  expect_error(regime.portfolios(regime = regime, portfolios = pl))
})

test_that("regime.portfolios() errors when portfolio assets are not identical", {
  R <- edhec[, 1:4]
  p1 <- portfolio.spec(assets = colnames(R))
  p2 <- portfolio.spec(assets = paste0("X", 1:4)) # different names
  pl <- combine.portfolios(list(p1, p2))
  regime <- xts::xts(rep(c(1L, 2L), length.out = nrow(R)), zoo::index(R))
  expect_error(regime.portfolios(regime = regime, portfolios = pl))
})

test_that("print.regime.portfolios() produces output without error", {
  R <- edhec[, 1:4]
  nms <- colnames(R)
  p1 <- portfolio.spec(assets = nms)
  p1 <- add.constraint(p1, type = "full_investment")
  p2 <- portfolio.spec(assets = nms)
  p2 <- add.constraint(p2, type = "full_investment")
  pl <- combine.portfolios(list(p1, p2))
  regime <- xts::xts(rep(c(1L, 2L), length.out = nrow(R)), zoo::index(R))
  rp <- regime.portfolios(regime = regime, portfolios = pl)
  expect_output(print(rp))
})
