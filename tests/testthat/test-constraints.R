library(PortfolioAnalytics)

skip_on_cran()

# ---- Fixture: 4-asset portfolio with every constraint type ----

N <- 4
init.portf <- portfolio.spec(assets = N)

# Weight_sum constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type = "weight_sum",
  min_sum = 0.99,
  max_sum = 1.01
)
# Box constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type = "box",
  min = 0,
  max = 1
)
# Group constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type = "group",
  groups = list(c(1, 3), c(2, 4)),
  group_min = c(0.15, 0.25),
  group_max = c(0.65, 0.55)
)
# Turnover constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type = "turnover",
  turnover_target = 0.6
)
# Diversification constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type = "diversification",
  div_target = 0.55
)
# Position limit constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type = "position_limit",
  max_pos = 3,
  max_pos_long = 2,
  max_pos_short = 1
)
# Return constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type = "return",
  return_target = 0.007
)
# Factor exposure constraint
init.portf <- add.constraint(
  portfolio = init.portf,
  type = "factor_exposure",
  B = rep(1, N),
  lower = 0.9,
  upper = 1.1
)

tmp_constraints <- PortfolioAnalytics:::get_constraints(init.portf)

# ---- Tests ----

test_that("weight_sum constraint is consistent", {
  expect_equal(tmp_constraints$min_sum, 0.99)
  expect_equal(tmp_constraints$max_sum, 1.01)
})

test_that("box constraint is consistent", {
  expect_equal(as.numeric(tmp_constraints$min), rep(0, N))
  expect_equal(as.numeric(tmp_constraints$max), rep(1, N))
})

test_that("group constraint is consistent", {
  expect_true(is.list(tmp_constraints$groups))
  expect_equal(tmp_constraints$groups[[1]], c(1, 3))
  expect_equal(tmp_constraints$groups[[2]], c(2, 4))
  expect_equal(tmp_constraints$group_labels, c("group1", "group2"))
  expect_equal(tmp_constraints$cLO, c(0.15, 0.25))
  expect_equal(tmp_constraints$cUP, c(0.65, 0.55))
})

test_that("turnover constraint is consistent", {
  expect_equal(tmp_constraints$turnover_target, 0.6)
})

test_that("diversification constraint is consistent", {
  expect_equal(tmp_constraints$div_target, 0.55)
})

test_that("position limit constraint is consistent", {
  expect_equal(tmp_constraints$max_pos, 3)
  expect_equal(tmp_constraints$max_pos_long, 2)
  expect_equal(tmp_constraints$max_pos_short, 1)
})

test_that("return constraint is consistent", {
  expect_equal(tmp_constraints$return_target, 0.007)
})

test_that("factor exposure constraint is consistent", {
  B <- matrix(1, ncol = 1, nrow = N)
  rownames(B) <- paste("Asset", 1:N, sep = ".")
  colnames(B) <- "factor1"
  expect_equal(tmp_constraints$B, B)
  expect_equal(tmp_constraints$lower, 0.9)
  expect_equal(tmp_constraints$upper, 1.1)
})


# ---------------------------------------------------------------------------
# Tests: print and summary methods for portfolio objects (covers generics.R)
# ---------------------------------------------------------------------------

test_that("print.portfolio produces output without error", {
  expect_output(print(init.portf))
})

test_that("summary.portfolio returns a list with assets and enabled_constraints", {
  s <- summary(init.portf)
  expect_true(is.list(s))
  expect_true(!is.null(s$assets))
  expect_true(!is.null(s$enabled_constraints))
})

test_that("print.constraint produces output without error", {
  con <- init.portf$constraints[[1]]
  expect_output(print(con))
})


# ---------------------------------------------------------------------------
# Tests: transaction_cost_constraint
# ---------------------------------------------------------------------------

test_that("transaction_cost_constraint with scalar ptc replicates to vector", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "transaction_cost", ptc = 0.01)
  tc <- Filter(function(x) inherits(x, "transaction_cost_constraint"), p$constraints)[[1]]
  expect_true(inherits(tc, "transaction_cost_constraint"))
  expect_equal(tc$ptc, rep(0.01, 4))
})

test_that("transaction_cost_constraint with vector ptc stores correctly", {
  ptc_vec <- c(0.01, 0.02, 0.015, 0.025)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "transaction_cost", ptc = ptc_vec)
  tc <- Filter(function(x) inherits(x, "transaction_cost_constraint"), p$constraints)[[1]]
  expect_equal(tc$ptc, ptc_vec)
})

test_that("transaction_cost_constraint errors when ptc length mismatches assets", {
  p <- portfolio.spec(assets = 4)
  expect_error(add.constraint(p, type = "transaction_cost", ptc = c(0.01, 0.02)))
})


# ---------------------------------------------------------------------------
# Tests: leverage_exposure_constraint
# ---------------------------------------------------------------------------

test_that("leverage_exposure_constraint stores leverage value correctly", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "leverage_exposure", leverage = 1.6)
  le <- Filter(function(x) inherits(x, "leverage_exposure_constraint"), p$constraints)[[1]]
  expect_true(inherits(le, "leverage_exposure_constraint"))
  expect_equal(le$leverage, 1.6)
})

test_that("leverage_exposure constraint appears in get_constraints output", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "leverage_exposure", leverage = 2.0)
  cons <- PortfolioAnalytics:::get_constraints(p)
  expect_equal(cons$leverage, 2.0)
})


# ---------------------------------------------------------------------------
# Tests: box_constraint branches (scalar vs vector min/max, long_only)
# ---------------------------------------------------------------------------

test_that("box_constraint expands scalar min/max to vector of length nassets", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "box", min = 0.05, max = 0.50)
  bc <- Filter(function(x) inherits(x, "box_constraint"), p$constraints)[[1]]
  expect_equal(as.numeric(bc$min), rep(0.05, 4))
  expect_equal(as.numeric(bc$max), rep(0.50, 4))
})

test_that("box_constraint with vector min/max stores full vectors unchanged", {
  min_v <- c(0.05, 0.10, 0.05, 0.10)
  max_v <- c(0.40, 0.50, 0.40, 0.50)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "box", min = min_v, max = max_v)
  bc <- Filter(function(x) inherits(x, "box_constraint"), p$constraints)[[1]]
  expect_equal(as.numeric(bc$min), min_v)
  expect_equal(as.numeric(bc$max), max_v)
})

test_that("box_constraint type=long_only sets min=0 and max=1", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "long_only")
  bc <- Filter(function(x) inherits(x, "box_constraint"), p$constraints)[[1]]
  expect_equal(bc$type, "long_only")
  expect_equal(as.numeric(bc$min), rep(0, 4))
  expect_equal(as.numeric(bc$max), rep(1, 4))
})


# ---------------------------------------------------------------------------
# Tests: update.constraint
# ---------------------------------------------------------------------------

test_that("update.constraint errors on NULL input", {
  expect_error(PortfolioAnalytics:::update.constraint(NULL))
})

test_that("update.constraint errors on non-constraint object", {
  expect_error(PortfolioAnalytics:::update.constraint(list(type = "box")))
})

test_that("update.constraint errors on constraint without call component", {
  bare_con <- structure(
    list(type = "box", enabled = TRUE),
    class = c("box_constraint", "constraint")
  )
  expect_error(PortfolioAnalytics:::update.constraint(bare_con))
})

test_that("update.constraint with no extras returns original constraint unchanged", {
  con <- init.portf$constraints[[1]]  # has $call set by add.constraint
  result <- PortfolioAnalytics:::update.constraint(con)
  expect_true(is.constraint(result))
  expect_equal(result$min_sum, con$min_sum)
})

test_that("update.constraint with existing field returns original (call updated in place)", {
  # Constraint added via add.constraint has $call; function modifies call but returns original
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  con <- p$constraints[[1]]
  result <- PortfolioAnalytics:::update.constraint(con, min_sum = 0.95)
  expect_true(is.constraint(result))
  # function returns the original object unchanged (call modification only)
  expect_equal(result$min_sum, 0.99)
})

test_that("update.constraint with new (non-existing) field extends call, returns original", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  con <- p$constraints[[1]]
  result <- PortfolioAnalytics:::update.constraint(con, brand_new_arg = 42)
  expect_true(is.constraint(result))
  expect_equal(result$min_sum, 0.99)  # original unchanged
})


# ---------------------------------------------------------------------------
# Tests: insert_constraints
# ---------------------------------------------------------------------------

test_that("insert_constraints replaces constraint list in portfolio", {
  fresh <- portfolio.spec(assets = 4)
  # Build a two-constraint list from a helper portfolio
  tmp <- portfolio.spec(assets = 4)
  tmp <- add.constraint(tmp, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  tmp <- add.constraint(tmp, type = "box", min = 0, max = 1)
  cons_list <- tmp$constraints

  result <- PortfolioAnalytics:::insert_constraints(fresh, cons_list)
  expect_equal(length(result$constraints), 2)
  expect_true(is.portfolio(result))
})

test_that("insert_constraints errors on non-portfolio input", {
  expect_error(PortfolioAnalytics:::insert_constraints(list(), list()))
})

test_that("insert_constraints errors when constraints is not a list", {
  p <- portfolio.spec(assets = 4)
  expect_error(PortfolioAnalytics:::insert_constraints(p, "not_a_list"))
})

test_that("insert_constraints errors when list contains non-constraint object", {
  p <- portfolio.spec(assets = 4)
  expect_error(PortfolioAnalytics:::insert_constraints(p, list(list(a = 1))))
})


# ---------------------------------------------------------------------------
# Tests: update_constraint_v1tov2
# ---------------------------------------------------------------------------

test_that("update_constraint_v1tov2 errors on non-portfolio input", {
  suppressMessages({
    v1c <- PortfolioAnalytics:::constraint_v1(
      assets = 4, min = rep(0, 4), max = rep(1, 4),
      min_sum = 0.99, max_sum = 1.01
    )
  })
  expect_error(PortfolioAnalytics:::update_constraint_v1tov2(list(), v1c))
})

test_that("update_constraint_v1tov2 errors on non-v1_constraint input", {
  p <- portfolio.spec(assets = 4)
  expect_error(PortfolioAnalytics:::update_constraint_v1tov2(p, list()))
})

test_that("update_constraint_v1tov2 converts weight_sum and box branches", {
  suppressMessages({
    v1c <- PortfolioAnalytics:::constraint_v1(
      assets = 4, min = rep(0, 4), max = rep(1, 4),
      min_sum = 0.99, max_sum = 1.01
    )
  })
  p <- portfolio.spec(assets = 4)
  result <- PortfolioAnalytics:::update_constraint_v1tov2(p, v1c)

  types <- sapply(result$constraints, function(x) x$type)
  expect_true("weight_sum" %in% types)
  expect_true("box" %in% types)
  expect_true(is.portfolio(result))
})

test_that("update_constraint_v1tov2 converts group branch when cLO/cUP present", {
  suppressMessages({
    v1c <- PortfolioAnalytics:::constraint_v1(
      assets = 4, min = rep(0, 4), max = rep(1, 4),
      min_sum = 0.99, max_sum = 1.01
    )
  })
  v1c$groups <- list(c(1, 2), c(3, 4))
  v1c$cLO    <- c(0.3, 0.3)
  v1c$cUP    <- c(0.7, 0.7)

  p      <- portfolio.spec(assets = 4)
  result <- PortfolioAnalytics:::update_constraint_v1tov2(p, v1c)

  types <- sapply(result$constraints, function(x) x$type)
  expect_true("group" %in% types)
})

test_that("update_constraint_v1tov2 skips weight_sum when min_sum/max_sum are NULL", {
  suppressMessages({
    v1c <- PortfolioAnalytics:::constraint_v1(
      assets = 4, min = rep(0, 4), max = rep(1, 4)
      # min_sum and max_sum intentionally omitted → they become NULL
    )
  })
  p      <- portfolio.spec(assets = 4)
  result <- PortfolioAnalytics:::update_constraint_v1tov2(p, v1c)
  types  <- sapply(result$constraints, function(x) x$type)
  expect_false("weight_sum" %in% types)
})


# ---------------------------------------------------------------------------
# Tests: check_constraints
# ---------------------------------------------------------------------------

test_that("check_constraints returns TRUE for fully feasible weights", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  w <- c(0.25, 0.25, 0.25, 0.25)
  expect_true(PortfolioAnalytics:::check_constraints(w, p))
})

test_that("check_constraints returns FALSE when weight_sum violated (under)", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  w <- c(0.2, 0.2, 0.2, 0.2)   # sum = 0.8 < 0.99
  expect_false(PortfolioAnalytics:::check_constraints(w, p))
})

test_that("check_constraints returns FALSE when weight_sum violated (over)", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  w <- c(0.4, 0.3, 0.3, 0.1)   # sum = 1.1 > 1.01
  expect_false(PortfolioAnalytics:::check_constraints(w, p))
})

test_that("check_constraints returns FALSE when box constraint violated", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.10, max = 0.50)
  w <- c(0.05, 0.35, 0.30, 0.30)   # first weight below min=0.10
  expect_false(PortfolioAnalytics:::check_constraints(w, p))
})

test_that("check_constraints returns FALSE when group constraint violated (cLO)", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(
    p, type = "group",
    groups    = list(c(1, 2), c(3, 4)),
    group_min = c(0.4, 0.4),
    group_max = c(0.7, 0.7)
  )
  # group 1 sum = 0.1 + 0.1 = 0.2 < 0.4  →  cLO violation
  w <- c(0.1, 0.1, 0.5, 0.3)
  expect_false(PortfolioAnalytics:::check_constraints(w, p))
})

test_that("check_constraints returns FALSE when position_limit violated", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "position_limit", max_pos = 2)
  w <- c(0.3, 0.3, 0.2, 0.2)   # 4 non-zero positions > max_pos=2
  expect_false(PortfolioAnalytics:::check_constraints(w, p))
})

test_that("check_constraints returns TRUE when position_limit is satisfied", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "position_limit", max_pos = 2)
  w <- c(0.5, 0.5, 0.0, 0.0)   # exactly 2 non-zero positions
  expect_true(PortfolioAnalytics:::check_constraints(w, p))
})

test_that("check_constraints returns TRUE for leverage_exposure satisfied", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "leverage_exposure", leverage = 1.5)
  w <- c(0.25, 0.25, 0.25, 0.25)   # sum(abs) = 1.0 <= 1.5
  expect_true(PortfolioAnalytics:::check_constraints(w, p))
})

test_that("check_constraints returns FALSE for leverage_exposure violation", {
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "leverage_exposure", leverage = 0.9)
  w <- c(0.25, 0.25, 0.25, 0.25)   # sum(abs) = 1.0 > 0.9
  expect_false(PortfolioAnalytics:::check_constraints(w, p))
})


# ---------------------------------------------------------------------------
# Tests: group_fail helper
# ---------------------------------------------------------------------------

test_that("group_fail returns TRUE for cLO violation in group 1", {
  groups <- list(c(1, 2), c(3, 4))
  cLO    <- c(0.4, 0.4)
  cUP    <- c(0.7, 0.7)
  # group 1 sum = 0.15 + 0.20 = 0.35 < 0.40  →  cLO violation
  # group 2 sum = 0.30 + 0.35 = 0.65, within [0.4, 0.7]  →  no violation
  w      <- c(0.15, 0.20, 0.30, 0.35)
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP)
  expect_true(result[1])
  expect_false(result[2])
})

test_that("group_fail returns TRUE for cUP violation in group 1", {
  groups <- list(c(1, 2), c(3, 4))
  cLO    <- c(0.1, 0.1)
  cUP    <- c(0.4, 0.9)
  # group 1 sum = 0.5 + 0.3 = 0.8 > 0.4
  w      <- c(0.5, 0.3, 0.1, 0.1)
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP)
  expect_true(result[1])
  expect_false(result[2])
})

test_that("group_fail returns all FALSE when both groups are within bounds", {
  groups <- list(c(1, 2), c(3, 4))
  cLO    <- c(0.3, 0.3)
  cUP    <- c(0.7, 0.7)
  w      <- c(0.3, 0.2, 0.3, 0.2)   # group sums = 0.5, 0.5
  result <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP)
  expect_false(any(result))
})

test_that("group_fail returns FALSE (scalar) when groups is NULL", {
  result <- PortfolioAnalytics:::group_fail(
    c(0.25, 0.25, 0.25, 0.25), NULL, NULL, NULL
  )
  expect_false(any(result))
})

test_that("group_fail detects group_pos violation", {
  # group_pos=1 means at most 1 non-zero weight per group
  groups    <- list(c(1, 2), c(3, 4))
  cLO       <- c(0.0, 0.0)
  cUP       <- c(1.0, 1.0)
  group_pos <- c(1, 2)   # group 1 allows only 1 non-zero
  w         <- c(0.3, 0.2, 0.3, 0.2)   # group 1 has 2 non-zero → violates group_pos[1]=1
  result    <- PortfolioAnalytics:::group_fail(w, groups, cLO, cUP, group_pos)
  expect_true(result[1])
  expect_false(result[2])
})


# ---------------------------------------------------------------------------
# Tests: pos_limit_fail helper
# ---------------------------------------------------------------------------

test_that("pos_limit_fail returns TRUE when max_pos is exceeded", {
  w <- c(0.3, 0.3, 0.2, 0.2)   # 4 non-zero > max_pos=2
  expect_true(
    PortfolioAnalytics:::pos_limit_fail(w, max_pos = 2, max_pos_long = NULL, max_pos_short = NULL)
  )
})

test_that("pos_limit_fail returns TRUE when max_pos_long is exceeded", {
  w <- c(0.3, 0.3, 0.3, 0.1)   # 4 long positions > max_pos_long=2
  expect_true(
    PortfolioAnalytics:::pos_limit_fail(w, max_pos = NULL, max_pos_long = 2, max_pos_short = NULL)
  )
})

test_that("pos_limit_fail returns TRUE when max_pos_short is exceeded", {
  w <- c(-0.2, -0.3, 0.75, 0.75)   # 2 short positions > max_pos_short=1
  expect_true(
    PortfolioAnalytics:::pos_limit_fail(w, max_pos = NULL, max_pos_long = NULL, max_pos_short = 1)
  )
})

test_that("pos_limit_fail returns FALSE when all position limits satisfied", {
  w <- c(0.5, 0.5, 0.0, 0.0)   # 2 non-zero, 2 long, 0 short
  expect_false(
    PortfolioAnalytics:::pos_limit_fail(w, max_pos = 3, max_pos_long = 3, max_pos_short = 3)
  )
})

test_that("pos_limit_fail returns FALSE when all limits are NULL", {
  w <- c(0.3, 0.3, 0.2, 0.2)
  expect_false(
    PortfolioAnalytics:::pos_limit_fail(w, max_pos = NULL, max_pos_long = NULL, max_pos_short = NULL)
  )
})


# ---------------------------------------------------------------------------
# Tests: fn_map branches
# ---------------------------------------------------------------------------

test_that("fn_map returns list with named weights element", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- paste0("Asset.", 1:4)
  result <- PortfolioAnalytics:::fn_map(w, p)
  expect_true(is.list(result))
  expect_true(!is.null(result$weights))
  expect_equal(length(result$weights), 4)
})

test_that("fn_map handles weight_sum violation by attempting transformation", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  # sum = 0.4, far below min_sum=0.99
  w <- c(0.1, 0.1, 0.1, 0.1)
  names(w) <- paste0("Asset.", 1:4)
  result <- PortfolioAnalytics:::fn_map(w, p)
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
})

test_that("fn_map handles box constraint violation by attempting transformation", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0.20, max = 0.40)
  # first weight far above max=0.40
  w <- c(0.70, 0.10, 0.10, 0.10)
  names(w) <- paste0("Asset.", 1:4)
  result <- PortfolioAnalytics:::fn_map(w, p)
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
})

test_that("fn_map handles group constraint violation by attempting transformation", {
  set.seed(42)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(
    p, type = "group",
    groups    = list(c(1, 2), c(3, 4)),
    group_min = c(0.3, 0.3),
    group_max = c(0.7, 0.7)
  )
  # group 1 sum = 0.55 + 0.30 = 0.85 > 0.7  →  violation
  w <- c(0.55, 0.30, 0.10, 0.05)
  names(w) <- paste0("Asset.", 1:4)
  result <- PortfolioAnalytics:::fn_map(w, p)
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
  expect_true(!is.null(result$cLO))
  expect_true(!is.null(result$cUP))
})

test_that("fn_map handles position_limit violation by attempting transformation", {
  set.seed(42)
  p <- portfolio.spec(assets = 6)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  p <- add.constraint(p, type = "box", min = 0, max = 1)
  p <- add.constraint(p, type = "position_limit", max_pos = 3)
  # all 6 positions non-zero  →  violates max_pos=3
  w <- rep(1 / 6, 6)
  names(w) <- paste0("Asset.", 1:6)
  result <- PortfolioAnalytics:::fn_map(w, p)
  expect_true(is.list(result))
  expect_equal(length(result$weights), 6)
  expect_false(is.null(result$max_pos))
})

test_that("fn_map with relax=TRUE exercises constraint relaxation path", {
  set.seed(99)
  p <- portfolio.spec(assets = 4)
  p <- add.constraint(p, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
  # Very tight box: forces many failures so relax path is triggered
  p <- add.constraint(p, type = "box", min = 0.249, max = 0.251)
  w <- c(0.1, 0.1, 0.1, 0.1)
  names(w) <- paste0("Asset.", 1:4)
  # relax=TRUE exercises the inner while-loop relaxation branches
  result <- PortfolioAnalytics:::fn_map(w, p, relax = TRUE)
  expect_true(is.list(result))
  expect_equal(length(result$weights), 4)
})

test_that('add.constraint with null type returns portfolio', {
  p <- portfolio.spec(assets = 4)
  p2 <- add.constraint(p, type='null')
  expect_equal(p, p2)
})

test_that('add.constraint with filter type errors', {
  p <- portfolio.spec(assets = 4)
  expect_error(add.constraint(p, type='filter'))
})

test_that('box_constraint edge cases', {
  p <- portfolio.spec(assets=4)
  expect_error(add.constraint(p, type='box', min=c(0,0), max=c(1,1,1)))
  expect_error(add.constraint(p, type='box', min=c(0,0), max=1))
  expect_error(add.constraint(p, type='box', min=0, max=c(1,1)))
  
  # min_mult / max_mult errors
  expect_error(add.constraint(p, type='box', min_mult=c(1,1), max_mult=c(2,2,2)))
  
  # min_mult adjustments
  p_m <- add.constraint(p, type='box', min_mult=1, max_mult=1)
  expect_true(is.null(p_m$constraints[[1]]$min_mult))
})

test_that('group_constraint edge cases', {
  p <- portfolio.spec(assets=4)
  expect_error(add.constraint(p, type='group', groups=c(1,2)))
  expect_error(add.constraint(p, type='group', groups=list(1:2, 3:4), group_min=c(0.1, 0.2, 0.3)))
  expect_error(add.constraint(p, type='group', groups=list(1:2, 3:4), group_max=c(0.8, 0.9, 1.0)))
  
  expect_error(add.constraint(p, type='group', groups=list(1:2, 3:4), group_pos=c(1,2,3)))
  expect_error(add.constraint(p, type='group', groups=list(1:2, 3:4), group_pos=c(-1, 1)))
  
  # Named groups
  p_g <- add.constraint(p, type='group', groups=list(A=1:2, B=3:4), group_min=c(0,0), group_max=c(1,1))
  expect_equal(p_g$constraints[[1]]$group_labels, c('A', 'B'))
  
  # group_pos > count
  p_gp <- add.constraint(p, type='group', groups=list(1:2, 3:4), group_min=c(0,0), group_max=c(1,1), group_pos=c(3, 1))
  expect_equal(p_gp$constraints[[1]]$group_pos, c(2, 1))
})

test_that('factor_exposure_constraint edge cases', {
  p <- portfolio.spec(assets=4)
  expect_error(add.constraint(p, type='factor_exposure', B=c(1,1)))
  expect_error(add.constraint(p, type='factor_exposure', B=rep(1,4), lower=c(0,0), upper=1))
  expect_error(add.constraint(p, type='factor_exposure', B=rep(1,4), lower=0, upper=c(1,1)))
  
  # B as matrix
  B_mat <- matrix(1, nrow=2, ncol=2)
  expect_error(add.constraint(p, type='factor_exposure', B=B_mat, lower=c(0,0), upper=c(1,1)))
  B_mat4 <- matrix(1, nrow=4, ncol=2)
  expect_error(add.constraint(p, type='factor_exposure', B=B_mat4, lower=0, upper=c(1,1)))
  expect_error(add.constraint(p, type='factor_exposure', B=B_mat4, lower=c(0,0), upper=1))
  
  p_fe <- add.constraint(p, type='factor_exposure', B=B_mat4, lower=c(0,0), upper=c(1,1))
  expect_equal(colnames(p_fe$constraints[[1]]$B), c('factor1', 'factor2'))
  expect_equal(rownames(p_fe$constraints[[1]]$B), names(p$assets))
})

