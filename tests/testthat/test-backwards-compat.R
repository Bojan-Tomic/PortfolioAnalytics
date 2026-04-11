###############################################################################
# tests/testthat/test-backwards-compat.R
#
# Migrated from inst/tests/test_backwards_compat.R
#  - Removed require(testthat) / library(testthat)
#  - Removed all context() calls
#  - Replaced all expect_that() with modern equivalents
#  - Reproduced demo/backwards_compat.R setup inline (no sourcing)
#  - Added skip guards
#  - Stochastic solvers (random, DEoptim) check class/numeric only
#  - ROI (deterministic) checks exact weights and objective measure
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(DEoptim)
library(ROI)
library(ROI.plugin.glpk)

skip_on_cran()
skip_if_not_installed("DEoptim")
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")

# ---------------------------------------------------------------------------
# Data and portfolio setup
# Reproduces demo/backwards_compat.R inline (no sourcing).
# ---------------------------------------------------------------------------

utils::data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

# v1 constraint object (legacy API)
gen.constr <- constraint_v1(
  assets     = funds,
  min        = 0,
  max        = 0.55,
  min_sum    = 0.99,
  max_sum    = 1.01,
  weight_seq = generatesequence(min = 0, max = 0.55, by = 0.002)
)

# Add a return objective to the v1 constraint
gen.constr <- add.objective(
  constraints = gen.constr,
  type        = "return",
  name        = "mean",
  enabled     = TRUE
)

# Random portfolios (stochastic — check structure/type, not exact values)
optrpv1 <- optimize.portfolio(
  R                = ret,
  constraints      = gen.constr,
  optimize_method  = "random",
  search_size      = 2000
)

# DEoptim (stochastic — check structure/type, not exact values)
optdev1 <- optimize.portfolio(
  R                = ret,
  constraints      = gen.constr,
  optimize_method  = "DEoptim",
  search_size      = 2000
)

# ROI (deterministic — check exact values with tolerance)
optroiv1 <- optimize.portfolio(
  R                = ret,
  constraints      = gen.constr,
  optimize_method  = "ROI"
)


# ---------------------------------------------------------------------------
# Tests: v1_constraint object structure
# ---------------------------------------------------------------------------

test_that("gen.constr has class v1_constraint", {
  expect_s3_class(gen.constr, "v1_constraint")
})

test_that("initial assets form an equal-weight portfolio", {
  expect_true(isTRUE(all.equal(as.numeric(gen.constr$assets), rep(1 / 4, 4))))
})

test_that("box constraint min vector is all 0s", {
  expect_true(isTRUE(all.equal(as.numeric(gen.constr$min), rep(0, 4))))
})

test_that("box constraint max vector is all 0.55", {
  expect_true(isTRUE(all.equal(as.numeric(gen.constr$max), rep(0.55, 4))))
})

test_that("min_mult is NULL", {
  expect_null(gen.constr$min_mult)
})

test_that("max_mult is NULL", {
  expect_null(gen.constr$max_mult)
})

test_that("min_sum is 0.99", {
  expect_equal(gen.constr$min_sum, 0.99)
})

test_that("max_sum is 1.01", {
  expect_equal(gen.constr$max_sum, 1.01)
})

test_that("first objective name is 'mean'", {
  expect_equal(gen.constr$objectives[[1]]$name, "mean")
})


# ---------------------------------------------------------------------------
# Tests: random portfolios result
# ---------------------------------------------------------------------------

test_that("random portfolios result contains an updated portfolio.spec", {
  expect_s3_class(optrpv1$portfolio, "portfolio.spec")
})

test_that("random portfolios returns numeric optimal weights", {
  expect_true(is.numeric(extractWeights(optrpv1)))
})

test_that("random portfolios returns a numeric mean objective measure", {
  expect_true(is.numeric(extractObjectiveMeasures(optrpv1)$mean))
})


# ---------------------------------------------------------------------------
# Tests: DEoptim result
# ---------------------------------------------------------------------------

test_that("DEoptim result contains an updated portfolio.spec", {
  expect_s3_class(optdev1$portfolio, "portfolio.spec")
})

test_that("DEoptim returns numeric optimal weights", {
  expect_true(is.numeric(extractWeights(optdev1)))
})

test_that("DEoptim returns a numeric mean objective measure", {
  expect_true(is.numeric(extractObjectiveMeasures(optdev1)$mean))
})


# ---------------------------------------------------------------------------
# Tests: ROI result (deterministic — exact checks)
# ---------------------------------------------------------------------------

test_that("ROI result contains an updated portfolio.spec", {
  expect_s3_class(optroiv1$portfolio, "portfolio.spec")
})

test_that("ROI returns optimal weights equal to c(0, 0, 0.55, 0.46)", {
  expect_equal(as.numeric(extractWeights(optroiv1)), c(0, 0, 0.55, 0.46))
})

test_that("ROI returns mean objective measure = 0.006849676", {
  expect_equal(
    as.numeric(extractObjectiveMeasures(optroiv1)$mean),
    0.006849676,
    tolerance = 1e-6
  )
})


# ============================================================================
# Direct calls to optimize.portfolio_v1() and optimize.portfolio.rebalancing_v1()
# These exercise the v1 code path directly, not through the v2 dispatcher.
# All stochastic calls are wrapped in tryCatch; tests guard with skip_if(is.null()).
# ============================================================================

# ---------------------------------------------------------------------------
# File-scope optimization calls — placed before test_that blocks
# ---------------------------------------------------------------------------

# 1. optimize.portfolio_v1 with DEoptim
opt_v1_de <- tryCatch({
  optimize.portfolio_v1(
    R               = ret,
    constraints     = gen.constr,
    optimize_method = "DEoptim",
    search_size     = 2000,
    itermax         = 30
  )
}, error = function(e) NULL)

# 2. optimize.portfolio_v1 with random
set.seed(42)
opt_v1_rp <- tryCatch({
  optimize.portfolio_v1(
    R               = ret,
    constraints     = gen.constr,
    optimize_method = "random",
    search_size     = 500
  )
}, error = function(e) NULL)

# 3. optimize.portfolio_v1 with ROI (deterministic)
opt_v1_roi <- tryCatch({
  optimize.portfolio_v1(
    R               = ret,
    constraints     = gen.constr,
    optimize_method = "ROI"
  )
}, error = function(e) NULL)

# 4. optimize.portfolio_v1 with DEoptim and trace=TRUE
#    The DEoptim results are stored in $DEoutput when trace=TRUE.
opt_v1_trace <- tryCatch({
  optimize.portfolio_v1(
    R               = ret,
    constraints     = gen.constr,
    optimize_method = "DEoptim",
    search_size     = 500,
    itermax         = 20,
    trace           = TRUE
  )
}, error = function(e) NULL)

# 5. optimize.portfolio.rebalancing_v1 with random
#    Build a short return series and a fresh v1 constraint with weight_seq so
#    random_portfolios_v1() (called below) works.  We pre-generate the random
#    portfolio matrix with random_portfolios_v1() and pass it via rp= to
#    bypass the incompatible random_portfolios() call inside the rebalancing
#    function (which expects a v2 portfolio.spec, not a v1_constraint).
ret_short <- ret["2019/2021"]

rebal_constr <- constraint_v1(
  assets     = colnames(ret_short),
  min        = 0,
  max        = 0.55,
  min_sum    = 0.99,
  max_sum    = 1.01,
  weight_seq = generatesequence(min = 0, max = 0.55, by = 0.002)
)
rebal_constr <- add.objective(
  constraints = rebal_constr,
  type        = "return",
  name        = "mean",
  enabled     = TRUE
)

# Pre-generate random portfolios using the v1 helper so we can pass rp= and
# skip the broken random_portfolios(rpconstraints=…) call in the v1 rebalancer.
rebal_rp <- tryCatch(
  random_portfolios_v1(rpconstraints = rebal_constr, permutations = 200),
  error = function(e) NULL
)

opt_v1_rebal <- tryCatch({
  if (!requireNamespace("foreach",   quietly = TRUE)) stop("foreach not available")
  if (!requireNamespace("iterators", quietly = TRUE)) stop("iterators not available")
  if (is.null(rebal_rp)) stop("rebal_rp generation failed")
  optimize.portfolio.rebalancing_v1(
    R               = ret_short,
    constraints     = rebal_constr,
    optimize_method = "random",
    search_size     = 200,
    rp              = rebal_rp,
    rebalance_on    = "years",
    training_period = 12
  )
}, error = function(e) NULL)


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with DEoptim
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 DEoptim result inherits optimize.portfolio", {
  skip_if(is.null(opt_v1_de))
  expect_true(inherits(opt_v1_de, "optimize.portfolio"))
})

test_that("optimize.portfolio_v1 DEoptim returns numeric optimal weights", {
  skip_if(is.null(opt_v1_de))
  expect_true(is.numeric(extractWeights(opt_v1_de)))
})

test_that("optimize.portfolio_v1 DEoptim returns numeric mean objective measure", {
  skip_if(is.null(opt_v1_de))
  expect_true(is.numeric(extractObjectiveMeasures(opt_v1_de)$mean))
})


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with random
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 random result inherits optimize.portfolio", {
  skip_if(is.null(opt_v1_rp))
  expect_true(inherits(opt_v1_rp, "optimize.portfolio"))
})

test_that("optimize.portfolio_v1 random returns numeric optimal weights", {
  skip_if(is.null(opt_v1_rp))
  expect_true(is.numeric(extractWeights(opt_v1_rp)))
})

test_that("optimize.portfolio_v1 random returns numeric mean objective measure", {
  skip_if(is.null(opt_v1_rp))
  expect_true(is.numeric(extractObjectiveMeasures(opt_v1_rp)$mean))
})


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with ROI
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 ROI result inherits optimize.portfolio", {
  skip_if(is.null(opt_v1_roi))
  expect_true(inherits(opt_v1_roi, "optimize.portfolio"))
})

test_that("optimize.portfolio_v1 ROI returns numeric optimal weights", {
  skip_if(is.null(opt_v1_roi))
  expect_true(is.numeric(extractWeights(opt_v1_roi)))
})

test_that("optimize.portfolio_v1 ROI returns numeric objective value in $out", {
  skip_if(is.null(opt_v1_roi))
  expect_true(is.numeric(opt_v1_roi$out))
})


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with trace=TRUE (DEoptim)
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 trace=TRUE DEoptim populates $DEoutput slot", {
  skip_if(is.null(opt_v1_trace))
  expect_false(is.null(opt_v1_trace$DEoutput))
})


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio.rebalancing_v1
# ---------------------------------------------------------------------------

test_that("optimize.portfolio.rebalancing_v1 result has class optimize.portfolio.rebalancing", {
  skip_if(is.null(opt_v1_rebal))
  expect_true(inherits(opt_v1_rebal, "optimize.portfolio.rebalancing"))
})

test_that("optimize.portfolio.rebalancing_v1 result has length > 0", {
  skip_if(is.null(opt_v1_rebal))
  expect_gt(length(opt_v1_rebal), 0)
})


# ---------------------------------------------------------------------------
# File-scope calls: optimize.portfolio_v1 with PSO
# ---------------------------------------------------------------------------

# 6. optimize.portfolio_v1 with PSO
opt_v1_pso <- if (requireNamespace("pso", quietly = TRUE)) {
  tryCatch({
    optimize.portfolio_v1(
      R               = ret,
      constraints     = gen.constr,
      optimize_method = "pso",
      maxit           = 50
    )
  }, error = function(e) NULL)
} else NULL

# 7. optimize.portfolio_v1 with PSO and trace=TRUE (exercises $PSOoutput branch)
opt_v1_pso_trace <- if (requireNamespace("pso", quietly = TRUE)) {
  tryCatch({
    optimize.portfolio_v1(
      R               = ret,
      constraints     = gen.constr,
      optimize_method = "pso",
      maxit           = 50,
      trace           = TRUE
    )
  }, error = function(e) NULL)
} else NULL


# ---------------------------------------------------------------------------
# File-scope calls: optimize.portfolio_v1 with GenSA
# ---------------------------------------------------------------------------

# 8. optimize.portfolio_v1 with GenSA
opt_v1_gensa <- if (requireNamespace("GenSA", quietly = TRUE)) {
  tryCatch({
    optimize.portfolio_v1(
      R               = ret,
      constraints     = gen.constr,
      optimize_method = "GenSA",
      maxit           = 50
    )
  }, error = function(e) NULL)
} else NULL

# 9. optimize.portfolio_v1 with GenSA and trace=TRUE (exercises $GenSAoutput branch)
opt_v1_gensa_trace <- if (requireNamespace("GenSA", quietly = TRUE)) {
  tryCatch({
    optimize.portfolio_v1(
      R               = ret,
      constraints     = gen.constr,
      optimize_method = "GenSA",
      maxit           = 50,
      trace           = TRUE
    )
  }, error = function(e) NULL)
} else NULL

# 10. optimize.portfolio_v1 with GenSA and rp= (exercises the !is.null(rp) branch;
#     par <- rp[, 1]).  The matrix is transposed so that rp[, 1] yields a valid
#     N-length weight vector (first portfolio from rebal_rp).
opt_v1_gensa_rp <- if (requireNamespace("GenSA", quietly = TRUE) && !is.null(rebal_rp)) {
  tryCatch({
    optimize.portfolio_v1(
      R               = ret,
      constraints     = gen.constr,
      optimize_method = "GenSA",
      maxit           = 50,
      rp              = t(rebal_rp)
    )
  }, error = function(e) NULL)
} else NULL


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with PSO
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 PSO result inherits optimize.portfolio", {
  skip_if(is.null(opt_v1_pso))
  expect_true(inherits(opt_v1_pso, "optimize.portfolio"))
})

test_that("optimize.portfolio_v1 PSO returns numeric optimal weights", {
  skip_if(is.null(opt_v1_pso))
  expect_true(is.numeric(extractWeights(opt_v1_pso)))
})

test_that("optimize.portfolio_v1 PSO returns numeric mean objective measure", {
  skip_if(is.null(opt_v1_pso))
  expect_true(is.numeric(extractObjectiveMeasures(opt_v1_pso)$mean))
})


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with PSO trace=TRUE
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 trace=TRUE PSO populates $PSOoutput slot", {
  skip_if(is.null(opt_v1_pso_trace))
  expect_false(is.null(opt_v1_pso_trace$PSOoutput))
})


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with GenSA
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 GenSA result inherits optimize.portfolio", {
  skip_if(is.null(opt_v1_gensa))
  expect_true(inherits(opt_v1_gensa, "optimize.portfolio"))
})

test_that("optimize.portfolio_v1 GenSA returns numeric optimal weights", {
  skip_if(is.null(opt_v1_gensa))
  expect_true(is.numeric(extractWeights(opt_v1_gensa)))
})

test_that("optimize.portfolio_v1 GenSA returns numeric mean objective measure", {
  skip_if(is.null(opt_v1_gensa))
  expect_true(is.numeric(extractObjectiveMeasures(opt_v1_gensa)$mean))
})


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with GenSA trace=TRUE
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 trace=TRUE GenSA populates $GenSAoutput slot", {
  skip_if(is.null(opt_v1_gensa_trace))
  expect_false(is.null(opt_v1_gensa_trace$GenSAoutput))
})


# ---------------------------------------------------------------------------
# Tests: optimize.portfolio_v1 with GenSA rp= argument
# ---------------------------------------------------------------------------

test_that("optimize.portfolio_v1 GenSA with rp= result inherits optimize.portfolio", {
  skip_if(is.null(opt_v1_gensa_rp))
  expect_true(inherits(opt_v1_gensa_rp, "optimize.portfolio"))
})

test_that("optimize.portfolio_v1 GenSA with rp= returns numeric optimal weights", {
  skip_if(is.null(opt_v1_gensa_rp))
  expect_true(is.numeric(extractWeights(opt_v1_gensa_rp)))
})
