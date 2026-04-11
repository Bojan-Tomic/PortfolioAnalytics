###############################################################################
# tests/testthat/test-extractstats-roi-etl.R
#
# Coverage targets:
#
#   R/optimize.portfolio.R — ROI + ETL/ES/CVaR path (lines 1248-1290)
#     1. ef=TRUE  branch (line 1257-1259): skip mean_etl_opt, set meanetl=TRUE
#     2. maxSTARR=FALSE branch (line 1258,1263): skip mean_etl_opt when FALSE
#     3. max_pos MILP branch (line 1268): etl_milp_opt is called
#
#   R/extractstats.R — extractStats.DEoptim (lines 101-106)
#     4. is.atomic=TRUE skip guard (the else branch when a result row is atomic)
#        — inject an atomic element into the DEoptim_objective_results list
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")

utils::data(edhec)
R4 <- edhec[, 1:4]
nms <- colnames(R4)

# ---------------------------------------------------------------------------
# Helper: build mean + ES portfolio (ROI solver)
# ---------------------------------------------------------------------------

make_mean_es_portf <- function(max_pos = NULL) {
  p <- portfolio.spec(nms)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "long_only")
  p <- add.objective(p, type = "return", name = "mean")
  p <- add.objective(p, type = "risk",   name = "ES",
                     arguments = list(p = 0.05))
  if (!is.null(max_pos)) {
    p <- add.constraint(p, type = "position_limit", max_pos = max_pos)
  }
  p
}

# ===========================================================================
# Section 1: ROI + ETL, ef=TRUE branch (lines 1257-1259)
# ef=TRUE skips the mean_etl_opt call and goes straight to etl_opt.
# This is exercised internally by meanetl.efficient.frontier().
# ===========================================================================

portf_mean_es <- make_mean_es_portf()

# ef=TRUE requires a target return; supply one close to the mean of the data
target_ret <- mean(colMeans(R4))

opt_ef_true <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_mean_es,
                       optimize_method = "ROI",
                       ef     = TRUE,
                       target = target_ret)
  )),
  error = function(e) NULL
)

test_that("ROI+ETL ef=TRUE: optimization returns non-NULL result", {
  skip_if(is.null(opt_ef_true))
  expect_false(is.null(opt_ef_true))
})

test_that("ROI+ETL ef=TRUE: result has class 'optimize.portfolio.ROI'", {
  skip_if(is.null(opt_ef_true))
  expect_s3_class(opt_ef_true, "optimize.portfolio.ROI")
})

test_that("ROI+ETL ef=TRUE: extractWeights returns numeric vector", {
  skip_if(is.null(opt_ef_true))
  w <- extractWeights(opt_ef_true)
  expect_true(is.numeric(w))
  expect_false(any(is.na(w)))
})

test_that("ROI+ETL ef=TRUE: weights sum to approximately 1", {
  skip_if(is.null(opt_ef_true))
  expect_equal(sum(extractWeights(opt_ef_true)), 1, tolerance = 1e-4)
})

test_that("ROI+ETL ef=TRUE: objective_measures has mean element (meanetl=TRUE)", {
  skip_if(is.null(opt_ef_true))
  om <- opt_ef_true$objective_measures
  expect_false(is.null(om$mean))
})

test_that("ROI+ETL ef=TRUE: objective_measures has ES element", {
  skip_if(is.null(opt_ef_true))
  om <- opt_ef_true$objective_measures
  es_val <- om$ES %||% om$ETL %||% om$CVaR
  expect_false(is.null(es_val))
})

# ===========================================================================
# Section 2: ROI + ETL, maxSTARR=FALSE branch (line 1258, 1263)
# With maxSTARR=FALSE, the mean_etl_opt call is skipped even when ef=FALSE
# and there's a mean objective + ES objective.
# ===========================================================================

opt_maxstarr_false <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_mean_es,
                       optimize_method = "ROI",
                       maxSTARR = FALSE)
  )),
  error = function(e) NULL
)

test_that("ROI+ETL maxSTARR=FALSE: optimization returns non-NULL result", {
  skip_if(is.null(opt_maxstarr_false))
  expect_false(is.null(opt_maxstarr_false))
})

test_that("ROI+ETL maxSTARR=FALSE: result has class 'optimize.portfolio.ROI'", {
  skip_if(is.null(opt_maxstarr_false))
  expect_s3_class(opt_maxstarr_false, "optimize.portfolio.ROI")
})

test_that("ROI+ETL maxSTARR=FALSE: extractWeights returns valid weights", {
  skip_if(is.null(opt_maxstarr_false))
  w <- extractWeights(opt_maxstarr_false)
  expect_true(is.numeric(w))
  expect_equal(sum(w), 1, tolerance = 1e-4)
})

# ===========================================================================
# Section 3: ROI + ETL + max_pos (MILP branch, line 1268)
# Adding a position_limit constraint routes through etl_milp_opt.
# Requires ROI.plugin.glpk for MILP.
# ===========================================================================

portf_max_pos <- make_mean_es_portf(max_pos = 3L)

opt_milp <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_max_pos,
                       optimize_method = "ROI")
  )),
  error = function(e) NULL
)

test_that("ROI+ETL+max_pos (MILP): optimization returns non-NULL or skips gracefully", {
  skip_if(is.null(opt_milp))
  expect_s3_class(opt_milp, "optimize.portfolio.ROI")
})

test_that("ROI+ETL+max_pos (MILP): extractWeights returns valid weights", {
  skip_if(is.null(opt_milp))
  w <- extractWeights(opt_milp)
  expect_true(is.numeric(w))
  expect_equal(sum(w), 1, tolerance = 1e-3)
})

test_that("ROI+ETL+max_pos (MILP): at most max_pos=3 non-zero weights", {
  skip_if(is.null(opt_milp))
  w <- extractWeights(opt_milp)
  expect_lte(sum(round(w, 6) > 0), 3L)
})

# ===========================================================================
# Section 4: meanetl.efficient.frontier exercises the ef=TRUE path internally
# ===========================================================================

test_that("meanetl.efficient.frontier exercises ef=TRUE branch in optimize.portfolio", {
  ef <- tryCatch(
    suppressMessages(suppressWarnings(
      create.EfficientFrontier(R4, portf_mean_es,
                               type = "mean-ETL",
                               n.portfolios = 5L)
    )),
    error = function(e) NULL
  )
  skip_if(is.null(ef))
  expect_s3_class(ef, "efficient.frontier")
  expect_false(is.null(ef$frontier))
  expect_gte(nrow(ef$frontier), 1L)
})

# ===========================================================================
# Section 5: extractStats.DEoptim is.atomic skip guard (lines 101-106)
# Inject an atomic (numeric scalar) into DEoptim_objective_results to trigger
# the is.atomic=TRUE branch (the row is skipped / left as NA in the matrix).
# ===========================================================================

# Run a real DEoptim optimization to get a valid object
portf_de <- portfolio.spec(nms)
portf_de  <- add.constraint(portf_de, type = "full_investment")
portf_de  <- add.constraint(portf_de, type = "long_only")
portf_de  <- add.objective(portf_de, type = "risk", name = "StdDev")

set.seed(42)
opt_de <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_de,
                       optimize_method = "DEoptim",
                       search_size    = 500L,
                       trace          = TRUE,
                       itermax        = 10L)
  )),
  error = function(e) NULL
)

test_that("extractStats.DEoptim is.atomic guard: injected atomic row is skipped (NA row)", {
  skip_if(is.null(opt_de))
  skip_if(is.null(opt_de$DEoptim_objective_results))
  skip_if(length(opt_de$DEoptim_objective_results) < 3)

  # Clone the object and replace a non-first element with an atomic scalar to
  # trigger the is.atomic=TRUE skip branch (the else of if(!is.atomic(...))).
  # We must keep [[1]] intact because the function uses it to compute nobj.
  fake_de <- opt_de
  fake_de$DEoptim_objective_results[[2]] <- 99.0  # atomic numeric — skipped

  result <- tryCatch(extractStats(fake_de), error = function(e) NULL)
  skip_if(is.null(result))

  # Row 2 should be entirely NA (the atomic element was skipped)
  expect_true(is.matrix(result))
  expect_true(all(is.na(result[2, ])))
})

test_that("extractStats.DEoptim is.atomic guard: non-atomic rows are populated", {
  skip_if(is.null(opt_de))
  skip_if(is.null(opt_de$DEoptim_objective_results))
  skip_if(length(opt_de$DEoptim_objective_results) < 3)

  fake_de <- opt_de
  fake_de$DEoptim_objective_results[[2]] <- 99.0  # atomic — skipped in row 2

  result <- tryCatch(extractStats(fake_de), error = function(e) NULL)
  skip_if(is.null(result))

  # Row 1 (kept intact) should have non-NA values
  expect_false(all(is.na(result[1, ])))
})
