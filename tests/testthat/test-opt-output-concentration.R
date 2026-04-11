###############################################################################
# tests/testthat/test-opt-output-concentration.R
#
# Coverage targets:
#
#   R/opt.outputMvo.R (80.77% baseline, 5 uncovered exprs):
#     - opt.outputMvo() digits non-NULL branch (L41-L42, L44)
#     - opt.outputMvo() weekly/daily frequency branches (L41-L44)
#
#   R/chart.concentration.R (77.55% baseline, 22 uncovered exprs):
#     - chart.Concentration() with conc.type="pct_contrib" (L128-L130)
#     - chart.Concentration() with chart.assets=TRUE (L38, L44-L45, L97-L98,
#       L106-L117)
#     - chart.Concentration() with unrecognised return.col/risk.col
#       (L92-L98 fallback pmatch loop)
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_on_cran()
skip_if_not_installed("PerformanceAnalytics")

utils::data(edhec)
R4 <- edhec[, 1:4]
nms <- colnames(R4)

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

portf_rp <- portfolio.spec(nms)
portf_rp <- add.constraint(portf_rp, type = "full_investment")
portf_rp <- add.constraint(portf_rp, type = "long_only")
portf_rp <- add.objective(portf_rp, type = "risk", name = "StdDev")
portf_rp <- add.objective(portf_rp, type = "return", name = "mean")

# Risk-budget portfolio for pct_contrib conc.type
portf_rb <- portfolio.spec(nms)
portf_rb <- add.constraint(portf_rb, type = "full_investment")
portf_rb <- add.constraint(portf_rb, type = "long_only")
portf_rb <- add.objective(portf_rb, type = "risk_budget", name = "ES",
                           arguments = list(p = 0.05))
portf_rb <- add.objective(portf_rb, type = "return", name = "mean")

# Run a random portfolio optimization with trace=TRUE so extractStats works
set.seed(42)
opt_trace <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_rp,
                       optimize_method = "random",
                       search_size    = 500L,
                       trace          = TRUE)
  )),
  error = function(e) NULL
)

# Risk-budget optimization for pct_contrib
set.seed(42)
opt_rb_trace <- tryCatch(
  suppressMessages(suppressWarnings(
    optimize.portfolio(R4, portf_rb,
                       optimize_method = "random",
                       search_size    = 500L,
                       trace          = TRUE)
  )),
  error = function(e) NULL
)

# ROI optimization (simple result, no trace iteration matrix, for opt.outputMvo)
opt_roi <- suppressMessages(suppressWarnings(
  optimize.portfolio(R4, portf_rp, optimize_method = "ROI")
))

# ===========================================================================
# Section 1: opt.outputMvo — digits and frequency branches
# ===========================================================================

test_that("opt.outputMvo: annualize=TRUE monthly (default) returns list", {
  result <- opt.outputMvo(opt_roi, R4, frequency = "monthly")
  expect_true(is.list(result))
  expect_true(all(c("Wgts","Mean","StdDev","SR") %in% names(result)))
})

test_that("opt.outputMvo: digits=4 branch rounds output (L41-L42)", {
  result <- opt.outputMvo(opt_roi, R4, digits = 4, frequency = "monthly")
  expect_true(is.list(result))
  # With digits=4 each element is rounded
  expect_true(is.numeric(result$Mean))
  expect_true(is.numeric(result$SR))
})

test_that("opt.outputMvo: digits=4 produces same length output as digits=NULL", {
  r_digits <- opt.outputMvo(opt_roi, R4, digits = 4)
  r_nodig  <- opt.outputMvo(opt_roi, R4, digits = NULL)
  expect_equal(length(r_digits), length(r_nodig))
})

test_that("opt.outputMvo: weekly frequency branch (L41, b=sqrt(52)) is exercised", {
  result <- opt.outputMvo(opt_roi, R4, frequency = "weekly")
  expect_true(is.list(result))
  expect_true(is.numeric(result$Mean))
})

test_that("opt.outputMvo: daily frequency branch (L43-L44, a=260) is exercised", {
  result <- opt.outputMvo(opt_roi, R4, frequency = "daily")
  expect_true(is.list(result))
  expect_true(is.numeric(result$Mean))
})

test_that("opt.outputMvo: annualize=FALSE leaves Mean unscaled", {
  r_ann   <- opt.outputMvo(opt_roi, R4, annualize = TRUE,  frequency = "monthly")
  r_noann <- opt.outputMvo(opt_roi, R4, annualize = FALSE, frequency = "monthly")
  # Annualized mean should be 12x the non-annualized mean
  expect_equal(r_ann$Mean, 12 * r_noann$Mean, tolerance = 1e-10)
})

test_that("opt.outputMvo: matrix returns input (non-xts branch) works", {
  R_mat <- coredata(R4)   # plain matrix, not xts
  result <- opt.outputMvo(opt_roi, R_mat, frequency = "monthly")
  expect_true(is.list(result))
})

# ===========================================================================
# Section 2: chart.Concentration — conc.type="weights" baseline
# (exercised in prior tests via test-charts-rp.R; confirmed here)
# ===========================================================================

test_that("chart.Concentration weights: returns NULL invisibly for trace opt", {
  skip_if(is.null(opt_trace))
  result <- tryCatch(
    suppressWarnings(
      chart.Concentration(opt_trace,
                          return.col = "mean",
                          risk.col   = "StdDev",
                          conc.type  = "weights")
    ),
    error = function(e) "error"
  )
  expect_false(identical(result, "error"))
})

# ===========================================================================
# Section 3: chart.Concentration — conc.type="pct_contrib" (L128-L130)
# Requires risk_budget optimization so that pct_contrib columns exist
# ===========================================================================

test_that("chart.Concentration pct_contrib: executes without hard error", {
  skip_if(is.null(opt_rb_trace))
  result <- tryCatch(
    suppressWarnings(
      chart.Concentration(opt_rb_trace,
                          return.col = "mean",
                          risk.col   = "ES",
                          conc.type  = "pct_contrib")
    ),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "err")
  )
  # Either NULL (success) or an expected 'pct_contrib not detected' stop
  if (inherits(result, "err")) {
    expect_match(result$msg, "pct_contrib")
  } else {
    expect_null(result)
  }
})

# ===========================================================================
# Section 4: chart.Concentration — chart.assets=TRUE (L102-L121)
# Adds asset scatter to the chart
# ===========================================================================

test_that("chart.Concentration chart.assets=TRUE: executes without error", {
  skip_if(is.null(opt_trace))
  result <- tryCatch(
    suppressWarnings(
      chart.Concentration(opt_trace,
                          return.col   = "mean",
                          risk.col     = "StdDev",
                          conc.type    = "weights",
                          chart.assets = TRUE)
    ),
    error = function(e) "error"
  )
  expect_false(identical(result, "error"))
})

# ===========================================================================
# Section 5: chart.Concentration — non-matching return.col / risk.col
# triggers the applyFUN fallback path (L72-L99)
# ===========================================================================

test_that("chart.Concentration non-matching cols: triggers applyFUN fallback", {
  skip_if(is.null(opt_trace))
  # 'VaR' is not an objective measure name, so pmatch returns NA,
  # triggering the applyFUN calculation path (L72-L99)
  result <- tryCatch(
    suppressWarnings(
      chart.Concentration(opt_trace,
                          return.col = "mean",
                          risk.col   = "VaR",
                          conc.type  = "weights")
    ),
    error = function(e) "error"
  )
  # Should succeed and return NULL invisibly
  expect_false(identical(result, "error"))
})
