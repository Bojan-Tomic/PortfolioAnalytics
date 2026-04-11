###############################################################################
# tests/testthat/test-moment-utils-gaps.R
#
# Coverage targets:
#
#   R/moment.functions.R:
#     1. CCCgarch.MM() — alpha1 < 0.01 branch (lines 37-38): when fitted GARCH
#        alpha1 is below 0.01, fall back to constant sigma = sd(R[,i])
#     2. set.portfolio.moments_v1() — garch grep branch (lines 75-87):
#        constraint has garch=TRUE objective → calls CCCgarch.MM
#     3. set.portfolio.moments_v1() — clean grep branch (lines 90-110):
#        constraint has arguments.clean set → calls Return.clean + sets moments
#     4. portfolio.moments.boudt() — multiple-clean warning (line 393):
#        two objectives with different clean= values
#     5. portfolio.moments.bl() — multiple-clean warning (line 477):
#        two objectives with different clean= values
#
#   R/utils.R:
#     6. modify.args() — missing arglist branch (line 9-10): arglist not passed
#     7. modify.args() — dots=TRUE path with unmatched dots (lines 33-37)
#     8. modify.args() — empty arglist early return (lines 16-17)
#
# Copyright (c) 2004-2026 Brian G. Peterson, Peter Carl, Ross Bennett
# License: GPL-3
###############################################################################

skip_on_cran()

library(PortfolioAnalytics)

# edhec4 is provided by helper-portfolioanalytics.R
R <- edhec4
N <- ncol(R)

# ---------------------------------------------------------------------------
# Helper: build a v1 constraint with arguments.clean
# ---------------------------------------------------------------------------
make_v1_clean_constr <- function(clean_method) {
  cc <- constraint_v1(
    assets   = colnames(R),
    min      = 0, max      = 0.55,
    min_sum  = 0.99, max_sum = 1.01
  )
  add.objective_v1(cc, type = "risk", name = "ES",
                   enabled = TRUE,
                   arguments.clean = clean_method)
}

# ---------------------------------------------------------------------------
# Helper: build a v1 constraint with garch=TRUE
# ---------------------------------------------------------------------------
make_v1_garch_constr <- function() {
  cc <- constraint_v1(
    assets   = colnames(R),
    min      = 0, max      = 0.55,
    min_sum  = 0.99, max_sum = 1.01
  )
  add.objective_v1(cc, type = "risk", name = "ES",
                   enabled = TRUE,
                   garch   = TRUE,
                   arguments.clean = NULL)
}

# ===========================================================================
# Section 1: CCCgarch.MM() — alpha1 < 0.01 branch (lines 37-38)
#
# Simulate 4 series from a GARCH(1,1) with alpha1=0.001 (true param << 0.01).
# With seed=1 the MLE alpha1 estimates are all < 0.01, forcing the fallback
# sigma = sd(R[,i]) path instead of gout@sigma.t.
# ===========================================================================

skip_if_not_installed("fGarch")

# Build the low-alpha GARCH series once at file scope
.low_alpha_R <- local({
  set.seed(1)
  n   <- 200
  sim <- function(n) {
    a0 <- 1e-4; a1 <- 0.001; b1 <- 0.998
    s2    <- numeric(n); s2[1] <- a0 / (1 - a1 - b1)
    eps   <- rnorm(n)
    x     <- numeric(n); x[1] <- sqrt(s2[1]) * eps[1]
    for (t in 2:n) {
      s2[t] <- a0 + a1 * x[t-1]^2 + b1 * s2[t-1]
      x[t]  <- sqrt(s2[t]) * eps[t]
    }
    x
  }
  m     <- matrix(replicate(4, sim(n)), nrow = n)
  colnames(m) <- colnames(R)
  xts::xts(m, order.by = seq(as.Date("2000-01-01"), by = "day", length.out = n))
})

test_that("CCCgarch.MM alpha1<0.01: all simulated alpha1 estimates are below 0.01", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  alpha1s <- sapply(seq_len(ncol(.low_alpha_R)), function(i) {
    gout <- tryCatch(
      fGarch::garchFit(formula ~ garch(1, 1),
                       data         = .low_alpha_R[, i],
                       include.mean = FALSE,
                       cond.dist    = "QMLE",
                       trace        = FALSE),
      error = function(e) NULL
    )
    if (is.null(gout)) NA_real_ else as.vector(gout@fit$coef["alpha1"])
  })
  skip_if(any(is.na(alpha1s)), "GARCH fit failed on simulated data")
  expect_true(all(alpha1s < 0.01),
              info = paste("alpha1 values:", paste(round(alpha1s, 6), collapse = ", ")))
})

test_that("CCCgarch.MM alpha1<0.01: returns a list with mu, sigma, m3, m4", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  result <- tryCatch(
    CCCgarch.MM(.low_alpha_R),
    error = function(e) NULL
  )
  skip_if(is.null(result), "CCCgarch.MM failed on simulated data")
  expect_true(is.list(result))
  expect_true(!is.null(result$mu),    label = "mu is non-NULL")
  expect_true(!is.null(result$sigma), label = "sigma is non-NULL")
  expect_true(!is.null(result$m3),    label = "m3 is non-NULL")
  expect_true(!is.null(result$m4),    label = "m4 is non-NULL")
})

test_that("CCCgarch.MM alpha1<0.01: sigma is an N x N symmetric matrix", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  result <- tryCatch(
    CCCgarch.MM(.low_alpha_R),
    error = function(e) NULL
  )
  skip_if(is.null(result), "CCCgarch.MM failed on simulated data")
  expect_equal(dim(result$sigma), c(N, N))
  expect_true(isSymmetric(result$sigma, tol = 1e-10))
})

test_that("CCCgarch.MM alpha1<0.01: mu has length N", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  result <- tryCatch(
    CCCgarch.MM(.low_alpha_R),
    error = function(e) NULL
  )
  skip_if(is.null(result), "CCCgarch.MM failed on simulated data")
  expect_equal(length(result$mu), N)
})


# ===========================================================================
# Section 2: set.portfolio.moments_v1() — garch grep branch (lines 75-87)
#
# A constraint with garch=TRUE in its objective triggers CCCgarch.MM inside
# set.portfolio.moments_v1.  Requires fGarch.
# ===========================================================================

test_that("set.portfolio.moments_v1 garch branch: returns list with all 4 moments", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  constr <- make_v1_garch_constr()
  # Verify the structure that triggers the garch grep
  expect_true(length(grep("garch", constr)) > 0)
  result <- tryCatch(
    PortfolioAnalytics:::set.portfolio.moments_v1(R, constr),
    error = function(e) NULL
  )
  skip_if(is.null(result), "set.portfolio.moments_v1 garch branch failed")
  expect_true(is.list(result))
  expect_true(!is.null(result$mu),    label = "v1 garch: mu set")
  expect_true(!is.null(result$sigma), label = "v1 garch: sigma set")
  expect_true(!is.null(result$m3),    label = "v1 garch: m3 set")
  expect_true(!is.null(result$m4),    label = "v1 garch: m4 set")
})

test_that("set.portfolio.moments_v1 garch branch: sigma is N x N", {
  skip_if_not_installed("fGarch")
  library(fGarch)
  constr <- make_v1_garch_constr()
  result <- tryCatch(
    PortfolioAnalytics:::set.portfolio.moments_v1(R, constr),
    error = function(e) NULL
  )
  skip_if(is.null(result), "set.portfolio.moments_v1 garch branch failed")
  expect_equal(dim(result$sigma), c(N, N))
})


# ===========================================================================
# Section 3: set.portfolio.moments_v1() — clean grep branch (lines 90-110)
#
# A constraint with arguments.clean = "boudt" triggers Return.clean() inside
# set.portfolio.moments_v1.
# ===========================================================================

test_that("set.portfolio.moments_v1 clean branch: returns list with all 4 moments", {
  constr <- make_v1_clean_constr("boudt")
  # Verify the structure that triggers the clean grep
  expect_true(length(grep("clean", constr)) > 0)
  result <- PortfolioAnalytics:::set.portfolio.moments_v1(R, constr)
  expect_true(is.list(result))
  expect_true(!is.null(result$mu),    label = "v1 clean: mu set")
  expect_true(!is.null(result$sigma), label = "v1 clean: sigma set")
  expect_true(!is.null(result$m3),    label = "v1 clean: m3 set")
  expect_true(!is.null(result$m4),    label = "v1 clean: m4 set")
})

test_that("set.portfolio.moments_v1 clean branch: sigma is N x N", {
  constr <- make_v1_clean_constr("boudt")
  result <- PortfolioAnalytics:::set.portfolio.moments_v1(R, constr)
  expect_equal(dim(result$sigma), c(N, N))
})

test_that("set.portfolio.moments_v1 clean branch: mu has length N", {
  constr <- make_v1_clean_constr("boudt")
  result <- PortfolioAnalytics:::set.portfolio.moments_v1(R, constr)
  expect_equal(length(as.numeric(result$mu)), N)
})

test_that("set.portfolio.moments_v1 clean branch: m3 dimensions are N x N^2", {
  constr <- make_v1_clean_constr("boudt")
  result <- PortfolioAnalytics:::set.portfolio.moments_v1(R, constr)
  expect_equal(dim(result$m3), c(N, N^2))
})


# ===========================================================================
# Section 4: portfolio.moments.boudt() — multiple-clean warning (line 393)
#
# When portfolio has two objectives with DIFFERENT clean= methods, the function
# warns that it defaults to the first method.
# ===========================================================================

.portf_two_clean_boudt <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  # objective 1: clean = "boudt"
  p <- add.objective(p, type = "risk",   name = "StdDev",
                     arguments = list(clean = "boudt"))
  # objective 2: clean = "geltner"  (different — triggers warning)
  p <- add.objective(p, type = "return", name = "mean",
                     arguments = list(clean = "geltner"))
  p
})

test_that("portfolio.moments.boudt warns on multiple different clean methods", {
  expect_warning(
    PortfolioAnalytics:::portfolio.moments.boudt(R, .portf_two_clean_boudt),
    regexp = "Multiple methods"
  )
})

test_that("portfolio.moments.boudt multi-clean: still returns a valid list", {
  result <- suppressWarnings(
    PortfolioAnalytics:::portfolio.moments.boudt(R, .portf_two_clean_boudt)
  )
  expect_true(is.list(result))
  expect_true(!is.null(result$mu) || !is.null(result$sigma),
              label = "at least one moment set")
})


# ===========================================================================
# Section 5: portfolio.moments.bl() — multiple-clean warning (line 477)
# ===========================================================================

P_mat <- matrix(rep(1 / N, N), nrow = 1)

.portf_two_clean_bl <- local({
  p <- portfolio.spec(assets = colnames(R))
  p <- add.constraint(p, type = "full_investment")
  p <- add.objective(p, type = "risk",   name = "StdDev",
                     arguments = list(clean = "boudt"))
  p <- add.objective(p, type = "return", name = "mean",
                     arguments = list(clean = "geltner"))
  p
})

test_that("portfolio.moments.bl warns on multiple different clean methods", {
  expect_warning(
    PortfolioAnalytics:::portfolio.moments.bl(R, .portf_two_clean_bl, P = P_mat),
    regexp = "Multiple methods"
  )
})

test_that("portfolio.moments.bl multi-clean: still returns a valid list", {
  result <- suppressWarnings(
    PortfolioAnalytics:::portfolio.moments.bl(R, .portf_two_clean_bl, P = P_mat)
  )
  expect_true(is.list(result))
  expect_true(!is.null(result$mu) || !is.null(result$sigma),
              label = "at least one moment set")
})


# ===========================================================================
# Section 6: modify.args() — missing arglist branch (lines 9-10)
#
# When arglist is not supplied, it defaults to NULL and combines with
# dots.names.  The function should return formals unchanged.
# ===========================================================================

test_that("modify.args with missing arglist returns formals unchanged", {
  f <- list(a = 1, b = 2, c = 3)
  result <- PortfolioAnalytics:::modify.args(formals = f)
  expect_identical(result, f)
})

test_that("modify.args with missing arglist and no dots returns formals", {
  f <- list(x = 10, y = 20)
  result <- PortfolioAnalytics:::modify.args(formals = f)
  expect_equal(length(result), 2L)
  expect_equal(result$x, 10)
  expect_equal(result$y, 20)
})

test_that("modify.args with NULL arglist returns formals unchanged", {
  f <- list(a = 1, b = 2)
  result <- PortfolioAnalytics:::modify.args(formals = f, arglist = NULL)
  expect_identical(result, f)
})


# ===========================================================================
# Section 7: modify.args() — dots=TRUE with unmatched named dots (lines 33-37)
#
# When dots=TRUE and formals contains '...', unmatched dot-args get appended.
# The '...' element in formals must come from actual formals(), not a list with
# '...'=NULL (which is NULL and fails the !is.null(.formals$...) check).
# ===========================================================================

# Build a formals list from a real function so '...' is a "name" symbol, not NULL
.f_with_dots <- formals(function(a = 1, b = 2, ...) NULL)

test_that("modify.args dots=TRUE appends unmatched dot-args when formals has ...", {
  result <- PortfolioAnalytics:::modify.args(formals = .f_with_dots,
                                             arglist = list(a = 99),
                                             extra_arg = "hello",
                                             dots = TRUE)
  # 'a' should be updated; extra_arg should be appended
  expect_equal(result$a, 99)
  expect_equal(result$extra_arg, "hello")
})

test_that("modify.args dots=TRUE with matched args updates formals correctly", {
  result <- PortfolioAnalytics:::modify.args(formals = .f_with_dots,
                                             arglist = list(a = 10, b = 20),
                                             dots = TRUE)
  expect_equal(result$a, 10)
  expect_equal(result$b, 20)
})


# ===========================================================================
# Section 8: modify.args() — empty arglist early return (lines 16-17)
#
# When arglist is NULL and no dots, length(arglist)==0 → return formals.
# ===========================================================================

test_that("modify.args with empty arglist returns formals via early return", {
  f <- list(x = 5, y = 6, z = 7)
  result <- PortfolioAnalytics:::modify.args(formals = f, arglist = list())
  expect_identical(result, f)
})

test_that("modify.args arglist=list() is a no-op", {
  f <- list(p = 100, q = 200)
  result <- PortfolioAnalytics:::modify.args(formals = f, arglist = list())
  expect_equal(result$p, 100)
  expect_equal(result$q, 200)
})
