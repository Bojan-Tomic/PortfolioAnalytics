## Tests for issue #41: position_limit constraint with max_pos_long > nassets
## causes "cannot take a sample larger than the population" error in
## random_portfolios_v2 with rp_method='sample'.
##
## Root cause: rp_transform() line 497 calls sample(1:length(tmp_w), max_pos)
## without replace=TRUE when max_pos > length(tmp_w).
## Fix: clamp max_pos to min(max_pos, length(tmp_w)) at the top of rp_transform().

library(PortfolioAnalytics)

skip_if_not_installed("PortfolioAnalytics")

# Helper: 4-asset portfolio with leverage + box constraints
make_portfolio <- function() {
  assets <- c("stock1", "stock2", "stock3", "stock4")
  pspec <- portfolio.spec(assets = assets)
  pspec <- add.constraint(pspec, type = "leverage",
                          min_sum = 0.98, max_sum = 1.02)
  pspec <- add.constraint(pspec, type = "box", min = 0, max = 1)
  pspec
}

test_that("random_portfolios_v2 works when max_pos_long > nassets (#41)", {
  skip_on_cran()
  pspec <- make_portfolio()
  # max_pos_long = 70 >> 4 assets -- should not error
  pspec <- add.constraint(pspec, type = "position_limit", max_pos_long = 70)
  expect_no_error(
    rp <- random_portfolios_v2(portfolio = pspec,
                               permutations = 100,
                               rp_method = "sample",
                               eliminate = TRUE)
  )
  expect_true(is.matrix(rp))
  expect_equal(ncol(rp), 4L)
})

test_that("random_portfolios_v2 works when max_pos > nassets (#41)", {
  skip_on_cran()
  pspec <- make_portfolio()
  # Explicit max_pos larger than number of assets
  pspec <- add.constraint(pspec, type = "position_limit", max_pos = 100)
  expect_no_error(
    rp <- random_portfolios_v2(portfolio = pspec,
                               permutations = 50,
                               rp_method = "sample",
                               eliminate = TRUE)
  )
  expect_true(is.matrix(rp))
})

test_that("random_portfolios_v2 still enforces a binding max_pos (#41)", {
  skip_on_cran()
  pspec <- make_portfolio()
  # max_pos = 2 with 4 assets: at most 2 non-zero weights per row
  pspec <- add.constraint(pspec, type = "position_limit", max_pos = 2)
  rp <- random_portfolios_v2(portfolio = pspec,
                             permutations = 50,
                             rp_method = "sample",
                             eliminate = TRUE)
  # Each row should have at most 2 non-zero weights
  nonzero_per_row <- apply(rp, 1, function(w) sum(abs(w) > .Machine$double.eps^0.5))
  expect_true(all(nonzero_per_row <= 2L))
})

test_that("rp_transform does not error when max_pos > length(w) (#41)", {
  skip_on_cran()
  w <- c(0.25, 0.25, 0.25, 0.25)
  names(w) <- paste0("a", 1:4)
  expect_no_error(
    out <- rp_transform(w      = w,
                        min_sum = 0.99,
                        max_sum = 1.01,
                        min_box = rep(0, 4),
                        max_box = rep(1, 4),
                        max_pos = 50)
  )
  expect_equal(length(out), 4L)
  expect_true(sum(out) >= 0.99 && sum(out) <= 1.01)
})
