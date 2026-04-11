library(testthat)
library(PortfolioAnalytics)

# Set parallel workers when not on CRAN.
#
# CRAN policy limits packages to 2 cores in tests, and the CRAN check
# environment does not set NOT_CRAN=true.  testthat 3.x reads TESTTHAT_CPUS
# to determine how many worker processes run test files concurrently.
# We only override the default (2) in non-CRAN environments, using half the
# available logical cores (minimum 2) so the machine remains responsive.
#
# Note: Config/testthat/parallel: true must also be set in DESCRIPTION for
# parallel execution to be enabled.
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  n_cores <- max(2L, round(parallel::detectCores() / 2))
  Sys.setenv(TESTTHAT_CPUS = n_cores)
}

test_check("PortfolioAnalytics")
