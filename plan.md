# PortfolioAnalytics Plan

> **History note:** All completed bug fixes, issue triage, and earlier coverage
> work are recorded in commit `6e5ef96` and earlier. The coverage test files
> added after that point are in commits `bc28c87`–`1174ee0`.

---

## Conventions and Rules

- All test files live in `tests/testthat/`, named `test-{topic}.R`
- Use only modern testthat 3.x syntax — no `context()`, no `expect_that()`
- Every test file must have `skip_on_cran()` and `skip_if_not_installed()` guards
- Do NOT check exact numerical values for stochastic optimizers
- Tests run against the **installed** package — run `R CMD INSTALL . --no-test-load` after any source change
- Run tests with `testthat::test_file()` — `test_dir()` crashes with the parallel runner (`Config/testthat/parallel: true` is set in DESCRIPTION)
- Run coverage with `TESTTHAT_CPUS=1` to prevent parallel runner from corrupting covr tracing
- Never run `git push` — only `git add` and `git commit`

---

## Remaining Open Items

### Feature Requests (deferred — non-trivial design needed)

- **#43** — Parallelization level control for rebalancing
- **#45** — Return solver failure info from rebalancing
- **#42** — Time-varying factor exposure (workaround documented in issue)

### Coverage Work

Run `covr::package_coverage(quiet=FALSE)` with `TESTTHAT_CPUS=1` externally to
identify the next set of coverage gaps, then add targeted test files here.

Known areas not yet exercised by any test (as of `1174ee0`):
- `extract_risk()` — file written (`test-extract-risk.R`) but all tests skip
  because CLARABEL is not installed in this environment
