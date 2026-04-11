# PortfolioAnalytics Comprehensive Test Suite Plan

## Overview

This document is the authoritative plan for building a comprehensive test suite
for the PortfolioAnalytics R package. It is written for use by human developers
or AI agents executing the work. All decisions, conventions, file mappings,
migration rules, and new test scope are specified here.

**Goal:** Achieve >80% code coverage as measured by the `covr` package, with
tests that run during `R CMD check` and in GitHub Actions CI, but are skipped
on CRAN.

**Parallel test execution:**
`Config/testthat/parallel: true` is set in DESCRIPTION.  The number of workers
is controlled by `TESTTHAT_CPUS`.  `tests/run-all.R` and `analyze_coverage.R`
both set `TESTTHAT_CPUS = max(2L, round(parallel::detectCores() / 2))` **only
when `NOT_CRAN=true`**.  On CRAN `TESTTHAT_CPUS` is left unset (defaults to 1),
keeping execution sequential and within CRAN's 2-core policy.  The GH Actions
coverage workflow pins `TESTTHAT_CPUS: 2` (matching the 2-vCPU free tier).

**Branch:** `feature/testing-improvements`

**Current state (Phase 8 complete, Phase 9 planning):**
- Coverage: **66.16%** (measured locally after Phase 8 via `covr::package_coverage()`)
- All tests: PASS 1,911, FAIL 0, SKIP 13 (all graceful), WARN 16 (pre-existing)
- Branch: `feature/testing-improvements`
- **Next action:** Phase 9 — targeted push from 66% → 80% (see §14)

Coverage math (from `coverage_session9.rds`):
- Total expressions: 7,711
- Currently covered: 5,366 (69.6% by expression count; covr reports 66.16% using its own weighting)
- Need for 80%: **803 more expressions** (by covr's measure)
- If all per-file targets in §14 are hit: projected **81.9%**

---

## Table of Contents

1. [Conventions and Rules](#1-conventions-and-rules)
2. [Infrastructure Setup](#2-infrastructure-setup)
3. [Phase 1 — Fix Existing Migrated File](#3-phase-1--fix-existing-migrated-file)
4. [Phase 2 — Migrate Legacy Tests](#4-phase-2--migrate-legacy-tests)
5. [Phase 3 — Man Page Examples Tests](#5-phase-3--man-page-examples-tests)
6. [Phase 4 — New Tests for Coverage Gaps](#6-phase-4--new-tests-for-coverage-gaps)
7. [Phase 5 — GitHub Actions Updates](#7-phase-5--github-actions-updates)
8. [Deprecated API Conversion Reference](#8-deprecated-api-conversion-reference)
9. [Known Bugs to Fix During Migration](#9-known-bugs-to-fix-during-migration)
10. [File Inventory and Status Tracking](#10-file-inventory-and-status-tracking)
11. [Coverage Targets by Source File](#11-coverage-targets-by-source-file)

---

## 1. Conventions and Rules

### 1.1 File Location and Naming

- All test files live in `tests/testthat/`
- File names use lowercase kebab-case: `test-{topic}.R`
- One test file per logical topic area (not one per demo script)
- The `inst/tests/` directory is the **legacy location**; files there are NOT
  discovered by modern `R CMD check`. After migration, `inst/tests/` should be
  deleted entirely.

### 1.2 Modern testthat Syntax (testthat >= 3.0)

Use ONLY modern `expect_*()` functions. Never use `expect_that()` or its helper
matchers (`equals()`, `is_true()`, `is_false()`, `is_a()`, etc.). See the full
conversion table in Section 7.

Do NOT use `context()`. It is deprecated in testthat 3.x. The file name now
serves as the context. Existing `context()` calls must be removed on migration.

### 1.3 CRAN and Dependency Guards

Every test file that performs optimization or requires a Suggested package MUST
have guards at the top of the file (not inside individual `test_that()` blocks):

```r
# At the top of every test file (after library calls):
skip_on_cran()

# Before any block that uses a Suggested package:
skip_if_not_installed("DEoptim")
skip_if_not_installed("ROI")
skip_if_not_installed("ROI.plugin.glpk")
skip_if_not_installed("ROI.plugin.quadprog")
skip_if_not_installed("quadprog")
skip_if_not_installed("CVXR")
skip_if_not_installed("corpcor")
skip_if_not_installed("Rglpk")
skip_if_not_installed("nloptr")
skip_if_not_installed("osqp")
```

Place `skip_if_not_installed()` inside a `test_that()` block or at the top of
the file, whichever is more appropriate. For files that require a single
package throughout, put it at the file top. For files with mixed requirements,
put guards inside the relevant `test_that()` blocks.

### 1.4 Stochastic Tests

Tests that use DEoptim, random portfolios, PSO, or GenSA produce non-deterministic
results. These tests should check:
- That the result object has the correct class (`expect_s3_class()`)
- That weights are numeric and sum approximately to 1 (`expect_true(is.numeric(...))`)
- That box constraints are not violated
- That objective measures are numeric (not NA/NaN/NULL)

Do NOT check exact numerical values for stochastic optimizers. Use `set.seed()`
when calling `random_portfolios()` directly to make portfolio generation
reproducible, but not when the randomness is inside `optimize.portfolio()` with
`optimize_method = "random"` or `"DEoptim"`.

### 1.5 ROI / Deterministic Tests

Tests using `optimize_method = "ROI"` (backed by quadprog, glpk, symphony) are
deterministic and SHOULD check exact numerical values with `tolerance = 1e-6`.

### 1.6 Test Structure

Prefer grouping related expectations into one `test_that()` block rather than
one expectation per block. For example, all box constraint bounds checks for a
single result object belong in one block.

Use `describe()` and `it()` only if there is a genuine hierarchy to express.
For most cases, `test_that()` is sufficient.

### 1.7 Data Fixtures

Most tests use the built-in `edhec` dataset from PerformanceAnalytics.
Define a small shared fixture at the top of each file:

```r
data(edhec)
R <- edhec[, 1:5]   # Use first 5 columns unless the test requires more
```

For tests that source demo scripts (see Section 4.2), this is handled by the
demo script itself.

### 1.8 Demo Scripts — Refactor to Inline Setup

Demo scripts are broad, exploratory scripts designed to illustrate many features
at once. They are **not** tests and should not be sourced inside test files,
even though the legacy `inst/tests/` files did so.

**The correct approach for Phase 2 migrations:**

Do NOT keep `source(system.file("demo/...", package = "PortfolioAnalytics"))`.
Instead, read the relevant demo to understand what optimization problem it sets
up, then reproduce that setup **inline** in the test file using the same data,
constraints, objectives, and solver. This makes tests:
- Faster (only runs what's needed for the test)
- More explicit (the reader understands the test without reading a separate file)
- More robust (not sensitive to changes in demo scripts)
- Better at isolating failures (a failing test pinpoints one problem)

When migrating, use the demo as a **reference document**, not as executable
code in the test. Copy the relevant `portfolio.spec`, `add.constraint`,
`add.objective`, and `optimize.portfolio` calls into the test file directly,
trimming anything not needed by the assertions.

### 1.9 Man Page Examples — Test All, Including `\dontrun{}`

All 21 Rd files with `\examples{}` blocks must be covered by
`tests/testthat/test-examples.R`. This includes the one `\dontrun{}` block in
`optimize.portfolio.rebalancing.Rd` (guard it with `skip_on_cran()` and
`skip_if_not_installed("ROI.plugin.quadprog")`).

The examples test file should:
- Execute each example block from the Rd file verbatim (or near-verbatim)
- Wrap each in a `test_that()` block with a description naming the function
- Add the minimum assertion needed to confirm the example ran correctly:
  - For portfolio spec constructors: `expect_s3_class(result, "portfolio")`
  - For constraint constructors: `expect_s3_class(result, "portfolio")`
  - For numeric outputs: `expect_true(is.numeric(result))`
  - For optimization results: `expect_s3_class(result, "optimize.portfolio.rebalancing")`
- NOT attempt to verify exact numerical values (examples are documentation,
  not regression tests)

See Section 5 for the full file specification.

---

## 2. Infrastructure Setup

✅ **Complete.** All tasks done before session 2. Summary:
- `tests/run-all.R` fixed to use `test_check()`
- `Config/testthat/edition: 3` added to `DESCRIPTION`
- `tests/testthat/helper-portfolioanalytics.R` created with shared fixtures
- `.codecov.yml` added (80% project target / 70% patch target)
- `covr` added to `Suggests` in `DESCRIPTION`

---

## Historic Phases (1 - 15)
Phases 1 through 15 have been completed successfully. This included migrating legacy inst/tests, generating testthat assertions, covering visualization modules, handling alternative MILP solvers (Rglpk), and tracking missing generic S3 coverage.
Coverage rose from **25.10% to 80.5%** during these phases.

## 16. Post-Suite Bug Fixes (Deferred)
During the test coverage push, several bugs were discovered but deferred so we could focus strictly on coverage. They should be fixed after the test suite hits its target.

*   **#49 - Rglpk solver matrix dimension mismatch in Mean-ES return target constraint:**
    When using `optimize.portfolio(..., optimize_method = "Rglpk")` with a portfolio that has both a `mean` return objective and an `ES/ETL` risk objective (Mean-ES optimization), adding a return target constraint causes a matrix dimension mismatch that corrupts the `Rglpk.mat` constraint matrix, leading the solver to fail. The `rbind` adds rows of incorrect length instead of a single target-return constraint row.
    *Location: `R/optimize.portfolio.R`, lines ~1746-1752.*

## 17. Phase 12 — Core Optimization Deep Dive (Hitting Full Coverage)

### Overview
After successfully reaching the 80% mark package-wide in Phase 11, this phase targets near-100% test coverage for the 6 core mathematical optimization and architecture files.

### 1. `optimize.portfolio.R` (Currently 71.9%)
- **Lines ~1750-2300**: MILP and Mean-ES blocks inside the `Rglpk` optimizer. (Note: These were not completely covered due to the `rbind` matrix dimension mismatch bug in the `target` return constraint branch).
- **Lines ~2400-2800**: Uncovered branches inside `osqp`, `mco`, and error-handling conditions.
- **Lines ~3330-3470**: `optimize.portfolio.rebalancing` error blocks, legacy parameter transitions, `turnover` constraints initializing from previous optimal weights.
- **Action**:
  - Add targeted unit tests inside `test-optimize-portfolio-advanced.R` for error/message branches.
  - Skip the native `Rglpk` mean-ES bug block or submit isolated test cases if feasible.
  - Add tests simulating missing solvers to hit `stop()` blocks.

### 2. `constrained_objective.R` (Currently 77.5%)
- **Missing lines**: Predominantly branches where penalties are evaluated for constraints (turnover, diversification, position limit, leverage).
- **Action**: Augment `test-constrained-objective.R` to test the internal function `constrained_objective()` explicitly with varying `w` (weight vectors) that purposefully violate `turnover`, `diversification`, and `leverage` constraints.

### 3. `moment.functions.R` (Currently 79.1%)
- **Missing lines**:
  - Error branches inside `custom.covRob` missing robust base packages.
  - Branches inside `portfolio.moments.boudt` handling robust arguments.
  - Fallbacks to `sample` estimators when parameters fail.
- **Action**: Augment `test-moment-functions-extended.R`.

### 4. `extractstats.R` (Currently 79.1%)
- **Missing lines**: Extraction routines for `GenSA`, `pso`, and `mco` when run with specific flags (like `trace=FALSE` returning NULL handling).
- **Action**: Expand `test-extract-functions.R` to cover stochastic solver results (from helper) specifically calling `extractStats`.

### 5. `constraints.R` & `objective.R` (Currently 92.1% & 82.2%)
- **Missing lines**:
  - Vector vs. scalar expansion mismatch handling in `add.constraint` and `add.objective`.
  - `update.objective` edge cases.
  - `insert_objectives` failing checks.
- **Action**: Augment `test-constraints.R` and `test-objectives.R` to cover malformed inputs (e.g. inserting non-objective objects).

