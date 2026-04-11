# PortfolioAnalytics Issue Resolution Plan

## Overview

Address open GitHub issues in PortfolioAnalytics and improve test coverage.

- Use `gh` CLI to interact with GitHub
- Track progress by editing this file
- Create a feature branch per issue
- Write a failing test *before* making code changes
- Enhance test coverage for poorly covered areas
- Squash-merge each feature branch with a commit referencing the issue

**Current state (as of 2026-04-11):** All active bugs resolved and merged to
master (15 commits ahead of origin as of last push `b9198d2`). Chart export
fix also complete. Support questions #29, #47 closed. Out-of-scope issues
#15, #48 closed and redirected to PerformanceAnalytics. Feature requests #43,
#45 and time-varying factor exposure request #42 remain open. Issue #14
(multiplier=0) clarified as expected behavior.

---

## Table of Contents

1. [Conventions and Rules](#1-conventions-and-rules)
2. [Issue Triage Summary](#2-issue-triage-summary)
3. [Completed Work](#3-completed-work)
4. [Coverage Improvement Plan](#4-coverage-improvement-plan)
5. [Remaining Open Items](#5-remaining-open-items)

---

## 1. Conventions and Rules

### 1.1 File Location and Naming

- All test files live in `tests/testthat/`
- File names use lowercase kebab-case: `test-{topic}.R`
- One test file per logical topic area

### 1.2 Modern testthat Syntax (testthat >= 3.0)

Use ONLY modern `expect_*()` functions. No `expect_that()`, no `context()`.

### 1.3 CRAN and Dependency Guards

Every test file that performs optimization or requires a Suggested package MUST
have `skip_on_cran()` and appropriate `skip_if_not_installed()` guards.

### 1.4 Stochastic Tests

Do NOT check exact numerical values for stochastic optimizers. Check structure,
classes, constraint satisfaction, and that objectives are numeric.

### 1.5 ROI / Deterministic Tests

Tests using `optimize_method = "ROI"` are deterministic and SHOULD check exact
numerical values with `tolerance = 1e-6`.

### 1.6 Test-First Workflow Per Issue

1. Create feature branch: `fix/issue-NN-short-description`
2. Write test(s) that reproduce the bug (expect failure)
3. Fix the code
4. Confirm tests pass
5. Add additional coverage tests if area is poorly covered
6. Squash-merge to master with message referencing `#NN`

### 1.7 Git Commit Messages

Short, imperative subject line (~50 chars). Body only when useful.
ALWAYS Reference the GitHub issue number. Use "Fixes #NN" to auto-close,
or "Refs #NN" if issue remains open.

### 1.8 Coverage Runs

- Run `covr::package_coverage()` with `TESTTHAT_CPUS=1` to prevent the
  parallel test runner from interfering with covr's function-level tracing.
- This is slow (10–20 min locally). Be patient; do not time-limit the run.
- GHA coverage workflow (`coverage.yml`) uses `TESTTHAT_CPUS: 2` and
  `fail_ci_if_error: false`; it completes because the GHA runner has no wall
  clock limit on that step.

---

## 2. Issue Triage Summary

| # | Title | Status | Category |
|---|-------|--------|----------|
| 1 | multi layer spec + rebalancing | **CLOSED** — regression test added | Bug |
| 2 | asset names not set in regime portfolio | **CLOSED** — regression test added | Bug |
| 3 | geometric chaining with negative weights | **CLOSED** — regression test added | Bug |
| 4 | New ROI version causes errors | **CLOSED** — regression test added | Bug |
| 5 | GenSA temperature arg name | **CLOSED** — regression test added | Bug |
| 6 | GenSA ignores rp | **CLOSED** — regression test added | Bug |
| 7 | DEoptim parallelType via `...` | **CLOSED** — fixed + tested (`c7ec3e4`) | Bug |
| 10 | Custom moments vignette / sigma.robust | **CLOSED** — helpful error added (`74a83c7`) | Bug |
| 12 | random_portfolios fails w/o box constraints | **CLOSED** — fixed + tested (`9b9cc54`) | Bug |
| 13 | Errors in several demos (old constraint API) | **CLOSED** — demos updated (`9da9280`) | Bug |
| 14 | multiplier=0 (ROI ignores multiplier) | **Commented** — expected stochastic behavior, not a bug | Bug |
| 15 | Return.clean boudt single column | **CLOSED** — redirected to PerformanceAnalytics | Out of scope |
| 20 | name="mean" crossprod error | **CLOSED** — fixed + tested (`6941922`) | Bug |
| 22 | Parallel rebalancing + custom momentFUN | **CLOSED** — fixed + tested (`03fdcda`) | Bug |
| 24 | itermax variable class issue | **CLOSED** — fixed + tested (`e5ccf82`) | Bug |
| 25 | data.frame causes rebalancing crash | **CLOSED** — fixed + tested (`bf49413`) | Bug |
| 26 | Max Sharpe Ratio xmin/xmax error | **CLOSED** — fixed + tested (`7f91dcb`) | Bug |
| 27 | training_period NULL + rolling_window (v1) | **CLOSED** — fixed + tested (`bf49413`) | Bug |
| 29 | Fixed income portfolio guidance | **CLOSED** — helpful comment posted | Support |
| 30 | Rebalancing weights not summing to 1 | **CLOSED** — not reproducible, comment posted | Bug |
| 31 | ROI fails for specific edhec period | **CLOSED** — not reproducible, comment posted | Bug |
| 36 | CVXR optimize_method length-2 vector | **CLOSED** — regression test added (`d70e1ab`) | Bug |
| 41 | position_limit + random_portfolios sample | **CLOSED** — fixed + tested (`95ffd96`) | Bug |
| 42 | Time-varying factor exposure question | **Open** — workaround comment posted | Feature request |
| 43 | [FR] Parallelization level control | **Open** — non-trivial design required | Feature request |
| 44 | CVXR division by near-zero sum | **CLOSED** — fixed + tested (`22bef28`) | Bug |
| 45 | [FR] Return solver failure info | **Open** — non-trivial design required | Feature request |
| 47 | Max ES Ratio + Sortino cov matrix | **CLOSED** — usage comment posted | Support |
| 48 | DownsideDeviation time-varying MAR | **CLOSED** — redirected to PerformanceAnalytics | Out of scope |
| 49 | Rglpk matrix dimension mismatch | **CLOSED** — fixed + tested (`3875705`) | Bug |

---

## 3. Completed Work

### 3.1 Bug Fixes (all merged to master, pushed to origin)

| Commit | Description |
|--------|-------------|
| `d70e1ab` | Add regression tests for already-fixed issues (closes #1 #2 #3 #4 #5 #6 #30 #31 #36) |
| `e5ccf82` | Fix itermax/maxit/parallel variable args (closes #24) |
| `bf49413` | Fix data.frame crash + missing training_period fallback (closes #25 #27) |
| `c7ec3e4` | Remove dead names(dotargs) assignments (closes #7) |
| `9b9cc54` | Fix random_portfolios crash with no box constraint (closes #12) |
| `6941922` | Fix NULL mu in port.mean; fix ifelse crash in random loop (closes #20) |
| `03fdcda` | Export custom momentFUN to foreach parallel workers (closes #22) |
| `7f91dcb` | Guard min_mean >= max_mean in max_sr_opt and mean_etl_opt (closes #26) |
| `95ffd96` | Clamp max_pos to length(w) in rp_transform (closes #41) |
| `22bef28` | Guard CVXR near-zero-sum division (closes #44) |
| `3875705` | Correct Rglpk return-target constraint matrix dimensions (closes #49) |
| `0a51927` | Expand test coverage for #7 #12 #20 #22 #24 #25 #26 #27 |
| `74a83c7` | Replace cryptic stopifnot with helpful ROI plugin error (closes #10) |
| `9da9280` | Fix demo scripts using old constraint() API; fix SDRB.portf typo (closes #13) |
| `b9198d2` | Export chart.Weight/Scatter/charts functions for all solver types (chart test fix) |

### 3.2 GitHub Issues Closed (no code change needed)

- **#48** — DownsideDeviation time-varying MAR: closed, redirected to PerformanceAnalytics
- **#15** — Return.clean boudt single column: closed, redirected to PerformanceAnalytics
- **#29** — Fixed income portfolio support question: closed with helpful comment
- **#47** — Max ES Ratio + Sortino cov support question: closed with usage tips
- **#42** — Time-varying factor exposure: workaround comment posted, left open as FR
- **#14** — multiplier=0 behavior: explained as expected RNG/stochastic behavior

### 3.3 Key Discoveries

- **Tests run against the installed package**, not source. Must run
  `R CMD INSTALL . --no-test-load` after any code change before testing.
- **`testthat::test_dir()` crashes with parallel runner** in this environment.
  Use per-file `testthat::test_file()` calls instead.
- **`constraint_v1()` returns class `"v1_constraint"/"constraint"`**, not
  `"portfolio"`. Tests must use `expect_s3_class(pobj, "constraint")`.
- **Chart test failures root cause**: chart functions were defined in R source
  but lacked `@export` roxygen tags → not exported from NAMESPACE. Fixed in
  `b9198d2` by adding `@export` to 15 functions and regenerating NAMESPACE.
- **Why GHA passed despite failures**: The R-CMD-check workflow does NOT set
  `NOT_CRAN=true`, so `skip_on_cran()` fires. The coverage workflow sets
  `NOT_CRAN=true` but has `fail_ci_if_error: false`.
- **NAMESPACE is roxygen2-generated**. Must add `@export` roxygen tags and
  regenerate via `roxygen2::roxygenise()`.
- **Custom momentFUN functions must be in `.GlobalEnv`** for
  `optimize.portfolio.rebalancing` to find them in parallel workers.
- **covr::package_coverage() must run with `TESTTHAT_CPUS=1`** locally to
  prevent parallel test runner from corrupting function-level tracing. The GHA
  workflow uses `TESTTHAT_CPUS: 2` but runs only one coverage job so
  cross-process trace corruption doesn't occur.

---

## 4. Coverage Improvement Plan

### 4.1 Static Analysis Results (2026-04-11)

Function-level static analysis: **~64% (149/232 functions touched by tests)**.
Note: S3 dispatch calls (e.g. `extractStats(roi_obj)`) are missed by static
grep analysis — actual runtime coverage is higher.

**Priority areas for new tests:**

| Area | File | Gap | Priority |
|------|------|-----|----------|
| print/summary S3 methods | `generics.R` | 17/22 uncovered | High |
| extractStats/Weights/ObjMeasures S3 | `extractstats.R` | 22/29 uncovered | High |
| constraint constructors | `constraints.R` | 5/18 uncovered | High |
| objective constructors | `objective.R` | 7/11 uncovered | Medium |
| stat factor model helpers | `stat.factor.model.R` | 8/13 uncovered | Medium |
| charts.multiple S3 (opt.list) | `charts.multiple.R` | 3/3 uncovered* | Medium |
| chart.RiskBudget variants | `charts.risk.R` | 2/5 uncovered | Low |

*Note: `chart.Weights.opt.list` and `chart.RiskReward.opt.list` ARE tested
via S3 dispatch in `test-charts-rp.R` — static analysis false negative.

### 4.2 New Test Files to Add

- [ ] `test-generics-print-summary.R` — print/summary S3 methods
- [ ] `test-extract-stats-methods.R` — extractStats/Weights/ObjMeasures per solver type
- [ ] `test-constraint-constructors.R` — diversification, return, position_limit,
      transaction_cost, leverage_exposure constraints
- [ ] `test-objective-constructors.R` — risk_budget, turnover, minmax,
      quadratic_utility, weight_concentration, is.objective, insert_objectives
- [ ] `test-stat-factor-model-helpers.R` — covarianceSF/MF, coskewness, cokurtosis

---

## 5. Remaining Open Items

### 5.1 Feature Requests (non-trivial, deferred)

- **#43** — Parallelization level control for rebalancing (needs design)
- **#45** — Return solver failure info from rebalancing (needs design)
- **#42** — Time-varying factor exposure (user FR, workaround documented)

### 5.2 Coverage Work

See Section 4.2 above. Run `covr::package_coverage()` with `TESTTHAT_CPUS=1`
to get a baseline, then add tests for priority areas.
