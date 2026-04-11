# PortfolioAnalytics Issue Resolution Plan

## Overview

Address open GitHub issues in PortfolioAnalytics.

- Use `gh` CLI to interact with GitHub
- Track progress by editing this file
- Create a feature branch per issue
- Write a failing test *before* making code changes
- Enhance test coverage for poorly covered areas
- Squash-merge each feature branch with a commit referencing the issue

**Current state:** Group "Already Fixed" complete — #1-6, #30, #31, #36 closed with regression tests (commit 877895f). Working on Group A active bugs.

---

## Table of Contents

1. [Conventions and Rules](#1-conventions-and-rules)
2. [Issue Triage Summary](#2-issue-triage-summary)
3. [Bugs — Already Fixed (close with tests)](#3-bugs--already-fixed-close-with-tests)
4. [Bugs — Active](#4-bugs--active)
5. [Feature Requests](#5-feature-requests)
6. [Support Request Responses](#6-support-request-responses)
7. [Out of Scope (PerformanceAnalytics)](#7-out-of-scope-performanceanalytics)

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
ALWAYS Reference the GitHub issue number. Use "Fixes #NN" to auto-close, or "Refs #NN" if issue remains open.

---

## 2. Issue Triage Summary

| # | Title | Status | Category |
|---|-------|--------|----------|
| 1 | multi layer spec + rebalancing | Already fixed | Close w/ tests |
| 2 | asset names not set in regime portfolio | Already fixed | Close w/ tests |
| 3 | geometric chaining with negative weights | Already fixed | Close w/ tests |
| 4 | New ROI version causes errors | Already fixed | Close w/ tests |
| 5 | GenSA temperature arg name | Already fixed | Close w/ tests |
| 6 | GenSA ignores rp | Already fixed | Close w/ tests |
| 7 | DEoptim parallelType via `...` | Active bug | Bug |
| 10 | Custom moments vignette / sigma.robust | Active bug | Bug |
| 12 | random_portfolios fails w/o box constraints | Active bug | Bug |
| 13 | Errors in several demos (old constraint API) | Active bug | Bug |
| 14 | multiplier=0 (ROI ignores multiplier) | Active bug (partial) | Bug |
| 15 | Return.clean boudt single column | PerformanceAnalytics (fixed) | Out of scope |
| 20 | name="mean" crossprod error | Active bug | Bug |
| 22 | Parallel rebalancing + custom momentFUN | Active bug | Bug |
| 24 | itermax variable class issue | Active bug | Bug |
| 25 | data.frame causes rebalancing crash | Active bug | Bug |
| 26 | Max Sharpe Ratio xmin/xmax error | Active bug | Bug |
| 27 | training_period NULL + rolling_window (v1) | Active bug (v1 only) | Bug |
| 29 | Fixed income portfolio guidance | Support question | Defer |
| 30 | Rebalancing weights not summing to 1 | Not reproducible | Close |
| 31 | ROI fails for specific edhec period | Not reproducible | Close |
| 36 | CVXR optimize_method length-2 vector | Already fixed | Close w/ tests |
| 41 | position_limit + random_portfolios sample | Active bug | Bug |
| 42 | Time-varying factor exposure question | Support question | Defer |
| 43 | [FR] Parallelization level control | Feature request | FR |
| 44 | CVXR division by near-zero sum | Active bug | Bug |
| 45 | [FR] Return solver failure info | Feature request | FR |
| 47 | Max ES Ratio + Sortino cov matrix | Support question | Defer |
| 48 | DownsideDeviation time-varying MAR | PerformanceAnalytics (partial) | Out of scope |
| 49 | Rglpk matrix dimension mismatch | Active bug | Bug |

---

## 3. Bugs — Already Fixed (close with tests)

Verify fix, add regression tests, close issue.

### 3.1 #4 — ROI `as.constraint` breakage
- **Status:** Fixed by ROI interface rewrite (uses `ROI::OP()` directly now)
- **Action:** Confirm existing tests cover this; close issue with comment
- [ ] Done

### 3.2 #5 — GenSA `temp` vs `temperature`
- **Status:** Fixed — param correctly named `temperature` at lines 430, 1351
- **Action:** Add regression test; close issue
- [ ] Done

### 3.3 #6 — GenSA ignores `rp`
- **Status:** Fixed — `if (!is.null(rp)) par <- rp[, 1]` at lines 448, 1369
- **Action:** Add regression test; close issue
- [ ] Done

### 3.4 #1 — multi layer spec + rebalancing
- **Status:** Fixed — correct portfolio object passed at lines 747, 3367
- **Action:** Add regression test if feasible; close issue
- [ ] Done

### 3.5 #2 — asset names in regime portfolio
- **Status:** Fixed — `names(assets)` set at `R/portfolio.R:165`
- **Action:** Add regression test; close issue
- [ ] Done

### 3.6 #3 — geometric chaining with negative weights
- **Status:** Fixed — detection at `R/mult.layer.portfolio.R:158`
- **Action:** Add regression test; close issue
- [ ] Done

### 3.7 #36 — CVXR length-2 optimize_method
- **Status:** Fixed — length check at lines 755, 3415
- **Action:** Add regression test; close issue
- [ ] Done

### 3.8 #30 — Rebalancing weights not summing to 1
- **Status:** Not reproducible with current code
- **Action:** Close with comment explaining ROI enforces constraints
- [ ] Done

### 3.9 #31 — ROI fails for specific edhec period
- **Status:** Not reproducible with current code
- **Action:** Close with comment; add test for that date range
- [ ] Done

---

## 4. Bugs — Active

Grouped by area of the codebase for efficient context sharing.

### Group A: optimize.portfolio.R core dispatch

#### 4.1 #7 — DEoptim `parallelType` from `...` not propagated
- **File:** `R/optimize.portfolio.R:113-117, 901-905`
- **Root cause:** `names(dotargs[pm > 0L])` modifies a copy, not `dotargs`
- **Fix:** Use `names(dotargs)[pm > 0L] <- DEcargs[pm]` or rewrite pmatch block
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.2 #24 — `itermax` variable class from `match.call()`
- **File:** `R/optimize.portfolio.R:95-96, 883-884`
- **Root cause:** `match.call()$itermax` returns unevaluated expression
- **Fix:** Use `eval()` or extract from `list(...)$itermax`
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.3 #25 — `data.frame` input crashes rebalancing
- **File:** `R/optimize.portfolio.R:3321+`
- **Root cause:** No `checkData(R)` call in `optimize.portfolio.rebalancing`
- **Fix:** Add `R <- checkData(R)` after early-return blocks
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.4 #27 — `training_period` NULL + `rolling_window` (v1 path)
- **File:** `R/optimize.portfolio.R:3183`
- **Root cause:** v1 unconditionally defaults training_period=36
- **Fix:** Add the same check that v2 has at line 3435
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.5 #22 — Parallel rebalancing ignores custom `momentFUN`
- **File:** `R/optimize.portfolio.R:3191,3198,3458,3467,3479,3486`
- **Root cause:** `foreach` calls lack `.export` for custom functions
- **Fix:** Detect custom functions in `...` and add to `.export`
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

### Group B: Objective / constraint calculation

#### 4.6 #12 — `random_portfolios` fails without box constraints
- **File:** `R/random_portfolios.R:243-245`, `R/constraint_fn_map.R:53-54`
- **Root cause:** `-Inf`/`Inf` from missing box constraints → `seq()` error
- **Fix:** Guard `generatesequence()` inputs; default to sensible min/max
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.7 #14 — `multiplier=0` ignored by ROI
- **File:** ROI path in `R/optimize.portfolio.R` (documented limitation)
- **Root cause:** ROI builds QP/LP directly, doesn't use multiplier
- **Action:** May be "won't fix" — document better, or apply post-solve
- [ ] Investigate feasibility
- [ ] Decision + action
- [ ] Done

#### 4.8 #20 — `name="mean"` crossprod error with missing `mu`
- **File:** `R/constrained_objective.R:572-576`, `R/objectiveFUN.R:68-71`
- **Root cause:** `port.mean(weights, mu)` fails when `mu` is NULL
- **Fix:** Guard for NULL mu; provide helpful error message
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.9 #41 — `position_limit` + `random_portfolios` sample method
- **File:** `R/random_portfolios.R` sample path
- **Root cause:** Seed portfolios violate position_limit, get eliminated
- **Fix:** Generate position-limit-aware seed portfolios
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

### Group C: Solver-specific bugs

#### 4.10 #26 — Max Sharpe Ratio `optimize()` xmin/xmax
- **File:** `R/optFUN.R:1338-1365`
- **Root cause:** No guard for `min_mean >= max_mean` before `optimize()`
- **Fix:** Check and handle degenerate case
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.11 #44 — CVXR division by near-zero `sum(cvxr_wts)`
- **File:** `R/optimize.portfolio.R:3091-3094`
- **Root cause:** No guard before `cvxr_wts / sum(cvxr_wts)`
- **Fix:** Check for near-zero sum; return error/warning
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.12 #49 — Rglpk matrix dimension mismatch
- **File:** `R/optimize.portfolio.R:1622-1627, 1748-1754`
- **Root cause:** Return target row has N cols, matrix has N+T+1 cols
- **Fix:** Pad the return target row with zeros for auxiliary variables
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

### Group D: Demos / vignettes

#### 4.13 #10 — Custom moments vignette unhelpful error
- **File:** `R/moment.functions.R` (stopifnot/requireNamespace pattern)
- **Root cause:** `stopifnot(requireNamespace(...))` gives "is not TRUE"
- **Fix:** Replace with `if(!requireNamespace(...)) stop("package X required")`
- [ ] Write failing test
- [ ] Fix code
- [ ] Verify

#### 4.14 #13 — Demos use old `constraint()` API
- **File:** `demo/backwards_compat.R`, `demo/constrained_optim.R`, etc.
- **Root cause:** Demos call `constraint(assets=...)` but export is `constraint_v2(type=...)`
- **Fix:** Update demos to use modern `portfolio.spec()` + `add.constraint()` API
- [ ] Audit all demos
- [ ] Fix each broken demo
- [ ] Verify

---

## 5. Feature Requests

### 5.1 #43 — Parallelization level control for rebalancing
- User wants to control where parallelism is applied
- Needs design discussion
- [ ] Design
- [ ] Implement
- [ ] Done

### 5.2 #45 — Return solver failure info from rebalancing
- Silently swallowed errors should be captured/returned
- [ ] Design
- [ ] Implement
- [ ] Done

---

## 6. Support Request Responses

Defer until bugs are resolved. May answer with helpful comments.

- **#29** — Fixed income portfolio guidance
- **#42** — Time-varying factor exposure question
- **#47** — Max ES Ratio + Sortino covariance matrix

---

## 7. Out of Scope (PerformanceAnalytics)

### 7.1 #15 — `Return.clean` boudt single column
- **Status:** Fixed in PerformanceAnalytics 2.0.9
- **Action:** Close with comment pointing to PA fix

### 7.2 #48 — `DownsideDeviation` / `SortinoRatio` time-varying MAR
- **Status:** `DownsideDeviation` fixed; `SortinoRatio` still broken in PA
- **Action:** Transfer issue to PerformanceAnalytics repo
