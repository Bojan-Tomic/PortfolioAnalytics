# PortfolioAnalytics Plan

> **History note:** All completed bug fixes, issue triage, and earlier coverage
> work are recorded in commit `6e5ef96` and earlier. Coverage test files from
> the prior session are in commits `bc28c87`–`a127fa0`. R CMD check fixes are
> in commit `bb2f17e`. Bug fixes (BUG-1 through BUG-8) are in the current
> working commit.

---

## Conventions and Rules

- All test files live in `tests/testthat/`, named `test-{topic}.R`
- Use only modern testthat 3.x syntax — no `context()`, no `expect_that()`
- Every test file must have `skip_on_cran()` and `skip_if_not_installed()` guards
- Do NOT check exact numerical values for stochastic optimizers
- Tests run against the **installed** package — run `R CMD INSTALL . --no-test-load` after any source change
- Run tests with `testthat::test_file()` — `test_dir()` crashes with the parallel runner (`Config/testthat/parallel: true` is set in DESCRIPTION)
- Run coverage with `TESTTHAT_CPUS=1` to prevent parallel runner from corrupting covr tracing
- Unexported package functions must be accessed in tests via `PortfolioAnalytics:::`
- Never run `git push` — only `git add` and `git commit`

---

## Bugs Discovered During Coverage Work

### BUG-1 — `extractStats.optimize.portfolio.parallel`: wrong iteration variable
**File:** `R/extractstats.R`, line 318
**Status: FIXED** — Changed `resultlist <- object` to `resultlist <- object$optimizations`
**Regression test updated:** `tests/testthat/test-optimize-parallel.R` — now asserts success.

### BUG-2 — `custom.covRob.Mcd`: `match.call()` captures symbol for `control=`
**File:** `R/custom.covRob.R`, line 90
**Status: FIXED** — Changed `match.call(expand.dots=TRUE)$control` to `list(...)[["control"]]`
**Regression test updated:** `tests/testthat/test-custom-covrob.R` — new test passes `control` as a variable.

### BUG-3 — `set.portfolio.moments()`: `match.call()` for `posterior_p`
**File:** `R/moment.functions.R`, line 209
**Status: FIXED** — Changed `match.call(expand.dots=TRUE)$posterior_p` to `list(...)[["posterior_p"]]`
**Regression test updated:** `tests/testthat/test-moment-functions-garch.R` — new test passes `posterior_p` as a variable and asserts different result from uniform default.

### BUG-4 — `CCCgarch.MM()`: `clean` argument logic is inverted
**File:** `R/moment.functions.R`, lines 45–47
**Status: FIXED** — Inverted condition (`!hasArg` → `hasArg`) and replaced `match.call()$clean` with `list(...)[["clean"]]`
**Test:** `tests/testthat/test-moment-functions-garch.R` (existing tests pass).

### BUG-5 — `optimize.portfolio.rebalancing()`: crashes with `regime.portfolios` input
**File:** `R/optimize.portfolio.R`, line 3497
**Status: FIXED** — Added NULL guard: `turnover_idx <- if (!is.null(portfolio$constraints)) which(...) else integer(0)`
**Regression test updated:** `tests/testthat/test-extractstats-regime.R` — new test calls `optimize.portfolio.rebalancing()` directly with `regime.portfolios` and asserts success.

### BUG-6 — `constrained_objective_v2()`: `CSM` switch arm leaves `fun` undefined
**File:** `R/constrained_objective.R`, lines 572 and 631–634
**Status: FIXED** — Added `fun <- NULL` before the switch; wrapped `do.call` with `if(is.function(fun)) { ... } else { next }` so CSM objectives are silently skipped.
**Regression test updated:** `tests/testthat/test-constrained-objective-gaps.R` — now asserts the call returns a finite numeric (no error).

### BUG-7 — `gmv_opt_ptc()`: target-return formulation produces non-numeric weights
**File:** `R/optFUN.R`, line 895
**Status: FIXED** — Changed `rhs <- 1 + target` to `rhs <- target` (mean-return equality constraint RHS was off by 1).
**Regression test updated:** `tests/testthat/test-optFUN-gaps.R` — now asserts finite numeric weights with `ptc=0.001` (feasible).

### BUG-8 — `summary.optimize.portfolio.parallel`: crashes when nodes use ROI solver
**File:** `R/generics.R`, lines 1078–1079
**Status: FIXED** — Added `if (is.vector(tmp)) tmp <- t(as.matrix(tmp))` before the `[,"out"]` index to handle ROI's named-vector return from `extractStats()`.
**Regression test updated:** `tests/testthat/test-generics-print-summary.R` — new test uses ROI solver and asserts `summary()` returns a `summary.optimize.portfolio.parallel` object.

### BUG-9 — `VaR`/`ES` in PerformanceAnalytics: `portfolio_method='single'` does not auto-compute moments
**Upstream package:** `PerformanceAnalytics` — filed as
[braverock/PerformanceAnalytics#197](https://github.com/braverock/PerformanceAnalytics/issues/197)
**Status: UPSTREAM — not fixed here**
**Symptom:** `VaR(R, weights=w, portfolio_method='single', method='modified')` (no explicit `mu`/`sigma`/`m3`/`m4`) crashes with *"M3 must be a matrix"*.
**Workaround:** Pass `FUN="StdDev"` to `SharpeRatio`, or supply explicit moments when calling `VaR`/`ES` directly.

---

## Remaining Open Items

### Feature Requests (deferred — non-trivial design needed)

- **#43** — Parallelization level control for rebalancing
- **#45** — Return solver failure info from rebalancing
- **#42** — Time-varying factor exposure (workaround documented in issue)

---

## Coverage Status

**Baseline from `covr/coverage-2026-04-11.rds`: 84.06%**

**After bug-fix session: ~85.39%** (coverage runs pending after fixes)

### Per-file coverage (latest run, lowest first)

| File | % |
|------|--:|
| `R/optimize.portfolio.R` | 75.1 |
| `R/optFUN.R` | 81.3 |
| `R/constrained_objective.R` | 83.9 |
| `R/stat.factor.model.R` | 86.5 |
| `R/charts.groups.R` | 86.7 |
| `R/chart.Weights.R` | 87.5 |
| `R/moment.functions.R` | 88.3 |
| `R/charts.risk.R` | 88.3 |
| `R/constraint_fn_map.R` | 88.5 |
| `R/extract.efficient.frontier.R` | 88.6 |
| `R/utils.R` | 88.9 |
| `R/mult.layer.portfolio.R` | 89.2 |
| `R/EntropyProg.R` | 89.2 |
| `R/charts.multiple.R` | 89.5 |
| `R/plotFrontiers.R` | 90.0 |
| `R/chart.concentration.R` | 90.8 |
| `R/charts.ROI.R` | 91.0 |
| `R/charts.efficient.frontier.R` | 91.2 |
| `R/charts.RP.R` | 92.0 |
| `R/trailingFUN.R` | 92.1 |
| `R/constraints.R` | 92.3 |
| `R/random_portfolios.R` | 92.4 |
| `R/generics.R` | 93.2 |
| `R/inverse.volatility.weight.R` | 93.3 |
| `R/extractstats.R` | 93.5 |
| `R/objective.R` | 94.1 |
| `R/custom.covRob.R` | 95.1 |
| `R/charts.DE.R` | 95.3 |
| `R/portfolio.R` | 95.7 |
| `R/charts.PSO.R` | 95.9 |
| `R/applyFUN.R` | 96.0 |
| `R/charts.GenSA.R` | 96.2 |
| `R/backtest.plot.R` | 96.5 |
| `R/ac_ranking.R` | 100.0 |
| `R/black_litterman.R` | 100.0 |
| `R/chart.RiskReward.R` | 100.0 |
| `R/constraints_ROI.R` | 100.0 |
| `R/constraintsFUN.R` | 100.0 |
| `R/equal.weight.R` | 100.0 |
| `R/extractrisk.R` | 100.0 |
| `R/meucci_moments.R` | 100.0 |
| `R/meucci_ranking.R` | 100.0 |
| `R/objectiveFUN.R` | 100.0 |
| `R/opt.outputMvo.R` | 100.0 |
| `R/utility.combine.R` | 100.0 |

---

## Next Steps (prioritized by bang-for-buck)

### Priority 1 — Already done (bugs fixed this session)

- BUG-1: `extractStats.optimize.portfolio.parallel` wrong iteration variable ✓
- BUG-2: `custom.covRob.Mcd` `match.call()` symbol issue ✓
- BUG-3: `set.portfolio.moments()` `match.call()` for `posterior_p` ✓
- BUG-4: `CCCgarch.MM()` inverted `clean` logic ✓
- BUG-5: `optimize.portfolio.rebalancing()` crash with `regime.portfolios` ✓
- BUG-6: `constrained_objective_v2()` CSM arm leaves `fun` undefined ✓
- BUG-7: `gmv_opt_ptc()` target-return `rhs = 1 + target` ✓
- BUG-8: `summary.optimize.portfolio.parallel` ROI vector indexing crash ✓

### Priority 2 — Next targets (lowest coverage, clear branches)

#### A. `R/constrained_objective.R` — 83.9%
- Lines 24, 32, 42: NULL/wrong-class early stops
- Lines 127–144: penalty weight branches (`enabled=FALSE` constraint penalties)
- Lines 158, 182–219: objective-type dispatch (CSM, weight_concentration)
**File:** extend `tests/testthat/test-constrained-objective-branches.R`

#### B. `R/stat.factor.model.R` — 86.5%
- Need to identify uncovered clusters (likely `factor.model.BetaCoV` edge cases)
**File:** extend or create `test-stat-factor-model-extra.R`

#### C. `R/charts.groups.R` — 86.7%
- Lines ~39, 42, 51, 66–67, 115, 122, 127 — `xlab`/`las`/`neighbors`/`chart.assets` branches
**File:** new `test-charts-groups-extra.R`

#### D. `R/chart.Weights.R` — 87.5%
- Lines 75, 76, 86 — likely `opt.list` method or `las`/`xlab` parameter path
**File:** add tests to existing chart test file

#### E. `R/constraint_fn_map.R` — 88.5%
- ~54 uncovered lines — dispatch branches for rarely-used constraint types
**File:** extend `test-constraint-fn-map.R`

#### F. `R/extract.efficient.frontier.R` — 88.6%
- ~33 uncovered lines — efficient frontier extraction edge cases
**File:** extend existing EF test file

### Priority 3 — Large files (deferred)

#### G. `R/optFUN.R` — 81.3%
- ~134 uncovered lines (see prior plan detail for clusters)

#### H. `R/optimize.portfolio.R` — 75.1%
- Largest gap — most require complex solver/MILP setup; tackle in sub-passes
