# PortfolioAnalytics Plan

> **History note:** All completed bug fixes, issue triage, and earlier coverage
> work are recorded in commit `6e5ef96` and earlier. Coverage test files from
> the prior session are in commits `bc28c87`–`a127fa0`. R CMD check fixes are
> in commit `bb2f17e`. Bug fixes (BUG-1 through BUG-8) are in commit `c1daf38`.
> Session 2 coverage work (charts, stat.factor.model, fn_map, EF, constrained_objective,
> optFUN, moment.functions, utils) is in commits `b9db2a7`–`5d02fae`.

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

**Baseline from `covr/coverage-2026-04-11.rds`: 85.24% R-only**

**After session 2 (`covr/coverage-2026-04-11-session2.rds`): 86.42% R-only (+1.18 pp)**

Note: overall `percent_coverage()` includes C files; the gcov stamp-mismatch
causes C coverage to read 0% in the session-2 run even though the C code is
unchanged.  R-only comparison is the accurate measure of progress.

### Session 2 test files added / extended

| File | Status | Tests |
|------|--------|------:|
| `test-charts-groups-extra.R` | NEW | 23 |
| `test-chart-weights-extra.R` | NEW | 17 |
| `test-stat-factor-model-extra.R` | NEW | 31 |
| `test-fn-map.R` | EXTENDED | +233 lines |
| `test-efficient-frontier.R` | EXTENDED | +262 lines |
| `test-constrained-objective-branches.R` | EXTENDED | 49 total |
| `test-optFUN-gaps.R` | EXTENDED | 31 total |
| `test-moment-utils-gaps.R` | NEW | 43 |

### Per-file coverage (session 2 run, lowest first)

| File | % |
|------|--:|
| `R/optimize.portfolio.R` | 71.0 |
| `R/optFUN.R` | 83.6 |
| `R/constrained_objective.R` | 85.7 |
| `R/charts.risk.R` | 86.4 |
| `R/charts.multiple.R` | 87.7 |
| `R/plotFrontiers.R` | 89.1 |
| `R/charts.efficient.frontier.R` | 90.5 |
| `R/chart.concentration.R` | 90.6 |
| `R/trailingFUN.R` | 90.6 |
| `R/constraint_fn_map.R` | 90.7 |
| `R/EntropyProg.R` | 90.8 |
| `R/mult.layer.portfolio.R` | 90.9 |
| `R/charts.ROI.R` | 91.5 |
| `R/extract.efficient.frontier.R` | 91.6 |
| `R/charts.RP.R` | 92.2 |
| `R/custom.covRob.R` | 92.2 |
| `R/charts.groups.R` | 92.9 |
| `R/random_portfolios.R` | 92.9 |
| `R/constraints.R` | 93.4 |
| `R/generics.R` | 93.4 |
| `R/extractstats.R` | 94.2 |
| `R/moment.functions.R` | 94.3 |
| `R/utils.R` | 94.4 |
| `R/objective.R` | 94.7 |
| `R/charts.DE.R` | 95.0 |
| `R/inverse.volatility.weight.R` | 95.5 |
| `R/applyFUN.R` | 95.6 |
| `R/charts.PSO.R` | 95.7 |
| `R/charts.GenSA.R` | 95.8 |
| `R/portfolio.R` | 96.3 |
| `R/backtest.plot.R` | 97.6 |
| `R/ac_ranking.R` | 100.0 |
| `R/black_litterman.R` | 100.0 |
| `R/chart.RiskReward.R` | 100.0 |
| `R/chart.Weights.R` | 100.0 |
| `R/constraints_ROI.R` | 100.0 |
| `R/constraintsFUN.R` | 100.0 |
| `R/equal.weight.R` | 100.0 |
| `R/extractrisk.R` | 100.0 |
| `R/meucci_moments.R` | 100.0 |
| `R/meucci_ranking.R` | 100.0 |
| `R/objectiveFUN.R` | 100.0 |
| `R/opt.outputMvo.R` | 100.0 |
| `R/stat.factor.model.R` | 100.0 |
| `R/utility.combine.R` | 100.0 |

---

## Next Steps (prioritized by bang-for-buck)

### Priority 1 — Completed (session 2)

- `chart.Weights.R`: 87.5% → 100.0% ✓
- `stat.factor.model.R`: 86.5% → 100.0% ✓
- `charts.groups.R`: 86.7% → 92.9% ✓
- `constrained_objective.R`: 83.9% → 85.7% ✓
- `optFUN.R`: 81.3% → 83.6% ✓
- `constraint_fn_map.R`: 88.5% → 90.7% ✓
- `extract.efficient.frontier.R`: 88.6% → 91.6% ✓
- `moment.functions.R`: 88.3% → 94.3% ✓
- `utils.R`: 88.9% → 94.4% ✓

### Priority 2 — Next targets (lowest R coverage)

#### A. `R/optimize.portfolio.R` — 71.0% (~135 uncovered lines)
- Largest single gap; most branches require complex solver/MILP setup
- Sub-targets: rebalancing edge cases, regime-switching paths, parallel paths

#### B. `R/optFUN.R` — 83.6% (~90 uncovered lines)
- Remaining: factor-exposure constraint blocks, try-error stop branches,
  `max_sr_opt` edge cases not yet hit

#### C. `R/constrained_objective.R` — 85.7% (~65 uncovered lines)
- Remaining: penalty-weight enabled=FALSE branches, some CSM/weight_conc paths

#### D. `R/charts.risk.R` — 86.4%
- Uncovered: some `chart.StackedBar` and `chart.GroupWeights` parameter paths

#### E. `R/charts.multiple.R` — 87.7%
- Uncovered: various optional argument branches in multi-chart functions
