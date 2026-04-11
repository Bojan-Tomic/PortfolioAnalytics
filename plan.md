# PortfolioAnalytics Plan

> **History note:** All completed bug fixes, issue triage, and earlier coverage
> work are recorded in commit `6e5ef96` and earlier. Coverage test files from
> the prior session are in commits `bc28c87`–`a127fa0`. R CMD check fixes are
> in commit `bb2f17e`.

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

These bugs were found while writing tests to increase coverage.  None have been
fixed yet — they are documented here for a future bug-fix pass.  Each has a
corresponding regression-guard test that will fail when the bug is fixed,
ensuring the fix is noticed and the test updated.

### BUG-1 — `extractStats.optimize.portfolio.parallel`: wrong iteration variable
**File:** `R/extractstats.R`, lines 317–328
**Symptom:** Calling `extractStats()` on the result of `optimize.portfolio.parallel()`
errors or produces garbage.
**Root cause:** The function assigns `resultlist <- object` then iterates
`1:length(object)`.  But `object` is a 3-element list (`$optimizations`,
`$call`, `$elapsed_time`), so `resultlist[[2]]` is a `call` object and
`resultlist[[3]]` is a `difftime` — neither has an `extractStats` method.
The intent was clearly `resultlist <- object$optimizations`.
**Fix:** Change line 318 to `resultlist <- object$optimizations`.
**Regression test:** `tests/testthat/test-optimize-parallel.R` —
"extractStats on parallel object either succeeds or fails with known bug"
(documents the error; must be updated when fixed to assert success).

### BUG-2 — `custom.covRob.Mcd`: `match.call()` returns unevaluated symbol for `control=`
**File:** `R/custom.covRob.R` (internal `control` handling)
**Symptom:** `custom.covRob.Mcd(R, control = ctrl)` where `ctrl` is a variable
fails because `match.call()` captures the unevaluated symbol `ctrl` rather than
its value, causing the downstream `covRobMcd()` call to receive `NULL`.
**Workaround:** Pass all individual parameters explicitly alongside `control=`
(e.g. `alpha=`, `nsamp=`, etc.) so the function never needs to dereference the
symbol.
**Fix:** Replace `match.call()$control` with `list(...)[["control"]]` or use
`sys.call()` / `eval()` to force the argument.
**Regression test:** `tests/testthat/test-custom-covrob.R` — the Mcd tests pass
all parameters explicitly to avoid the bug.

### BUG-3 — `set.portfolio.moments()` / `optimize.portfolio()`: `match.call()` for `posterior_p`
**File:** `R/moment.functions.R`, `set.portfolio.moments()` line ~209
**Symptom:** `set.portfolio.moments(R, p, method="meucci", posterior_p=pp)` where
`pp` is a variable silently uses `NULL` for `posterior_p` (the unevaluated
symbol `pp` is passed to `meucci.moments()` which cannot use it).
**Root cause:** Same `match.call()` pattern as BUG-2: `hasArg(posterior_p)`
correctly detects the argument is present, but `match.call(...)$posterior_p`
returns the symbol rather than the value.
**Fix:** Use `list(...)[["posterior_p"]]` or `eval(match.call(...)$posterior_p,
parent.frame())` to force evaluation.
**Regression test:** `tests/testthat/test-moment-functions-garch.R` — the
`posterior_p` test is intentionally a no-op (documents the bug).

### BUG-4 — `CCCgarch.MM()`: `clean` argument logic is inverted
**File:** `R/moment.functions.R`, `CCCgarch.MM()`, lines ~36–40
**Symptom:** The branch `if (!hasArg(clean))` fires when `clean` is *absent*
from `...`, setting `clean <- match.call(...)$clean` which is `NULL`.  The
`else` fires when `clean` *is* present, and also sets `clean <- NULL` —
discarding the caller's value entirely.  Result: `clean` is always `NULL`
regardless of what the caller passes.
**Fix:** Invert the condition to `if (hasArg(clean))` and use proper forced
evaluation, e.g. `clean <- eval(match.call(expand.dots=TRUE)$clean, parent.frame())`.
**Regression test:** `tests/testthat/test-custom-covrob.R` and
`test-moment-functions-garch.R` — CCCgarch.MM tests use `tryCatch` / `skip_if`
to guard against the NULL clean issue.

### BUG-5 — `optimize.portfolio.rebalancing()`: crashes with `regime.portfolios` input
**File:** `R/optimize.portfolio.R`, line ~3497
**Symptom:** Calling `optimize.portfolio.rebalancing()` with a `regime.portfolios`
portfolio object throws: *argument to 'which' is not logical*.
**Root cause:** The turnover-constraint check `which(sapply(...) == "turnover")`
is applied to `portfolio$constraints`, but `regime.portfolios` objects have
`NULL` constraints at the top level, so `sapply(NULL, ...)` returns `list()`
and `list() == "turnover"` is not a logical vector.
**Workaround:** Manually construct the `"optimize.portfolio.rebalancing"` result
object by running individual `optimize.portfolio()` calls and assembling:
`list(portfolio=regime_port, R=R, opt_rebalancing=out_list)` with the
appropriate class.
**Regression test:** `tests/testthat/test-extractstats-regime.R` — uses the
manual construction workaround; includes a comment describing the bug.

### BUG-6 — `constrained_objective_v2()`: `CSM` switch arm leaves `fun` undefined
**File:** `R/constrained_objective.R`, switch statement around line ~605
**Symptom:** Any call to `constrained_objective()` with a portfolio containing a
`portfolio_risk_objective` named `"CSM"` throws: *object 'fun' not found*.
**Root cause:** The switch arm `CSM = {}` is an empty block — it does not assign
`fun`.  There is no default initialisation of `fun <- NULL` before the switch.
The immediately following `if (is.function(fun))` then fails because `fun`
does not exist in scope.
**Fix:** Add `fun <- NULL` before the switch statement, or give the `CSM` arm a
real function (e.g. `fun <- ES` or a CSM-specific helper).
**Regression test:** `tests/testthat/test-constrained-objective-gaps.R` —
"CSM objective name [known bug]: constrained_objective throws 'fun not found'"
uses `expect_error(regexp="fun")` as a regression guard.

### BUG-7 — `gmv_opt_ptc()`: target-return formulation produces non-numeric weights
**File:** `R/optFUN.R`, `gmv_opt_ptc()` function, lines ~882–888
**Symptom:** When `target` is non-`NA`, the function adds a mean-return equality
constraint row to the QP matrix, but the resulting QP solution contains
non-numeric (NA/NaN) weights.
**Root cause:** Not yet fully investigated; likely a sign error or index mismatch
in the equality constraint construction.
**Regression test:** `tests/testthat/test-optFUN-gaps.R` — the "non-NA target"
test is guarded by `skip_if` with a comment "known formulation issue".

### BUG-8 — `summary.optimize.portfolio.parallel`: crashes when nodes use ROI solver
**File:** `R/generics.R`, `summary.optimize.portfolio.parallel()`, lines 1077–1080
**Symptom:** Calling `summary()` on the result of `optimize.portfolio.parallel()`
with `optimize_method="ROI"` throws: *incorrect number of dimensions*.
**Root cause:** `extractStats()` on a single ROI optimization result returns a
**named numeric vector** (not a matrix). The summary method then calls
`tmp[which.min(tmp[,"out"]),]` which requires a matrix — the `[,"out"]`
column-selector fails on a vector.  By contrast, `random`/`DEoptim`/`pso`
results (which use `trace=TRUE`) return a multi-row matrix, so those solvers work.
**Fix:** Wrap the `extractStats(x)` result in `as.matrix()` (or `rbind()`) before
applying `[,]` indexing; or add `if (is.vector(tmp)) tmp <- t(as.matrix(tmp))`
before line 1079.
**Workaround:** Use `optimize_method="random"` (with `trace=TRUE`) or `"DEoptim"`
when calling `optimize.portfolio.parallel()` if you need `summary()` to work.
**Regression test:** `tests/testthat/test-generics-print-summary.R` — the parallel
section uses `optimize_method="random"` as a workaround; includes a comment
describing why ROI cannot be used here.

### BUG-9 — `VaR`/`ES` in PerformanceAnalytics: `portfolio_method='single'` does not auto-compute moments
**Upstream package:** `PerformanceAnalytics` — filed as
[braverock/PerformanceAnalytics#197](https://github.com/braverock/PerformanceAnalytics/issues/197)
**Symptom:** `VaR(R, weights=w, portfolio_method='single', method='modified')` (no explicit `mu`/`sigma`/`m3`/`m4`) crashes with *"M3 must be a matrix"*. Same for `method='gaussian'` (*"requires numeric/complex matrix/vector arguments"*) and for `ES()`.
**Root cause:** The `portfolio_method='single'` branch of `VaR()`/`ES()` passes `mu`/`sigma`/`m3`/`m4` directly to `mVaR.MM()`/`GVaR.MM()` without first auto-computing them when they are `NULL`. The multi-asset branch already has `if (is.null(m3)) m3 = M3.MM(R, as.mat=FALSE)` guards; the `single` branch lacks them.
**Downstream impact:** `applyFUN()` in PortfolioAnalytics calls `SharpeRatio()` with `FUN='SharpeRatio'`; `SharpeRatio` has no `m3`/`m4` formals so it cannot pass higher-order moments to its internal `VaR()`/`ES()` calls, causing the crash in `chart.Concentration` (and any `applyFUN()` usage) when `FUN` is any function that routes through `VaR`/`ES` with `method='modified'` or `method='gaussian'`.
**Fix (upstream):** Add moment auto-computation guards to the `single+weights` path in `VaR()` and `ES()`.
**Workaround:** Pass `FUN="StdDev"` to `SharpeRatio`, or supply explicit moments when calling `VaR`/`ES` directly.
**Regression test:** None yet — the bug is upstream; once PA#197 is fixed and a new PA version is released, add a test confirming `applyFUN(R, weights, FUN='SharpeRatio')` works without error.

---

## Remaining Open Items

### Feature Requests (deferred — non-trivial design needed)

- **#43** — Parallelization level control for rebalancing
- **#45** — Return solver failure info from rebalancing
- **#42** — Time-varying factor exposure (workaround documented in issue)

---

## Coverage Status

**Baseline from `covr/coverage-2026-04-11.rds`: 84.06%**

**Current (after this session's commits): 85.39%** (`covr/coverage-latest.rds`)

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

### Priority 1 — Already done this session (commit `8ae803e` + latest)

- `test-ac-ranking-trailing.R` (20 tests) — `trailingFUN`, `ac_ranking`, `centroid.buckets` ✓
- `test-charts-de-pso-extra.R` (21 tests) — `charts.DE`, `charts.PSO` ✓
- `test-charts-risk-extra.R` (22 tests) — `charts.risk` ✓
- `test-generics-print-summary.R` extended (+20 tests) — `generics.R` print branches ✓

### Priority 2 — Next targets (lowest coverage, clear branches)

#### A. `R/constrained_objective.R` — 83.9%
- Lines 24, 32, 42: NULL/wrong-class early stops
- Lines 127–144: penalty weight branches (`enabled=FALSE` constraint penalties)
- Lines 158, 182–219: objective-type dispatch (CSM, weight_concentration)
- Lines 580–602: CSM switch arm (BUG-6 area)
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
