# PortfolioAnalytics Plan

> **History note:** All completed bug fixes, issue triage, and earlier coverage
> work are recorded in commit `6e5ef96` and earlier. Coverage test files from
> the prior session are in commits `bc28c87`–`a127fa0`. R CMD check fixes are
> in commit `bb2f17e`. Bug fixes (BUG-1 through BUG-8) are in commit `c1daf38`.
> Session 2 coverage work (charts, stat.factor.model, fn_map, EF, constrained_objective,
> optFUN, moment.functions, utils) is in commits `b9db2a7`–`5d02fae`.
> Sessions 3–6 coverage and bug-fix work is in commits `6b9100f`–`e237b23`.

---

## Conventions and Rules

- Be concise and efficient when communicating. Think all you like, but don't think out loud. Silence is golden. Communicate only when you need input from the user or complete a task
- You are authorized to use parallel subagents to accomplish a task
- All test files live in `tests/testthat/`, named `test-{topic}.R`
- Use only modern testthat 3.x syntax — no `context()`, no `expect_that()`
- Every `test_that()` block must have `skip_on_cran()` inside the block (NOT at file level — file-level `skip_on_cran()` outside `test_that()` skips everything when run under covr)
- Do NOT check exact numerical values for stochastic optimizers
- Tests run against the **installed** package — run `R CMD INSTALL . --no-test-load` after any source change
- Run tests with `NOT_CRAN=true Rscript -e "testthat::test_file(...)"` — never `test_dir()` (crashes with parallel runner)
- Run coverage in only one agent at a time to prevent parallel runner from corrupting covr tracing; do not run `tests/run-all.R` in parallel subagents
- `DESCRIPTION` has `Config/testthat/parallel: false` (changed from `true` to prevent covr hangs)
- Unexported package functions must be accessed in tests via `PortfolioAnalytics:::`
- Do not end bullets with a period
- Never run `git push` — only `git add` and `git commit`

---

## Bugs Discovered During Coverage Work

### BUG-1 — `extractStats.optimize.portfolio.parallel`: wrong iteration variable
**File:** `R/extractstats.R`, line 318
**Status: FIXED** — Changed `resultlist <- object` to `resultlist <- object$optimizations`
**Regression test:** `tests/testthat/test-optimize-parallel.R`

### BUG-2 — `custom.covRob.Mcd`: `match.call()` captures symbol for `control=`
**File:** `R/custom.covRob.R`, line 90
**Status: FIXED** — Changed `match.call(expand.dots=TRUE)$control` to `list(...)[["control"]]`
**Regression test:** `tests/testthat/test-custom-covrob.R`

### BUG-3 — `set.portfolio.moments()`: `match.call()` for `posterior_p`
**File:** `R/moment.functions.R`, line 209
**Status: FIXED** — Changed `match.call(expand.dots=TRUE)$posterior_p` to `list(...)[["posterior_p"]]`
**Regression test:** `tests/testthat/test-moment-functions-garch.R`

### BUG-4 — `CCCgarch.MM()`: `clean` argument logic is inverted
**File:** `R/moment.functions.R`, lines 45–47
**Status: FIXED** — Inverted condition (`!hasArg` → `hasArg`) and replaced `match.call()$clean` with `list(...)[["clean"]]`
**Test:** `tests/testthat/test-moment-functions-garch.R`

### BUG-5 — `optimize.portfolio.rebalancing()`: crashes with `regime.portfolios` input
**File:** `R/optimize.portfolio.R`, line 3497
**Status: FIXED** — Added NULL guard: `turnover_idx <- if (!is.null(portfolio$constraints)) which(...) else integer(0)`
**Regression test:** `tests/testthat/test-extractstats-regime.R`

### BUG-6 — `constrained_objective_v2()`: `CSM` switch arm leaves `fun` undefined
**File:** `R/constrained_objective.R`, lines 572 and 631–634
**Status: FIXED** — Added `fun <- NULL` before the switch; wrapped `do.call` with `if(is.function(fun)) { ... } else { next }`
**Regression test:** `tests/testthat/test-constrained-objective-gaps.R`

### BUG-7 — `gmv_opt_ptc()`: target-return formulation produces non-numeric weights
**File:** `R/optFUN.R`, line 895
**Status: FIXED** — Changed `rhs <- 1 + target` to `rhs <- target`
**Regression test:** `tests/testthat/test-optFUN-gaps.R`

### BUG-8 — `summary.optimize.portfolio.parallel`: crashes when nodes use ROI solver
**File:** `R/generics.R`, lines 1078–1079
**Status: FIXED** — Added `if (is.vector(tmp)) tmp <- t(as.matrix(tmp))` before the `[,"out"]` index
**Regression test:** `tests/testthat/test-generics-print-summary.R`

### BUG-9 — `VaR`/`ES` in PerformanceAnalytics: `portfolio_method='single'` does not auto-compute moments
**Upstream package:** `PerformanceAnalytics` — filed as
[braverock/PerformanceAnalytics#197](https://github.com/braverock/PerformanceAnalytics/issues/197)
**Status: UPSTREAM — not fixed here**
**Workaround:** Pass `FUN="StdDev"` to `SharpeRatio`, or supply explicit moments when calling `VaR`/`ES` directly

### BUG-10 — `etl_opt` group constraint dimension mismatch
**File:** `R/optFUN.R`, line 463
**Status: FIXED** (`cdc1cc2`) — `Amat.group` had only `N` columns but needed `N + T+1`; fix: `cbind(Amat.group, zeros)` where `zeros` has `T+1` cols

### BUG-11 — `gmv_opt_toc`/`gmv_opt_ptc`/`gmv_opt_leverage`: NULL cLO/cUP and dir-filter bug
**File:** `R/optFUN.R`, ~lines 775, 895, 1022
**Status: FIXED** (`e237b23`) — local `cLO`/`cUP` variables were set conditionally but then the original `constraints$cLO`/`constraints$cUP` were used in `rhs`; dir-filter used already-modified `rhs`

### BUG-12 — `gmv_opt_ptc` accidentally deleted from source
**File:** `R/optFUN.R`
**Status: FIXED** (`e237b23`) — Function was restored with all cLO/cUP and dir-filter bugs already corrected

---

## Feature Requests (deferred — non-trivial design needed)

- **#43** — Parallelization level control for rebalancing
- **#45** — Return solver failure info from rebalancing
- **#42** — Time-varying factor exposure (workaround documented in issue)

---

## Coverage Status

| Checkpoint | R-only coverage |
|---|---|
| Baseline (`covr/coverage-2026-04-11.rds`) | 85.24% |
| After session 2 (`covr/coverage-2026-04-11-session2.rds`) | 86.42% |
| After sessions 3–4 | 87.01% |
| After sessions 5–6 (`covr/coverage-2026-04-11.rds` — this is the most recent measurement) | **92.82%** |

Both `R CMD check --as-cran` and `NOT_CRAN=true R CMD check` pass — tests: OK.
Pre-existing warnings (`.gcda` files, missing `inst/doc`, version, compile flags) are unrelated to our work.

### Per-file coverage (from `covr/coverage-2026-04-11.rds`)

| File | % | Notes |
|------|--:|-------|
| `R/charts.risk.R` | 86.36 | charting, hard to test |
| `R/charts.multiple.R` | 87.65 | charting |
| `R/plotFrontiers.R` | 89.09 | charting |
| `R/charts.efficient.frontier.R` | 90.49 | charting |
| `R/chart.concentration.R` | 90.63 | charting |
| `R/trailingFUN.R` | 90.63 | |
| `R/constraint_fn_map.R` | 90.75 | |
| `R/EntropyProg.R` | 90.83 | |
| `R/mult.layer.portfolio.R` | 90.91 | |
| `R/optFUN.R` | 91.16 | remaining: try-error stops, milp paths, factor-exposure blocks |
| `R/extract.efficient.frontier.R` | 91.58 | |
| `R/optimize.portfolio.R` | 92.06 | |
| `R/charts.RP.R` | 92.16 | charting |
| `R/custom.covRob.R` | 92.19 | |
| `R/charts.groups.R` | 92.86 | charting |
| `R/random_portfolios.R` | 92.95 | |
| `R/generics.R` | 93.41 | |
| `R/constraints.R` | 93.45 | |
| `R/constrained_objective.R` | 94.10 | remaining: penalty enabled=FALSE, some CSM/weight_conc paths |
| `R/extractstats.R` | 94.22 | |
| `R/moment.functions.R` | 94.29 | |
| `R/utils.R` | 94.44 | |
| `R/objective.R` | 94.67 | |
| *(≥95% files not shown)* | | |

### Test files added (all committed)

| File | Session | Tests |
|------|---------|------:|
| `test-charts-groups-extra.R` | 2 | 23 |
| `test-chart-weights-extra.R` | 2 | 17 |
| `test-stat-factor-model-extra.R` | 2 | 31 |
| `test-fn-map.R` | 2 | extended +233 lines |
| `test-efficient-frontier.R` | 2 | extended +262 lines |
| `test-constrained-objective-branches.R` | 2 | 49 total |
| `test-optFUN-gaps.R` | 2 | 31 total |
| `test-moment-utils-gaps.R` | 2 | 43 |
| `test-optimize-portfolio-gaps.R` | 3 | 49 |
| `test-optimize-portfolio-v1-gaps.R` | 4 | 16 |
| `test-optimize-portfolio-rglpk-max-pos.R` | 4 | 8 |
| `test-optimize-portfolio-osqp-gaps.R` | 4 | 12 |
| `test-optimize-portfolio-mco-gaps.R` | 4 | 14 |
| `test-optFUN-gaps2.R` | 4 | 18 |
| `test-constrained-objective-gaps.R` | 4 | 22 |
| `test-constrained-objective-gaps2.R` | 4 | 19 |
| `test-optimize-roi-extended.R` | 4 | 89 |
| `helper-portfolioanalytics.R` | 4 | shared fixtures |
| `test-optFUN-gaps3.R` | 5/6 | 27 |
| `test-constrained-objective-gaps3.R` | 5/6 | 16 |

---

## Next Steps

The remaining uncovered lines fall into these categories (prioritized):

1. **`R/optFUN.R`** (91.16%) — ~62 uncovered lines
   - try-error `stop()` branches (lines 141, 253, 389, 482, 637) — triggered only when solver fails; hard to force
   - `maxret_milp_opt` and `etl_milp_opt` group/factor-exposure paths (lines 606–611, 692–693)
   - `gmv_opt_toc`/`ptc`/`leverage` with non-zero `moments$mean` target path (lines 717–720, 856–857, etc.)
   - `max_sr_opt` factor-exposure block (lines 1103–1118)

2. **`R/constrained_objective.R`** (94.10%) — ~19 uncovered lines
   - `penalty_objective` with `enabled=FALSE` (lines 100, 128–129, 137, 158, 196, 214–219, 242)
   - Line 371 (`stop("portfolio object is not of class portfolio")`) — effectively dead code
   - Lines 581–582, 592, 603 — v2 objective penalty edge cases
   - Line 649 — v2 message branch (known-but-failing function)

3. **`R/trailingFUN.R`** (90.63%), **`R/constraint_fn_map.R`** (90.75%), **`R/EntropyProg.R`** (90.83%) — moderate effort
