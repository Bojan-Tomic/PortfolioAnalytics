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

**Baseline from `covr/coverage-2026-04-11.rds`: 84.06%** (overall; taken before
the `test-charts-gensa-extra.R` and `test-charts-rp-extra.R` commits, so actual
current coverage is slightly higher — ~84–85%).

### Per-file coverage (from RDS, lowest first)

| File | Lines | Covered | % | Uncovered lines |
|------|------:|--------:|--:|-----------------|
| `R/optimize.portfolio.R` | 2053 | 1458 | 71.0 | ~595 — see detail below |
| `R/optFUN.R` | 713 | 576 | 80.8 | ~137 — see detail below |
| `R/charts.risk.R` | 220 | 182 | 82.7 | 38 lines |
| `R/charts.DE.R` | 161 | 134 | 83.2 | 27 lines |
| `R/constrained_objective.R` | 319 | 267 | 83.7 | 52 lines |
| `R/ac_ranking.R` | 74 | 62 | 83.8 | 12 lines |
| `R/generics.R` | 591 | 496 | 83.9 | 95 lines |
| `R/moment.functions.R` | 210 | 177 | 84.3 | 33 lines |
| `R/trailingFUN.R` | 32 | 27 | 84.4 | 5 lines |
| `R/charts.PSO.R` | 116 | 98 | 84.5 | 18 lines |
| `R/stat.factor.model.R` | 168 | 143 | 85.1 | 25 lines |
| `R/chart.Weights.R` | 21 | 18 | 85.7 | 3 lines (75, 76, 86) |
| `R/charts.groups.R` | 56 | 48 | 85.7 | 8 lines |
| `R/extract.efficient.frontier.R` | 285 | 249 | 87.4 | 36 lines |
| `R/charts.multiple.R` | 81 | 71 | 87.7 | 10 lines |
| `R/utils.R` | 18 | 16 | 88.9 | 2 lines |
| `R/plotFrontiers.R` | 55 | 49 | 89.1 | 6 lines |
| `R/constraint_fn_map.R` | 454 | 406 | 89.4 | 48 lines |

---

## Next Steps (prioritized by bang-for-buck)

### Priority 1 — Quick wins (small files, isolated branches)

#### A. `R/trailingFUN.R` — 5 uncovered lines (84.4% → ~100%)
Uncovered: 40 (`nargs=NULL` when no `...`), 59 (vector R slicing), 73 (`FUNargs`
not-list warning), 76 (no `FUNargs` warning), 82 (try-error message branch).
- Line 40: call `trailingFUN` with `nargs=NULL` explicitly (no extra `...`)
- Line 59: pass a plain numeric vector for `R` instead of a matrix
- Line 73: pass `FUNargs` as a non-list (e.g. a character string)
- Line 76: pass `FUNargs=NULL` with a function that has formals
- Line 82: pass a `FUN` that throws an error (e.g. `function(...) stop("boom")`)
**File:** `tests/testthat/test-ac-ranking-trailing.R` (new)

#### B. `R/ac_ranking.R` — 12 uncovered lines (83.8% → ~100%)
- Line 35: `ac.ranking(R, order, max.value=0.5)` — `hasArg(max.value)` TRUE branch
- Lines 220–237: `centroid.buckets(list(c(1,2), c(3,4)))` — entire function untested
**File:** `tests/testthat/test-ac-ranking-trailing.R` (new, same file as above)

#### C. `R/chart.Weights.R` — 3 uncovered lines (85.7% → ~100%)
Lines 75, 76, 86 — need to identify what branch these are (likely the
`opt.list` method or a specific `las`/`xlab` parameter path).
**File:** add a few tests to `tests/testthat/test-charts-roi.R` or a new file.

### Priority 2 — Medium files with clear branch patterns

#### D. `R/charts.PSO.R` — 18 uncovered lines (84.5% → ~95%)
Same branch pattern as `charts.GenSA.R` (already covered in `test-charts-gensa-extra.R`):
- Lines 5, 84, 87: stop() branches (wrong class, NULL R)
- Lines 19, 30, 35: `xlab` non-NULL → minmargin=5; `las<=1` → bottommargin=minmargin; Inf constraints → ylim from weights
- Lines 100–101, 123–125, 130–136: non-matching `return.col`/`risk.col` → applyFUN fallback
- Lines 165, 178–179: `chart.assets=TRUE`; matrix neighbors in scatter
**File:** `tests/testthat/test-charts-de-pso-extra.R` (new)

#### E. `R/charts.DE.R` — 27 uncovered lines (83.2% → ~95%)
Same pattern as PSO:
- Lines 17, 102, 105: stop() branches
- Lines 31, 42, 47: `xlab`, `las<=1`, Inf constraints
- Lines 118–119, 141–154: applyFUN fallback branches in `chart.Scatter.DE`
- Lines 203–211: matrix neighbors scatter; `chart.assets=TRUE`
- Lines 250, 267, 273, 286–287: further scatter branches
**File:** `tests/testthat/test-charts-de-pso-extra.R` (new, same file as PSO)

#### F. `R/charts.risk.R` — 38 uncovered lines (82.7% → ~90%)
Key uncovered branches:
- Lines 62, 67, 72: `pct_contrib` risk type path in `chart.RiskBudget.optimize.portfolio`
- Lines 101–102: neighbors as vector (scalar) path
- Lines 127, 142–143: neighbors as matrix path; `pct_contrib` nbriskcol
- Lines 182, 186: `chart.RiskBudget.opt.list` — regime.portfolios path
- Lines 201–202, 230–231: `chart.RiskBudget.optimize.portfolio.rebalancing`
- Lines 236, 244, 257, 269, 283, 288–289, 294: barplot risk budget branches
- Lines 318, 333–334, 338–339: further rebalancing / absolute risk type branches
**File:** `tests/testthat/test-charts-risk-extra.R` (new)

#### G. `R/charts.groups.R` — 8 uncovered lines (85.7% → ~95%)
Lines 39, 42, 51, 66–67, 115, 122, 127 — likely `xlab`/`las` parameter branches
and a `neighbors` or `chart.assets` path.
**File:** add to existing chart tests or new `test-charts-groups-extra.R`

### Priority 3 — Larger files requiring more fixtures

#### H. `R/generics.R` — 95 uncovered lines (83.9%)
Key clusters:
- Lines 241–250: `print.portfolio` — `box (unconstrained)` and `box (with shorting)` branches (need portfolios with `-Inf`/`Inf` box or short-selling constraints)
- Lines 393–399, 435–441, 477–483, 519–525, 561–567, 603–609: `print.optimize.portfolio.*` — `objective_measures` second-element printing loop (need optimizations where `length(objective_measures[[i]]) > 1`)
- Lines 657–663: another print variant
- Lines 787–799: `summary` method branch with multiple objective measures
- Lines 889–912: `print.optimize.portfolio.rebalancing` — deeper branches
- Line 964: unknown branch
**File:** add targeted tests to `tests/testthat/test-generics-print-summary.R`

#### I. `R/moment.functions.R` — 33 uncovered lines (84.3%)
Key clusters:
- Lines 38, 49–50: `Return.clean()` try-error path in `set.portfolio.moments` (BUG-4 region)
- Lines 77–104: GARCH/clean moment paths — `garch=TRUE` objective branch and `arguments.clean` branch; these require a portfolio with a `garch=TRUE` objective or `arguments.clean` set
- Lines 198, 202–205, 209: `set.portfolio.moments` with `method="meucci"` — `hasArg` branches for `k`, `P`, `Mu`, `Sigma`, `Views`, `posterior_p`
- Lines 216–222: another GARCH branch (duplicate of 77–104 pattern)
- Lines 393, 477: isolated lines — need context
**File:** extend `tests/testthat/test-moment-functions-extended.R`

#### J. `R/constrained_objective.R` — 52 uncovered lines (83.7%)
- Lines 24, 32, 42, 82, 100, 106: early validation branches (NULL portfolio, wrong class, etc.)
- Lines 127–144: penalty weight branches
- Lines 158, 182, 188, 196, 206–219: objective-type dispatch branches
- Lines 229–242, 252–258, 268–277: further objective branches
- Lines 352, 358, 371, 376: constraint penalty branches
- Lines 580–602: CSM switch / BUG-6 area
- Line 644: isolated line
**File:** extend `tests/testthat/test-constrained-objective-branches.R`

#### K. `R/optFUN.R` — 137 uncovered lines (80.8%)
Very scattered across many functions. Key clusters:
- Lines 20–21, 28: early error branches in small helpers
- Lines 79–80, 141: `minvar_opt` / `etl_opt` edge cases
- Lines 186–187, 194, 202–205: `qp_opt` / `maxret_opt` branches
- Lines 229–230, 253, 263: further QP branches
- Lines 298–306, 351–370: `gmv_opt` / `gmv_opt_toc` branches
- Lines 389, 423–431, 444, 461–474, 482: ROI solver dispatch branches
- Lines 527–535, 601–620, 637: more ROI/quadprog branches
- Lines 692–720, 775–790, 805: `rp_opt` and random portfolio branches
- Lines 856–887: `gmv_opt_ptc` (BUG-7 area — target-return path)
- Lines 942–975, 1022–1047, 1103–1133, 1203–1204, 1410–1411: MILP and leverage branches
**File:** extend `tests/testthat/test-optFUN-extended.R` and `test-optFUN-gaps.R`

#### L. `R/optimize.portfolio.R` — 595 uncovered lines (71.0%)
The largest gap. Key clusters (deferred — most require complex solver setup):
- Lines 32–107: `portfolio_spec` validation / early error branches
- Lines 169–195: `constrained_objective` dispatch edge cases
- Lines 225–265: weight normalization / position limit branches
- Lines 285–356: penalty term branches
- Lines 1800–2332: RGLPK MILP with position limits (large block — needs `Rglpk` and complex constraint setup)
- Lines 2426–2600: ROI MILP branches
- Lines 2690–2977: rebalancing internals
- Lines 3049–3524: additional solver dispatch / error handling
**File:** extend existing solver-specific test files; create `test-optimize-portfolio-milp-branches.R` for MILP cluster

---

## Suggested Work Order

1. **A+B** (`trailingFUN` + `ac_ranking`) — single new file, ~15 tests, high yield
2. **D+E** (`charts.PSO` + `charts.DE`) — single new file, ~20 tests, mirrors already-written GenSA/RP pattern
3. **F** (`charts.risk`) — moderate complexity, ~15 tests
4. **C+G** (`chart.Weights` L75/76/86, `charts.groups`) — small additions to existing files
5. **H** (`generics.R`) — extend existing file, target the print-loop branches
6. **I** (`moment.functions`) — extend existing file, target meucci `hasArg` branches first (avoid BUG-3/4 regions)
7. **J** (`constrained_objective`) — extend existing file
8. **K** (`optFUN`) — extend existing files, work through clusters
9. **L** (`optimize.portfolio`) — largest file, tackle in sub-passes by line cluster
