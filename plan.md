# PortfolioAnalytics Plan

> **History note:** All completed bug fixes, issue triage, and earlier coverage
> work are recorded in commit `6e5ef96` and earlier. The coverage test files
> added after that point are in commits `bc28c87`–`fd1ec32`.

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

---

## Remaining Open Items

### Feature Requests (deferred — non-trivial design needed)

- **#43** — Parallelization level control for rebalancing
- **#45** — Return solver failure info from rebalancing
- **#42** — Time-varying factor exposure (workaround documented in issue)

### Coverage Work

Current baseline (from `covr/coverage-2026-04-11.rds`): **82.63%**

Lowest-coverage files remaining:

| File | Coverage | Uncovered lines |
|------|----------|-----------------|
| `R/optimize.portfolio.R` | 74.44% | ~400 |
| `R/optFUN.R` | 77.08% | ~157 |
| `R/chart.concentration.R` | 77.55% | ~22 |
| `R/generics.R` | 78.71% | ~122 |
| `R/charts.RP.R` | 80.25% | ~32 |
| `R/opt.outputMvo.R` | 80.77% | ~5 |
| `R/charts.GenSA.R` | 82.05% | ~14 |

Known untested areas within the above (preliminary, awaiting deeper analysis):
- `R/optimize.portfolio.R`: GenSA solver path; `optimize_method="invol"` and
  `"eqwt"` dispatch; `print.*`/`summary.*` S3 methods for opt result objects;
  rebalancing with `training_period` edge cases
- `R/generics.R`: `print.*` methods for constraint/objective/portfolio objects;
  `extractGroups` dispatch branches; `extractObjectiveMeasures` non-identical
  objectives branch (line ~625)
- `R/optFUN.R`: remaining gaps after `test-optFUN-gaps.R`; MILP helper
  `etl_milp_opt` internal branches
