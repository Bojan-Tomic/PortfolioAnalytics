# PortfolioAnalytics Plan

We have completed major test coverage, bug fixes, and a comprehensive
optimization solvers vignette for `PortfolioAnalytics`. This document
summarizes conventions, current status, and remaining work.

## Current Status

- **Branch:** `feature/optimization-solvers-vignette` — 15 commits, not yet pushed
- **Vignette:** `vignettes/optimization_solvers.qmd` — feature-complete, renders to HTML and PDF
- **Bugs fixed this session:** BUG-13 through BUG-16 (see git log for details)
- **Enhancements:** #50 (osqp/Rglpk factor exposure, CLOSED), #52 (`fn_map()` QP projection, CLOSED)
- **Open issue:** #51 — CSM in metaheuristics (standalone `CSM()` function, design work needed)

### Coverage

| Checkpoint | R-only coverage | Source |
|------------|----------------:|--------|
| Initial baseline | 86.5% | `covr/coverage-2026-04-11-session2.rds` |
| After vignette session | **93.23%** (7827/8395) | `covr/coverage-2026-04-12.rds` |
| Delta | **+8.08 pp** | |

#### Key file improvements

| File | Before | After | Delta |
|------|-------:|------:|------:|
| `R/optimize.portfolio.R` | 71.02% | 92.13% | +21.11 pp |
| `R/constrained_objective.R` | 85.71% | 97.20% | +11.49 pp |
| `R/optFUN.R` | 83.59% | 94.13% | +10.54 pp |
| `R/constraint_fn_map.R` | 90.75% | 91.46% | +0.71 pp |

#### Lowest-coverage files (current)

| File | % | Lines | Notes |
|------|--:|------:|-------|
| `R/charts.risk.R` | 86.36 | 190/220 | charting, hard to test |
| `R/charts.multiple.R` | 87.65 | 71/81 | charting |
| `R/plotFrontiers.R` | 89.09 | 49/55 | charting |
| `R/charts.efficient.frontier.R` | 90.49 | 352/389 | charting |
| `R/chart.concentration.R` | 90.62 | 87/96 | charting |
| `R/trailingFUN.R` | 90.62 | 29/32 | |
| `R/EntropyProg.R` | 90.83 | 109/120 | |
| `R/mult.layer.portfolio.R` | 90.91 | 50/55 | |
| `R/constraint_fn_map.R` | 91.46 | 450/492 | |
| `R/charts.ROI.R` | 91.55 | 65/71 | charting |
| `R/extract.efficient.frontier.R` | 91.58 | 261/285 | |
| `R/optimize.portfolio.R` | 92.13 | 1955/2122 | |

Both `R CMD check --as-cran` and `NOT_CRAN=true R CMD check` pass — tests: OK.
Pre-existing warnings (`.gcda` files, missing `inst/doc`, version, compile flags) are unrelated to our work.

---

## Development Conventions and Rules

- Be concise and efficient when communicating. Think all you like, but don't think out loud. Silence is golden. Communicate only when you need input from the user or complete a task
- You are authorized to use parallel subagents to accomplish a task
- All test files live in `tests/testthat/`, named `test-{topic}.R`
- Use only modern testthat 3.x syntax — no `context()`, no `expect_that()`
- Every `test_that()` block must have `skip_on_cran()` inside the block (NOT at file level — file-level `skip_on_cran()` outside `test_that()` skips everything when run under covr)
- Do NOT check exact numerical values for stochastic optimizers
- Tests run against the **installed** package — run `R CMD INSTALL . --no-test-load` after any source change
- Run tests with `NOT_CRAN=true Rscript -e "testthat::test_file(...)"` — never `test_dir()` (crashes with parallel runner)
- Run coverage in only one agent at a time to prevent parallel runner from corrupting covr tracing; do not run `tests/run-all.R` in parallel subagents
- `DESCRIPTION` has `Config/testthat/parallel: true` (`run-all.R` sets environment variable to prevent covr hangs)
- Unexported package functions must be accessed in tests via `PortfolioAnalytics:::`
- Do not end bullets with a period
- Never run `git push` — only `git add` and `git commit`

---

## All Bugs Fixed (reference)

| Bug | File | Summary | Commit |
|-----|------|---------|--------|
| BUG-1 | `R/extractstats.R` | `extractStats.optimize.portfolio.parallel` wrong iteration variable | prior session |
| BUG-2 | `R/custom.covRob.R` | `custom.covRob.Mcd` `match.call()` captures symbol | prior session |
| BUG-3 | `R/moment.functions.R` | `set.portfolio.moments()` `match.call()` for `posterior_p` | prior session |
| BUG-4 | `R/moment.functions.R` | `CCCgarch.MM()` `clean` argument logic inverted | prior session |
| BUG-5 | `R/optimize.portfolio.R` | `optimize.portfolio.rebalancing()` crashes with `regime.portfolios` | prior session |
| BUG-6 | `R/constrained_objective.R` | `constrained_objective_v2()` CSM switch arm leaves `fun` undefined | prior session |
| BUG-7 | `R/optFUN.R` | `gmv_opt_ptc()` target-return formulation wrong RHS | prior session |
| BUG-8 | `R/generics.R` | `summary.optimize.portfolio.parallel` crashes with ROI solver | prior session |
| BUG-9 | PerformanceAnalytics | `VaR`/`ES` portfolio_method='single' doesn't auto-compute moments | UPSTREAM ([#197](https://github.com/braverock/PerformanceAnalytics/issues/197)) |
| BUG-10 | `R/optFUN.R` | `etl_opt` group constraint dimension mismatch | prior session |
| BUG-11 | `R/optFUN.R` | `gmv_opt_toc`/`ptc`/`leverage` NULL cLO/cUP and dir-filter bug | prior session |
| BUG-12 | `R/optFUN.R` | `gmv_opt_ptc` accidentally deleted from source | prior session |
| BUG-13 | `R/optFUN.R` | `etl_milp_opt` group constraint references undefined `Amat` | `83fbeac` |
| BUG-14 | `R/optimize.portfolio.R` | CVXR `EQSratio` missing from Charnes-Cooper conditions | `83fbeac` |
| BUG-15 | `R/optimize.portfolio.R` | CVXR factor exposure constraints not scaled by `weight_scale` | `83fbeac` |
| BUG-16 | `R/optimize.portfolio.R` | CVXR turnover constraint not scaled by `weight_scale` | `d857c7a` |

---

## Remaining Work

### Open Issues

**#51 — CSM in metaheuristics** (OPEN)
- `constrained_objective()` has an empty block for CSM (line ~606 in `R/constrained_objective.R`)
- Need a standalone `CSM()` function (extract from `extract_risk()` which currently couples to CVXR)
- Performance concern: each evaluation requires SOCP solve; needs design for caching/warm-start or closed-form approximation
- Medium effort, design-heavy

### Coverage Gaps (prioritized)

1. **Charting functions** (~86–92%) — 6 files, ~100 uncovered lines total. Hard to test meaningfully in automated tests; diminishing returns
2. **`R/trailingFUN.R`** (90.62%) — 3 uncovered lines
3. **`R/EntropyProg.R`** (90.83%) — 11 uncovered lines
4. **`R/mult.layer.portfolio.R`** (90.91%) — 5 uncovered lines
5. **`R/constraint_fn_map.R`** (91.46%) — 42 uncovered lines, mostly edge cases in `rp_transform()`
6. **`R/extract.efficient.frontier.R`** (91.58%) — 24 uncovered lines
7. **`R/optimize.portfolio.R`** (92.13%) — 167 uncovered lines, many in hard-to-reach error paths

### Other Forward-Looking Items

- **Merge & push** `feature/optimization-solvers-vignette` branch after review
- **CRAN submission prep:** ensure `R CMD check --as-cran` is clean, vignette builds without internet
- **`extract_risk()` CVXR coupling:** If CVXR were ever moved to Suggests, `extract_risk()` and its callers (`meanvar.efficient.frontier()`, `meanrisk.efficient.frontier()`, `plotFrontiers()`) would need `requireNamespace()` guards
