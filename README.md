README
================

<!-- badges: start -->
[![Downloads from the RStudio CRAN mirror](https://cranlogs.r-pkg.org/badges/PortfolioAnalytics)](https://cran.r-project.org/package=PortfolioAnalytics)
[![R-CMD-check](https://github.com/braverock/PortfolioAnalytics/workflows/R-CMD-check/badge.svg)](https://github.com/braverock/PortfolioAnalytics/actions)
[![Codecov test coverage](https://codecov.io/gh/braverock/PortfolioAnalytics/graph/badge.svg)](https://app.codecov.io/gh/braverock/PortfolioAnalytics)
<!-- badges: end -->

## Overview

`PortfolioAnalytics` is a comprehensive R package designed to provide numerical methods and visualization tools for portfolio optimization and analysis. It is built to handle complex portfolio optimization problems with a wide range of constraints and objectives, supporting both traditional and modern risk measures.

### Main Use Cases
*   **Constrained Optimization**: Define and solve optimization problems with box, group, turnover, diversification, leverage, and factor exposure constraints.
*   **Custom Objectives**: Optimize for various targets including mean return, variance, expected shortfall (CVaR), risk budgets, and coherent second moments (CSM).
*   **Backtesting**: Evaluate portfolio performance over time with rebalancing and walk-forward analysis.
*   **Visualization**: Analyze efficient frontiers, risk-reward trade-offs, and weight distributions through extensive charting capabilities.

### Advanced Features
*   **CVXR Integration**: Seamlessly use the `CVXR` solver infrastructure for convex optimization problems (LP, QP, SOCP, SDP, MIP).
*   **Robust Covariance Estimators**: Compute outliers-robust portfolios using MM-estimators, Rocke's estimator, Minimum Covariance Determinant (MCD), and more.
*   **Multi-layer Optimization**: Define hierarchical portfolio structures for sub-portfolio optimization.
*   **Regime Switching**: Optimize portfolios across different market regimes.
*   **Custom Moments and Objectives**: Flexibility to provide user-defined moment functions and objective functions.

## Supported Optimization Engines

`PortfolioAnalytics` supports a variety of global and local optimization engines:

*   **CVXR**: A modern convex optimization interface supporting various solvers like ECOS, OSQP, and SCS.
*   **Random Portfolios**: Fast, heuristic optimization using sample, simplex, or grid-based portfolio generation.
*   **DEoptim**: Differential Evolution optimization for non-convex and non-smooth problems.
*   **ROI (R Optimization Infrastructure)**: A unified interface to multiple solvers including GLPK, Symphony, and Quadprog.
*   **GenSA**: Generalized Simulated Annealing for global optimization.
*   **pso**: Particle Swarm Optimization.
*   **mco**: Multi-objective optimization using genetic algorithms (e.g., NSGA-II).
*   **osqp**: Operator Splitting Solver for Quadratic Programming.
*   **Rglpk**: Interface to the GNU Linear Programming Kit.

## Documentation and Examples

Comprehensive documentation is available in the `vignettes` directory:
*   `portfolio_vignette`: Introductory guide to the package.
*   `cvxrPortfolioAnalytics`: Deep dive into using CVXR for convex optimization.
*   `robustCovMatForPA`: Detailed explanation of robust covariance matrix estimators.
*   `ROI_vignette`: Guide to using the ROI solver infrastructure.
*   `risk_budget_optimization`: Focus on risk budgeting techniques.
*   `custom_moments_objectives`: Instructions for extending the package with custom logic.

The package also includes extensive examples and demo scripts:
*   `demo/`: Over 30 scripts covering everything from basic setup to replicating research papers (e.g., `demo_JPM2024MinDownsideRisk.R`).
*   `inst/examples/`: Additional example scripts for specific use cases.

## Release Summary

### [2.1.x] - Current
*   **Enhanced Visualization**: Improved graphical displays for multiple efficient frontiers and robust covariance estimator settings.
*   **Solver Updates**: Continued refinements to CVXR and ROI integrations.
*   **JPM 2024 Paper Replication**: Added scripts to replicate "Minimum Downside Risk Portfolios" (October 2024).
*   **Risk Measure Update**: Replaced EQS (Expected Quadratic Shortfall) with CSM (Coherent Second Moment).

### [2.0.x] - 2024
*   **CVXR Integration**: Major feature allowing access to eleven solver packages via `CVXR`.
*   **MCSM Portfolios**: Capability for Minimum Coherent Second Moment (MCSM) portfolios using SOCP.
*   **Robust Portfolio Optimization**: Integrated several robust covariance matrix estimators (MM, Rocke, Mcd, TSGS).
*   **New Utility Functions**: Added `extract_risk`, `chart.EfficientFrontierCompare`, `opt.outputMvo`, and more.

### [1.x] - 2013-2023
*   **Rebalancing Support**: Enhanced `optimize.portfolio.rebalancing` for backtesting and walk-forward analysis.
*   **Expanded Statistics**: Added `extractStats` and refined S3 methods for optimization outputs.
*   **Group Constraints**: Robust support for hierarchical group-based constraints.

### [0.x] - 2009-2012
*   **Package Foundation**: Initial development (originally named `optimizer`).
*   **Core Solvers**: Introduced `DEoptim` and `Random Portfolios` support.
*   **Initial ROI Support**: Early integration with the R Optimization Infrastructure.

## Thanks

`PortfolioAnalytics` has benefited immensely from the support of the open-source community and academic funding:

*   **Google Summer of Code (GSoC)**: We are grateful for the project funding provided by Google over several years, including 2013, 2014, and 2022.
*   **Student Contributors**:
    *   **Xinran Zhao** (GSoC 2022) - CVXR integration and CSM risk measures.
    *   **Yifu Kang** (GSoC 2022) - Robust covariance matrix estimators.
    *   **Ross Bennett** (GSoC 2013, 2014) - Core framework development, rebalancing, and documentation.
*   **Mentors**: Doug Martin, Steve Murray, Brian Peterson, Peter Carl, and Guy Yollin.
*   **Community**: Thank you to all contributors who have submitted bug reports, feature requests, and code patches. Special thanks to the many users who have shared their expertise to make this package a industry standard for portfolio optimization in R.

For bug reporting and contributions, please visit our [GitHub Issues page](https://github.com/braverock/PortfolioAnalytics/issues).
