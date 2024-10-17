README
================

This V2.1 version of PortfolioAnalytics is an update to the substantial
V2.0 version that was released on 2024-07-03. We first describe the V2.0
features, then discuss the R demo capability, and finally we describe
the additional V2.1 features.

# 2.0 Features

A major feature of 2.0 is the integration of the CVXR solver R package
for convex optimization. CVXR supports eleven solver packages, each of
which supports solvers for one or more of the following optimization
problems: LP, QP, SOCP, SDP, EXP, MIP. See the Table near the beginning
of the document “Convex Optimization in R” at <https://cvxr.rbind.io/>.
Thus, with PortfolioAnalytics 2.0, users are able to use any one of a
variety of solvers available in CVXR for their portfolio optimization
problems.

A particular use of CVXR in PortfolioAnalytics 2.0 is for computing
Minimum Coherent Second Moment (MinCSM) portfolios, which are
second-order cone programming (SOCP) optimization problems. This is
quite a new capability that is not available in other portfolio
optimization software products. Details are provided in the Vignette
“cvxrPortfolioAnalytics”.

Another important feature of PortfolioAnalytics 2.0, is that it contains
functionality for computing outliers-robust minimum variance (MV)
optimal portfolios based on any one of several robust covariance matrix
estimators that are not much influenced by outliers Details are provided
in the Vignette “robustCovMatForPA”.

New PortfolioAnalytics Functions:

1.  meancsm.efficient.frontier (create Mean-CSM efficient frontier)
    utility function
2.  meanrisk.efficient.frontier (generate multiple efficient frontiers
    for portfolios with the same constraint object.
3.  extract_risk (extract the risk value, e.g., StdDev or ES or CSM,
    based on the weights of a portfolio)
4.  chart.EfficientFrontierCompare (Overlay the efficient frontiers of
    different minRisk portfolio objects on a single plot)
5.  backtest.plot (based on Peter Carl’s code, generate plots of the
    cumulative returns and/or drawdown for back-testing)
6.  opt.outputMvo (converts output of `optimize.portfolio` to a list of
    the portfolio weights, mean, volatility and Sharpe Ratio)
7.  plotFrontiers (plot frontiers based on the result of
    `meanvar.efficient.frontier`, `meanetl.efficient.frontier` or
    `meancsm.efficient.frontier`)

Enhanced PortfolioAnalytics Functions:

1.  optimize.portfolio (enhanced with CVXR solvers, CSM objective,
    customizable arg `momentFUN=` and output `~$moment_values`)
2.  optimize.portfolio.rebalancing (enhanced with CVXR solvers, CSM
    objective and customizable arg `momentFUN=`)
3.  create.EfficientFrontier (enhanced with type `mean-CSM` and
    `mean-risk`, and customizable arg `momentFUN=`)

Support S3 Methods for CVXR:

1.  print.optimize.portfolio.CVXR
2.  extractStats.optimize.portfolio.CVXR

Custom Moment Functions for Robust Covariance Matrices:

1.  custom.covRob.MM
2.  custom.covRob.Rocke
3.  custom.covRob.Mcd
4.  custom.covRob.TSGS
5.  MycovRobMcd
6.  MycovRobTSGS

New Vignettes and their Code Functions in the demo Folder:

1.  cvxrPortfolioAnalytics: CRAN title = “CVXR for PortfolioAnalytics”.
2.  demo_cvxrPortfolioAnalytics.R
3.  robustCovMatForPA: CRAN title = “Robust Covariance Matrices for
    PortfolioAnalytics”
4.  demo_robustCovMatForPA.R

# PortfolioAnalytics Demo Scripts
PortfolioAnalytics has contained a substantial number of demo R scripts in the *demo* folder for a long time.  Assuming that an R package is installed, but not necessarily loaded, you can view a list of the names of al the demo folder R scripts with the following R command

demo(package = “packageName”)

which you can easily verify for the case of PortfolioAnalytics.


# New 2.1 Features

xxx

# Bug Reportin

Please contribute with bug fixes, comments, and testing scripts!

Please take your data and disguise it when submitting, or use data sets
like “edhec” like we do in the demos or or like “stocksCRSP” and
“factorsSPGMI” in the PCRA package or with your constraints and other
objectives modified to demonstrate your problem on public data.

Please report any bugs or issues on the PortfolioAnalytics GitHub page
at <https://github.com/braverock/PortfolioAnalytics/issues>

# Acknowledgements

The bulk of the work in creating PortfolioAnalytics 2.0 was done by
Xinran Zhao, along with contributions from Yifu Kang, under the support
of a 2022 Google Summer of Code (GSOC 2022). Xinran and Yifu were
mentored in GSOC 2022 by Professor Doug Martin and Professor Steve
Murray in the Applied Mathematics Department at the University of
Washington.
