# this is modified from a classroom script
# supplied by Anatoly Schmidt of NYU on R-SIG-Finance on 2026-04-08
# we should use it to test CVXR and ROI in common use cases

library(quantmod)
# library(tseries)
# library(forecast)
# library(rugarch)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(CVXR)

# parameters
datasource <- "Y"
# tkr_list <- c("AAPL", "MSFT", "GOOGL", "NFLX", "ADBE", "NVDA")
tkr_list <- c("MRK", "CVX", "IBM", "WMT", "JPM")

# basket <- "portf1"
basket <- "portf2"
startDate <- as.Date("2015-01-02")
endDate <- as.Date("2019-12-31")

getSymbols(tkr_list, from = startDate, to = endDate, src = "yahoo")

# apply over tkr_list symbols and construct a xts object of returns using the close prices
# prices <- do.call(merge, lapply(tkr_list, function(x) Ad(get(x))))
prices <- do.call(merge, lapply(tkr_list, function(x) Ad(get(x))))

# returns
returns <- do.call(merge, lapply(tkr_list, function(x) dailyReturn(Cl(get(x)))))

names(returns) <- tkr_list
head(returns)

fund.names <- colnames(returns)
pspec <- portfolio.spec(assets = fund.names)
# print.default(pspec)
pspec <- add.constraint(portfolio = pspec, type = "full_investment")
pspec <- add.constraint(portfolio = pspec, type = "long_only")
pspec <- add.objective(portfolio = pspec, type = "return", name = "mean")
pspec <- add.objective(portfolio = pspec, type = "risk", name = "StdDev")

###############################################################################
# Part 1: CVXR as the solver
# create.EfficientFrontier defaults to optimize_method='CVXR'
# optimize.portfolio defaults to DEoptim when optimize_method is not specified,
# so we pass optimize_method='CVXR' explicitly here
###############################################################################

cat("\n=== Part 1: CVXR solver ===\n")

eff_cvxr <- create.EfficientFrontier(returns, pspec,
  type = "mean-StdDev",
  optimize_method = "CVXR",
  n.portfolios = 25
)

maxSharpe_cvxr <- optimize.portfolio(
  R = returns, portfolio = pspec,
  optimize_method = "CVXR", # "DEoptim" is the default, but the objectives should choos  CVXR automatically
  maxSR = TRUE, trace = TRUE
)

chart.EfficientFrontier(eff_cvxr,
  match.col = "StdDev", type = "l",
  RAR.text = "Sharpe Ratio", pch = 4,
  main = "Efficient Frontier (CVXR)"
)

cat("maxSharpe_cvxr names:\n")
print(names(maxSharpe_cvxr))
cat("maxSharpe_cvxr$weights:\n")
print(maxSharpe_cvxr$weights)

###############################################################################
# Part 2: ROI as the explicit solver
# passing optimize_method='ROI' everywhere it is accepted
###############################################################################

cat("\n=== Part 2: ROI (explicit solver) ===\n")

eff_roi <- create.EfficientFrontier(returns, pspec,
  type = "mean-StdDev",
  optimize_method = "ROI",
  n.portfolios = 25
)

maxSharpe_roi <- optimize.portfolio(
  R = returns, portfolio = pspec,
  optimize_method = "ROI", maxSR = TRUE, trace = TRUE
)

# minSD.portfolio <- add.objective(portfolio=pspec,
#                                 type="risk",
#                                 name="StdDev")
# minSD.opt <- optimize.portfolio(R = returns, portfolio = minSD.portfolio,
#                                optimize_method = "ROI", trace = TRUE)

chart.EfficientFrontier(eff_roi,
  match.col = "StdDev", type = "l",
  RAR.text = "Sharpe Ratio", pch = 4,
  main = "Efficient Frontier (ROI)"
)

cat("maxSharpe_roi names:\n")
print(names(maxSharpe_roi))
cat("maxSharpe_roi$weights:\n")
print(maxSharpe_roi$weights)

###############################################################################
# Compare weights from CVXR and ROI
###############################################################################

cat("\n=== Weight comparison (CVXR vs ROI) ===\n")
comparison <- rbind(
  CVXR = maxSharpe_cvxr$weights,
  ROI  = maxSharpe_roi$weights
)
print(round(comparison, 6))
