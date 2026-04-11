### Regression tests for GitHub issue #26
### "Maximum Sharpe Ratio fails with error in PortfolioAnalytics"
###
### When all feasible portfolios have nearly identical expected returns
### (e.g. very low-volatility ETF data), max_sr_opt computes min_mean >= max_mean
### and optimize() throws "xmin not less than xmax".
###
### Fix: guard in max_sr_opt (and the analogous mean_etl_opt) so that when
### min_mean >= max_mean, we return max_mean directly and emit a warning
### instead of throwing an error.

library(testthat)
library(PortfolioAnalytics)
library(PerformanceAnalytics)

skip_if_not_installed("PortfolioAnalytics")
skip_if_not_installed("PerformanceAnalytics")

# The exact data from issue #26: two near-zero-variance ETFs
returns_text <- '
"date","NEAR","MINT"
"2014-07-01",0.000161110006774212,-9.86193284648884e-05
"2014-07-02",-0.000218949044995531,0.000197258111483523
"2014-07-03",-0.000378267533203247,9.86096047428386e-05
"2014-07-07",0,-9.85998818474609e-05
"2014-07-08",-0.000199163513616485,9.86096047428386e-05
"2014-07-09",0.000197211155088084,0.000197199763695144
"2014-07-10",0.000201155547844056,0
"2014-07-11",0.000203106333835912,0.000197160883615322
"2014-07-14",-0.000203065090029986,0
"2014-07-15",0.000509757069411476,0.000169524935917709
"2014-07-16",-0.000111452544867996,-0.00016949620208484
"2014-07-17",-0.000597133756883639,0.000394244036637348
"2014-07-18",0.000597490538653656,-0.000492610837184437
"2014-07-21",0,9.85707246621281e-05
"2014-07-22",-0.000199044586359554,0.000295683027205396
"2014-07-23",-3.78259998102815e-05,-0.000394127499825259
"2014-07-24",3.78274306707116e-05,0
"2014-07-25",-0.000597252636787449,-0.000197141449324145
"2014-07-28",0.000398406373049998,0.000295770482607871
"2014-07-29",0.00019912385541021,0.000295683027205396
"2014-07-30",0,-0.000197063750457582
"2014-07-31",0,0.000502611610304848
"2014-08-01",0.00054833764653095,0.000197122017793872
"2014-08-04",-0.000199123854866534,-0.000689791091990632
"2014-08-05",-0.000300736903335763,0.000493048023763265
"2014-08-06",-0.0001990544369285,-0.000197122017793872
"2014-08-07",-0.000199178218073773,-0.000394048003780578
"2014-08-08",0.000200119895905534,0.000197141449324145
"2014-08-11",3.99080135148682e-05,9.85609574437753e-05
"2014-08-12",-0.000199164553019012,9.85511367523279e-05
"2014-08-13",0.000398847766073047,0.000197102877869048
"2014-08-14",0.000199642289501723,9.85218806568754e-05
"2014-08-15",-9.95221226428151e-05,-0.000394047226892949
"2014-08-18",9.96210640440068e-05,0.000394144286419849
"2014-08-19",0.000297467527186956,0.000394047226892949
"2014-08-20",-0.000199222779832965,-0.00029693445148785
"2014-08-21",-0.000298850592562659,-9.85023706399994e-05
"2014-08-22",0,0
"2014-08-25",0.000299146516826956,9.84927495785117e-05
"2014-08-26",0.000298103030636826,0.000394047226892949
"2014-08-27",-0.000397007498893768,-9.84927495785117e-05
"2014-08-28",0.000199398796254086,-9.84927495785117e-05
"2014-08-29",-0.0001992011009434,-0.000295478248735535
'
R_26 <- as.matrix(read.csv(text = returns_text, row.names = "date"))

test_that("max Sharpe Ratio optimization completes without error on near-zero variance data (#26)", {
  skip_on_cran()
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if_not_installed("ROI.plugin.quadprog")

  ps <- portfolio.spec(colnames(R_26))
  ps <- add.constraint(ps, type = "full_investment")
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "return", name = "mean")
  ps <- add.objective(ps, type = "risk",   name = "StdDev")

  # Should complete without error (may or may not trigger the degenerate warning)
  expect_no_error(
    opt <- optimize.portfolio(R_26, ps,
                              optimize_method = "ROI",
                              maxSR = TRUE)
  )
  expect_true(!is.null(opt$weights))
  expect_equal(length(opt$weights), 2L)
  expect_equal(sum(opt$weights), 1, tolerance = 1e-6)
})

test_that("max Sharpe Ratio optimization works normally on non-degenerate data", {
  skip_on_cran()
  skip_if_not_installed("ROI")
  skip_if_not_installed("ROI.plugin.glpk")
  skip_if_not_installed("ROI.plugin.quadprog")

  data(edhec)
  R <- edhec[, 1:4]
  ps <- portfolio.spec(colnames(R))
  ps <- add.constraint(ps, type = "full_investment")
  ps <- add.constraint(ps, type = "long_only")
  ps <- add.objective(ps, type = "return", name = "mean")
  ps <- add.objective(ps, type = "risk",   name = "StdDev")

  expect_no_error(
    opt <- optimize.portfolio(R, ps, optimize_method = "ROI", maxSR = TRUE)
  )
  expect_true(!is.null(opt$weights))
  expect_equal(sum(opt$weights), 1, tolerance = 1e-6)
})
