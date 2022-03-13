[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **GMM_CAPM** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: 'GMM_CAPM'

Published in: 'METIS'

Description: 'Test Capital Asset Pricing Theory using Generaliyed Method of Moments.'

Keywords: 'Genaralized Method of Moment, CAPM, '

Author: 'Yifu Wang'

```

### R Code
```r

library("quantmod")
library("PerformanceAnalytics")
library("tidyverse")
# install.packages("gmm",destdir = "D:/r_packages/download")
library("gmm")

setwd("E:\\workspace\\R\\metis")
factor_FF3_all<-read.csv("F-F_Research_Data_Factors_daily.csv")
factor_FF3<-factor_FF3_all[factor_FF3_all$X>="20200101"&factor_FF3_all$X<="20201231",]
r_factor_mkt<-factor_FF3$Mkt.RF
r_factor_mkt<-r_factor_mkt[-length(r_factor_mkt)]
r_factor_mkt<-r_factor_mkt[-1]

## component of DJIA
list_DJIA<-c("V","CAT","CVX","VZ","TRV","DIS","MRK","MMM","BA","JNJ",
            "MCD","AMGN","KO","NKE","DOW","IBM","AXP","HD","WBA","UNH",
            "INTC","WMT","HON","AAPL","PG","CRM","MSFT","CSCO","JPM","GS")

setSymbolLookup(s_AAPL=list(name="AAPL",src="yahoo"))
getSymbols("s_AAPL", from="2020-01-01", to="2020-12-31")
r_AAPL = Return.calculate(S_AAPL[, 6], method = "log")
r_AAPL = r_AAPL[(-1)]

r_stock_all<-matrix(nrow = length(r_AAPL),ncol = 30)
colnames(r_stock_all)<-list_DJIA

for (i in 1:30) {
  setSymbolLookup(s_TEMP=list(name=list_DJIA[i],src="yahoo"))
  getSymbols("s_TEMP", from="2020-01-01", to="2020-12-31")
  r_TEMP = Return.calculate(S_TEMP[, 6], method = "log")
  r_TEMP = r_TEMP[(-1)]
  r_stock_all[,i]<-r_TEMP
}

## gmm

### combining the data
x_all<-cbind(r_factor_mkt,r_stock_all)
g_CAPM <- function(tet, x) {
  gmat <- (tet[1] + tet[2] * (1 + c(x[, 1]))) * (1 + x[, 2:31]) - 1
  return(gmat)
  }
res_sdf <- gmm(g_CAPM, x = x_all, c(0, 0))
specTest(res_sdf)

## robustness
list_remove<-c("UNH","V","AAPL","JPM","MSFT")
r_stock_robust<-r_stock_all[,!list_DJIA %in% list_remove]

x_robust<-cbind(r_factor_mkt,r_stock_robust)
g_robust <- function(tet, x) {
  gmat <- (tet[1] + tet[2] * (1 + c(x[, 1]))) * (1 + x[, 2:26]) - 1
  return(gmat)
}
res_sdf_robust <- gmm(g_robust, x = x_robust, c(0, 0))
specTest(res_sdf_robust)

save.image(file = "Metis_GMM.RData")
load(file = "Metis_GMM.RData")

```

automatically created on 2022-03-13