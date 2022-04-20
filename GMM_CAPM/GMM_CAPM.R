### Estimating a system of equations: CAPM ###
## install packages ##
libraries = c("gmm", "quantmod", "tidyverse", "PerformanceAnalytics","RColorBrewer")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )

library("quantmod")
library("PerformanceAnalytics")
library("tidyverse")
library("gmm")

## import data ##
# wd_current <- getwd()
# factor_FF3_all <- read.csv(paste(wd_current, "/F-F_Research_Data_Factors_daily.csv",sep = ""),skip = 4)
factor_FF3_all <- read.csv("/Users/yifuwang/Downloads/F-F_Research_Data_Factors_daily.csv",skip = 4)
factor_FF3 <- factor_FF3_all[factor_FF3_all$X>="20200101"&factor_FF3_all$X<="20201231",]

r_factor_mkt <- factor_FF3$Mkt.RF
r_free <- factor_FF3$RF

## component of DJIA ##
list_DJIA <- c("V","CAT","CVX","VZ","TRV","DIS","MRK","MMM","BA","JNJ",
             "MCD","AMGN","KO","NKE","DOW","IBM","AXP","HD","WBA","UNH",
             "INTC","WMT","HON","AAPL","PG","CRM","MSFT","CSCO","JPM","GS")

setSymbolLookup(s_AAPL=list(name="AAPL",src="yahoo"))
getSymbols("s_AAPL", from="2020-01-01", to="2020-12-31")
r_AAPL <- Return.calculate(S_AAPL[, 6], method = "log")
r_AAPL <- na.fill(r_AAPL,0)

r_stock_all<-matrix(nrow = length(r_AAPL),ncol = length(list_DJIA))
colnames(r_stock_all)<-list_DJIA

for (i in 1:30) {
  setSymbolLookup(s_TEMP=list(name=list_DJIA[i],src="yahoo"))
  getSymbols("s_TEMP", from="2020-01-01", to="2020-12-31")
  r_TEMP = Return.calculate(S_TEMP[, 6], method = "log")
  r_TEMP <- na.fill(r_TEMP,0)
  r_stock_all[,i]<-r_TEMP
}

## gmm estimation and test ##
## unrestricted model ##
res_1 <- gmm((r_stock_all - r_factor_mkt[-length(r_factor_mkt)])~r_factor_mkt[-length(r_factor_mkt)],
             x = r_factor_mkt[-length(r_factor_mkt)])
coef(res_1)
specTest(res_1)

## restricted model ##
res_2 <- gmm((r_stock_all - r_factor_mkt[-length(r_factor_mkt)])~r_factor_mkt[-length(r_factor_mkt)] - 1,
            x = cbind(1,r_factor_mkt[-length(r_factor_mkt)]))
coef(res_2)
specTest(res_2)

## robustness ##
list_remove <- c("UNH","V","AAPL","JPM","MSFT")
r_stock_robust <- r_stock_all[,!list_DJIA %in% list_remove]

res_robust <- gmm((r_stock_robust - r_factor_mkt[-length(r_factor_mkt)]) ~ r_factor_mkt[-length(r_factor_mkt)] - 1,
                  x = cbind(1,r_factor_mkt[-length(r_factor_mkt)]))
specTest(res_robust)

save.image(file = "Metis_GMM.RData")
# load(file = "Metis_GMM.RData")

## plot time seies of returns ##
date_xts <- as.Date(factor_FF3$X,"%Y%m%d")
r_factor_mkt_xts <- xts(r_factor_mkt[-length(r_factor_mkt)], order.by = date_xts[-length(date_xts)])
r_stock_all_xts <- xts(r_stock_all, order.by = date_xts[-length(date_xts)])

n <- 30

myCol = c("pink1", "violet", "mediumpurple1", "slateblue1", "purple", "purple3",
          "turquoise3", "skyblue", "steelblue", "blue2", "navyblue",
          "orangered", "tomato", "coral2", "palevioletred", "violetred", "red2",
          "springgreen2", "darkgreen", "palegreen4",
          "darkolivegreen", "tan4", "tan2", "tan3", "brown",
          "grey40", "grey50", "grey30")
# alternative color scheme
# myCol1 <- colorRampPalette(c("steelblue", "#FFFFFF", "brown"))(30)
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# col_plot <- col_vector[seq(2,(2*n+2),by=2)]
# plot.xts(r_stock_all_xts["2020-02"], col = myCol)
# addLegend(lty = rep(1,30),col = myCol, ncol = 5)

# manually plot the time series in different months 
png(paste("Metis_tsplot",as.character(1),".png", sep = ""), width = 900, height = 600, bg = "transparent")
par(mfrow=c(2,1))
plot.xts(r_stock_all_xts["2020-1"], col = myCol, main = "Daily returns of components of Dow Jones Industrial Average", bg = "transparent")
plot.xts(r_factor_mkt_xts["2020-1"], main = "Daily risk premium", bg = "transparent")
dev.off()
