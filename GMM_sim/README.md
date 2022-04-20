[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **GMM_sim** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: 'GMM_sim'

Published in: 'METIS'

Description: 'Comparison of GMM, 2SLS and OLS based on simulated data'

Keywords: 'Genaralized Method of Moment, Instrument Variables, 2-Stage Least Square, Ordinary Least Square'

Author: 'Yifu Wang'

```

### R Code
```r

## install and load packages ##
libraries = c("gmm", "mvtnorm", "tidyverse", "MASS")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)} )

library(gmm)
library(mvtnorm)
library(tidyverse)
library(MASS)
        
set.seed(1234)
sig <- matrix(c(1, .9, .9, 1), 2, 2)
# n may affect the performances of the MSC
n <- 100
e <- rmvnorm(n, sigma=sig)
zi <- rnorm(n)
## endogeneity setup ##
xi <- exp(-zi^2) + e[,1]
x_regressor <- cbind(rep(1, n), xi)
y0 <- 0.3 * xi + e[, 2]

# GMM-BIC function
criteria_BIC <- function(c, p) {
  BIC <- (c - p) * log(n)
  return(BIC)
}
# GMM-HQIC function Q=2.1
criteria_HQIC <- function(c, p) {
  HQIC <- (c - p) * 2.1 * log(log(n))
  return(HQIC)
}

## set moment conditions 
moment_condition_2 <- cbind(zi, zi^2)

moment_condition_3 <- cbind(zi, zi^2, zi^3)

moment_condition_4 <- cbind(zi, zi^2, zi^3, zi^4)

moment_condition_5 <- cbind(zi, zi^2, zi^3, zi^4, zi^5)

relation <- y0~xi

data_all <- data.frame(cbind(xi, y0))

list_gmm_2 <- gmm(relation, x=moment_condition_2)
summary(list_gmm_2)
y_gmm_1 <- fitted(list_gmm_2)

list_gmm_3 <- gmm(relation, x=moment_condition_3)
summary(list_gmm_3)
y_gmm_2 <- fitted(list_gmm_3)

list_gmm_4 <- gmm(relation, x=moment_condition_4)
summary(list_gmm_4)
y_gmm_3 <- fitted(list_gmm_4)

list_gmm_5 <- gmm(relation, x=moment_condition_5)
summary(list_gmm_5)
y_gmm_4 <- fitted(list_gmm_5)

list_iv <- gmm(relation, x=zi)
summary(list_iv)
y_iv <- fitted(list_iv)

list_ols <- lm(relation)
y_ols <- fitted(list_ols)

k <- length(list_ols$coefficients) - 1
resid_ols <- y0 - y_ols
# Estimated residual standard error
sigma2_est <- sum(resid_ols^2) / (n - k - 1)
var_y_est <- sigma2_est*t(xi)%*%ginv(x_regressor%*%t(x_regressor))%*%xi
y_ols_ci <- qt(.975, df = n-k-1)*sqrt(var_y_est)

data_all <- data.frame(cbind(xi, y0, y_gmm_1, y_gmm_2, y_gmm_3, y_gmm_4, y_iv, y_ols))

# calculate the CI of OLS
## remove the lattice
ggplot(data_all, aes(x = xi, y = y0)) +
  geom_point() +
  geom_line(aes(xi, y_ols),colour = "blue",lwd = 2) +
  geom_ribbon(aes(ymin=y_ols-rep(y_ols_ci,n), ymax=y_ols+rep(y_ols_ci,n)), alpha=0.1, fill = "blue", 
              color = "black", linetype = "dotted") +
  # geom_line(aes(xi, y_gmm_1),colour = "pink",lwd = 2)+
  # geom_line(aes(xi, y_gmm_2),colour = "red",lwd = 2)+
  geom_line(aes(xi, y_gmm_3),colour = "orange",lwd = 2) +
  # geom_line(aes(xi, y_gmm_4),colour = "pink",lwd = 2)+
  geom_line(aes(xi, y_iv),colour = "dark green",lwd = 2) +
  geom_line(aes(xi, .3*xi),colour = "dark grey",lwd = 2) +
  theme_bw() +
  theme(plot.background = element_blank()) +
  labs(x = "x") +
  labs(y = "y")
# ggsave("metis_gmm.png")

# calculate GMM-BIC
BIC_2moments <- criteria_BIC(2,2)
BIC_3moments <- criteria_BIC(3,2)
BIC_4moments <- criteria_BIC(4,2)
BIC_5moments <- criteria_BIC(5,2)

# calculate GMM-HQIC
HQIC_2moments <- criteria_HQIC(2,2)
HQIC_3moments <- criteria_HQIC(3,2)
HQIC_4moments <- criteria_HQIC(4,2)
HQIC_5moments <- criteria_HQIC(5,2)

# J test
J_test_2moments <- specTest.gmm(list_gmm_2)
J_2moments <- J_test_2moments$test[1]
J_test_3moments <- specTest.gmm(list_gmm_3)
J_3moments <- J_test_3moments$test[1]
J_test_4moments <- specTest.gmm(list_gmm_4)
J_4moments <- J_test_4moments$test[1]
J_test_5moments <- specTest.gmm(list_gmm_5)
J_5moments <- J_test_5moments$test[1]

# MSC_BIC
GMM_MSC_BIC_2moments <- J_2moments+BIC_2moments
GMM_MSC_BIC_3moments <- J_3moments+BIC_3moments
GMM_MSC_BIC_4moments <- J_4moments+BIC_4moments
GMM_MSC_BIC_5moments <- J_5moments+BIC_5moments

# MSC_HQIC
GMM_MSC_HQIC_2moments <- J_2moments+HQIC_2moments
GMM_MSC_HQIC_3moments <- J_3moments+HQIC_3moments
GMM_MSC_HQIC_4moments <- J_4moments+HQIC_4moments
GMM_MSC_HQIC_5moments <- J_5moments+HQIC_5moments

```

automatically created on 2022-03-13
