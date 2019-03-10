---
title: "424project"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
options(digits=3, width=70)

library(IntroCompFinR)
library(PerformanceAnalytics)
library(tseries)
library(zoo)
library(boot)
library(corrplot)



# change this to the appropriate path on your computer. This is where some data
# will be saved
savePath="C:/Users/calfa/Documents/ECON 424/project"

getwd()
setwd("C:/Users/calfa/Documents/ECON 424/project")
#
# load data from Yahoo!
#

# get monthly adjusted closing price data on Vanguard mutual fund data from Yahoo
# using the tseries function get.hist.quote. Set sample to December 2009 through
# December 2014. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

asset.names = c("vfinx","veurx","veiex","vbltx","vbisx","vpacx")
start.date = "2014-01-01"
end.date = "2019-01-31"

vfinx.prices = get.hist.quote(instrument="vfinx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")    
veurx.prices = get.hist.quote(instrument="veurx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
veiex.prices = get.hist.quote(instrument="veiex", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vbltx.prices = get.hist.quote(instrument="vbltx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vbisx.prices = get.hist.quote(instrument="vbisx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
vpacx.prices = get.hist.quote(instrument="vpacx", start=start.date,
                             end=end.date, quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
# change time indices to class yearmon, which is most appropriate for monthly data
index(vfinx.prices) = as.yearmon(index(vfinx.prices))
index(veurx.prices) = as.yearmon(index(veurx.prices))
index(veiex.prices) = as.yearmon(index(veiex.prices))
index(vbltx.prices) = as.yearmon(index(vbltx.prices))
index(vbisx.prices) = as.yearmon(index(vbisx.prices))
index(vpacx.prices) = as.yearmon(index(vpacx.prices))

projectPrices.z = merge(vfinx.prices,veurx.prices,veiex.prices,vbltx.prices,
                        vbisx.prices,vpacx.prices)
colnames(projectPrices.z) = asset.names
# create data.frame for downloading
projectPrices.df = coredata(projectPrices.z)
rownames(projectPrices.df) = as.character(index(projectPrices.z))

#


#
# Create matrix of return data and compute pairwise scatterplots
#
projectReturns.z = diff(log(projectPrices.z))   

ret.mat = coredata(projectReturns.z)

#
# compute descriptive statistics
#

muhat.vals = colMeans(projectReturns.z)
sd.vals = apply(projectReturns.z, 2, sd)
skew.vals = apply(projectReturns.z, 2, skewness)
ekurt.vals = apply(projectReturns.z, 2, kurtosis)
cov.mat = var(projectReturns.z)
cor.mat = cov2cor(cov.mat)
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- 
  c("vfinx,veurx","vfinx,veiex","vfinx,vbltx", "vfinx,vbisx", "vfinx,vpacx",
    "veurx,veiex", "veurx,vbltx", "veurx,vbisx", "veurx,vpacx",
    "veiex,vbltx", "veiex,vbisx", "veiex,vpacx",
    "vbltx,vbisx", "vbltx,vpacx",
    "vbisx,vpacx")
# empirical quantiles for VaR calculations
q.vals = apply(projectReturns.z, 2, quantile, prob=c(0.01,0.05))

# display results in a table
stats.mat = rbind(muhat.vals, 
                  sd.vals,
                  skew.vals,
                  ekurt.vals,
                  q.vals)
rownames(stats.mat) = c("Mean", "Std Dev", "Skewness", 
                        "Excess Kurtosis", "1% Quantile", 
                        "5% Quantile")
# print statistics
stats.mat

```

## 1. Return Calculation and Sample Statistics 

### 1.1 Time Trends of Fund Prices and Continuously Compounded Returns
```{r simple and cc returns, echo=FALSE}

#
# compute cc and simple returns
#

projectReturns.z = diff(log(projectPrices.z))   
projectReturnsSimple.z = exp(projectReturns.z) - 1
# create data.frame for downloading
projectReturns.df = as.data.frame(coredata(projectReturns.z))
rownames(projectReturns.df) = as.character(index(projectReturns.z))
projectReturnsSimple.df = as.data.frame(coredata(projectReturnsSimple.z))
rownames(projectReturnsSimple.df) = as.character(index(projectReturnsSimple.z))


#
# plot data
#
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}

plot(projectPrices.z, col="blue", lwd=2)
plot(projectReturns.z, panel=my.panel, col="blue", lwd=2)


```

The prices of VEURX and VEIEX decreased a large amount throughout 2018, and VFINX dipped a bit near the middle to end of 2018 as well. We see this reflected in the returns, as the returns of all 3 assets were in the negatives in late 2018. VBLTX and VBISX seem to be more stable and exhibit less sharp of a decrease in returns, and also shows less volatility over the years. An event that might explain these low returns in mid to late 2018 is the stock market crash that happened in October 2018, which lasted until late December. The stock market crash happened due to the trade war with China, where the Trump administration induced tariffs on a huge amount of Chinese imports. In addition to this, the Federal Reserve increased interest rates, which was the third time they had increased rates that year. 

```{r cum returns, echo=FALSE}
# plot growth of $1 over the five years using PerformanceAnalytics function
# chart.CumReturns
chart.CumReturns(projectReturnsSimple.z, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="growth of $1") 
```

VFINX gives the highest value. I am not surprised, since the S&P 500 is a fund with 500 of the largest US stocks, so it has the largest capitalization of well-performing stocks. Since many of the company stocks in the S&P 500 have grown explosively in the last 5 years, it makes sense that the VFINX fund has grown the most out of the 6 funds. 

###1.2 Four-Panel Plot of Return Series 

```{r return series, echo=FALSE}
fourPanelPlot(projectReturns.z[, "vfinx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "veurx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "veiex", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vbltx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vbisx", drop=FALSE])
fourPanelPlot(projectReturns.z[, "vpacx", drop=FALSE])


```

The returns do not look normally distributed. 
For the returns for VFINX, we can see that in the QQ-plot, the points deviate from linearity in the left and right tails. In the box plot, there also appears to be large negative outliers and some positive outliers. Also in the histogram, we see that there is skew towards the left. These all indicate that the returns are not normally distributed. There does not appear to be any linear time dependence since the SACF plot shows no significatn autocorrelations for lags less than 15, and all of the sample autocorrelations are small and show no distinct pattern over time. 
For VEURX, the QQ-plot shows deviation from linearity in left and right tails, and also there are breaks in the middle of the plot. Also, in the histogram we see that the graph has fat left and right tails and is not clustered around the mean. These indicate non-normality for this return series. There does not appear to be linear time dependence, since there are no patterns over time. 
For VEIEX, the histogram shows positive skew. In the qq-plot, the points deviate from linearity in the left tail, and we see outliers in the left and right tails. The boxplot also indicates positive skew. These all indicate the series is not normally distributed.  There does not appear to be linear time dependence, since there are no patterns over time. 
For VBLTX, the histogram indicates left skew. The qq-plot shows deviation from linearity in the left and right tails. The boxplot also shows us there are positive and negative outliers. Therefore the series is not normally distributed. There does not appear to be linear time dependence, since there are no patterns over time. 
For VBISX, the histogram indicates right skew. The qq plot shows strong deviation from linearity in the right tail. The boxplot indicates one positive outlier. Therefore the series is not normally distributed.  There does not appear to be linear time dependence, since there are no patterns over time. 
For VPACX, the histogram indicates some right skew, but generally looks pretty  normally distributed. However, from the qq-plot we see that there is deviation from linearity in the left and right tails. Also, the boxplot indicates there are many negative outliers, and one positive one. Therefore the series is not normally distributed. There does not appear to be linear time dependence, since there are no patterns over time. 

### 1.3 Descriptive Statistics 

```{r des stat, echo=FALSE}
# compute descriptive statistics
#

muhat.vals = colMeans(projectReturns.z)
sd.vals = apply(projectReturns.z, 2, sd)
skew.vals = apply(projectReturns.z, 2, skewness)
ekurt.vals = apply(projectReturns.z, 2, kurtosis)
cov.mat = var(projectReturns.z)
cor.mat = cov2cor(cov.mat)
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- 
  c("vfinx,veurx","vfinx,veiex","vfinx,vbltx", "vfinx,vbisx", "vfinx,vpacx",
    "veurx,veiex", "veurx,vbltx", "veurx,vbisx", "veurx,vpacx",
    "veiex,vbltx", "veiex,vbisx", "veiex,vpacx",
    "vbltx,vbisx", "vbltx,vpacx",
    "vbisx,vpacx")
# empirical quantiles for VaR calculations
q.vals = apply(projectReturns.z, 2, quantile, prob=c(0.01,0.05))

# display results in a table
stats.mat = rbind(muhat.vals, 
                  sd.vals,
                  skew.vals,
                  ekurt.vals,
                  q.vals)
rownames(stats.mat) = c("Mean", "Std Dev", "Skewness", 
                        "Excess Kurtosis", "1% Quantile", 
                        "5% Quantile")
# print statistics
stats.mat
``` 

The fund with the highest average return is VFINX and the one with the lowest average return is VBISX. VBISX also has the lowest average standard deviation, while VEIEX has the highest average SD. VEIEX has the second smallest skew and has the smallest excess kurtosis, which makes it appear to be the most normally distributed. VFINX looks to be the least normally distributed as it has the largest skewness and also the largest excess kurtosis. We know that for a normal distribution, both skew and excess kurtosis should be 0. 

### 1.4 Sharpe Ratios
```{r sharpe, echo=FALSE}
# plot return-risk tradeoff and compute Sharpe ratios
#
## risk free rate
rf = 0.005/12

plot(sd.vals, muhat.vals, xlim=c(0, 0.06), ylim=c(0, 0.013),
     ylab="Expected Return", xlab="Standard Deviation",
     cex=2, pch=16, col="cornflowerblue")
text(sd.vals, muhat.vals, labels=colnames(projectReturns.z),
     pos=3)

SharpeRatios = (muhat.vals - rf)/sd.vals


# compute bootstrap standard error 
# function to bootstrap VaR
sharpeRatio.boot = function(x, idx, risk.free) {
  muhat = mean(x[idx])
  sigmahat = sd(x[idx])
  sharpeRatio = (muhat - risk.free)/sigmahat
  sharpeRatio
}

sharpe.vfinx.boot = boot(ret.mat[, "vfinx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.veurx.boot = boot(ret.mat[, "veurx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.veiex.boot = boot(ret.mat[, "veiex"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vbltx.boot = boot(ret.mat[, "vbltx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vbisx.boot = boot(ret.mat[, "vbisx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)
sharpe.vpacx.boot = boot(ret.mat[, "vpacx"], 
                         statistic=sharpeRatio.boot, R=999, risk.free=rf)


sharpe.vfinx.boot
sharpe.veurx.boot
sharpe.veiex.boot
sharpe.vbltx.boot
sharpe.vbisx.boot
sharpe.vpacx.boot

boot.sd.vals <- data.frame(sd(sharpe.vfinx.boot$t), sd(sharpe.veurx.boot$t), sd(sharpe.veiex.boot$t), sd(sharpe.vbltx.boot$t), sd(sharpe.vbisx.boot$t), sd(sharpe.vpacx.boot$t))
boot.sd.vals <- rbind(boot.sd.vals, SharpeRatios)

colnames(boot.sd.vals) <- c("vfinx", "veurx", "veiex", "vbltx", "vbisx", "vpacx" )
rownames(boot.sd.vals) <- c("std. error", "sharpe ratios")

boot.sd.vals
```

### 1.5 Standard Errors and 95% Confidence Intervals of Mean and SD
```{r code, echo=FALSE}

# compute standard errors and confidence intervals (do it yourself)
muhat.vals = apply(ret.mat, 2, mean)
sigmahat.vals = apply(ret.mat, 2, sd)

muhat.vals
sigmahat.vals

nobs = nrow(ret.mat)
nobs

##standard error of mean
se.muhat = sigmahat.vals/sqrt(nobs)
se.muhat

##standard error of sd
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
se.sigmahat

## confidence interval for mean
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
cbind(mu.lower,mu.upper)


## confidence interval for sd
sigma.lower = sigmahat.vals - 2*se.sigmahat
sigma.upper = sigmahat.vals + 2*se.sigmahat
cbind(sigma.lower,sigma.upper)
```

The estimated standard deviations are estimated more precisely than the means because the standard errors are small. The SE for sd are less than 1/10 of the size of the estimates  whereas the SE for means are the same size as the estimates, which means they are not estimated precisely.

Looking at the confidence intervals, we also see the estimates for SD are more precise as the confidence interval only contains positive numbers whereas the interval for the mean contains both positive and negative numbers. 

###1.6 
```{r annual sharpe, echo=FALSE}
##compute annualized sharpe ratios
muhat.vals.annualized <- 12*muhat.vals
sd.vals.annualized <- sqrt(12)*sd.vals
SharpeRatios.annualized = (muhat.vals.annualized - rf)/sd.vals.annualized
##print annualized sharpe ratios
SharpeRatios.annualized
```

