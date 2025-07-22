#################### Demo Sp500 return ###################
## 大盤用收整價；個股用調整價

#install.packages("quantmod")
#install.packages("kableExtra")
#install.packages("xts")
#install.packages("zoo")

library(quantmod)
library(xts)
library(zoo)
library(fBasics)      # for basicStats and jarqueberaTest
library(ggplot2)  

#setwd("D:\\統計研習營_2025\\R codes")
Sys.setlocale ("LC_ALL","English")
##getSymbols('^GSPC',source='yahoo',from='2012-01-03', to=Sys.Date())
getSymbols('^GSPC',source='yahoo',from='2012-01-03', to='2025-07-15')
GSPC.rtn=diff(log(GSPC$GSPC.Close))
GSPC.rtn=na.omit(GSPC.rtn)
rtn=GSPC.rtn*100
head(rtn)
tail(rtn)
dim(rtn)
#jpeg(filename = "SP500_price.jpeg",height = 200, width = 300, units = 'mm', res = 300)
chartSeries(GSPC,theme="white")
#dev.off()

chartSeries(GSPC,type="auto", theme=chartTheme("white"),subset="2020-03-01::2020-03-30") 
chartSeries(GSPC,type="candlesticks", theme=chartTheme("white"),subset="2020-03-01::2020-03-30") 
chartSeries(rtn,theme="white")
chartSeries(rtn, theme = "white", name = "SP500 return")

#jpeg(filename = "SP500_rtn.jpeg",height = 200, width = 300, units = 'mm', res = 300)
#chartSeries(rtn, theme = "white")
#dev.off()
###################### Provide summary statistics #####################

basicStats(rtn)  
jarqueberaTest(rtn)

# Extract mean and standard deviation
mu <- mean(rtn)
mu
sigma <- sd(rtn)
sigma
# Convert rtn (xts object) to data frame
rt <- data.frame(Date = index(rtn), Return = as.numeric(rtn))

# Plot: empirical density + normal density
##jpeg(filename = "density_SP500.jpeg", height = 120, width = 210, units = "mm", res = 300)
ggplot(rt, aes(x = Return)) +
  geom_density(aes(fill = "Empirical Density"), alpha = 0.6, color = NA) +
  stat_function(fun = dnorm, 
                args = list(mean = mu, sd = sigma),
                aes(color = "Normal Density"),
                linetype = "dashed", size = 1.0) +
  scale_fill_manual("", values = c("Empirical Density" = "orchid")) +
  scale_color_manual("", values = c("Normal Density" = "cyan3")) +
  labs(title = "Empirical Density of Daily S&P500 Returns",
       x = "Daily Return", y = "Density") +
  theme_minimal() +
  theme(legend.position = "right")
##dev.off()


### Test autocorrelation in the mean equation
Box.test(rtn,lag=5,type = "Ljung")
### Test autocorrelation in the volatility equation
Box.test(rtn^2,lag=5,type = "Ljung")

par(mfrow=c(2,2),mar = c(4, 4, 3, 2))
acf(rtn, ylim = c(0, 1))
pacf(rtn, ylim = c(0, 1))
acf(rtn^2, ylim = c(0, 1))
pacf(rtn^2, ylim = c(0, 1))



#jpeg("ACF_PACF_rtn.jpeg", height = 200, width = 280, units = "mm", res = 300)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 2) + 0.1, oma = c(1, 1, 4, 1), cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

# ACF and PACF of returns
acf(rtn, main = "ACF of Daily Returns", col = "deepskyblue", lwd = 2, ylim = c(0, 1))
pacf(rtn, main = "PACF of Daily Returns", col = "deepskyblue", lwd = 2, ylim = c(0, 1))

# ACF and PACF of squared returns
acf(rtn^2, main = "ACF of Squared Returns", col = "magenta", lwd = 2, ylim = c(0, 1))
pacf(rtn^2, main = "PACF of Squared Returns", col = "magenta", lwd = 2, ylim = c(0, 1))

# Add an overarching title
mtext("ACF and PACF Plots of S&P500 Returns", outer = TRUE, cex = 1.3, font = 2)
#dev.off()


Box.test(rtn,lag=5,type="Ljung")  # Q(5)
Box.test(rtn^2,lag=5,type="Ljung")  # Q^2(5)


### Example 1: TSMC (2330.TW)
# Load TSMC data from Yahoo Finance
getSymbols("2330.TW", src = "yahoo",
           from = "2018-01-01", to = Sys.Date(),
           auto.assign = TRUE)

# Assign TSMC data to variable
TSMC = `2330.TW`

# Compute daily log returns (in percentage)
rtn2 = diff(log(TSMC$`2330.TW.Adjusted`))
rtn2 = na.omit(rtn2)
rtn2 = rtn2 * 100

# Summary statistics
head(rtn2)
tail(rtn2)
summary(rtn2)

# Plot entire series
chartSeries(rtn2, theme = "white")

# Plot a specific time range
chartSeries(TSMC, type = "auto", theme = chartTheme("white"),
            subset = "2025-03-01::2025-04-30")
chartSeries(TSMC, type = "candlesticks", theme = chartTheme("white"),
            subset = "2025-03-01::2025-04-30")

# Plot return series
chartSeries(rtn2, theme = "white")

# Save return series chart to JPEG
#jpeg(filename = "TSMC_rtn.jpeg", height = 200, width = 300, units = "mm", res = 300)
#chartSeries(rtn2, theme = "white",name = "TSMC")
#dev.off()

#################### Demo GOOG daily return ###################
Sys.setlocale("LC_ALL", "English")


# Get Google stock data
getSymbols('GOOG', source = 'yahoo', from = '2007-01-04', to = Sys.Date())

# Compute daily log returns (in %)
GOOG.rtn = diff(log(GOOG$GOOG.Adjusted))
GOOG.rtn = na.omit(GOOG.rtn)
GOOG.rtn = GOOG.rtn * 100

# Display basic information
head(GOOG.rtn)
tail(GOOG.rtn)
dim(GOOG.rtn)

# Plot stock chart
chartSeries(GOOG.rtn, theme = "white",name = "Google")

