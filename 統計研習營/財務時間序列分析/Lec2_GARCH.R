#################### Demo MSFT daily return ###################

library(quantmod)
library(xts)
library(zoo)
library(fBasics)      # for basicStats and jarqueberaTest
library(ggplot2)  
library(rugarch)
#setwd("D:\\統計研習營_2025\\R codes")
Sys.setlocale ("LC_ALL","English")


# Get Microsoft stock data from Yahoo Finance
getSymbols('MSFT', source = 'yahoo', from = '2007-01-04', to = Sys.Date())

# Compute daily log returns in percentage
MSFT.rtn = diff(log(MSFT$MSFT.Adjusted))
MSFT.rtn = na.omit(MSFT.rtn)
MSFT.rtn = MSFT.rtn * 100

# Display basic statistics
head(MSFT.rtn)
tail(MSFT.rtn)
dim(MSFT.rtn)

# Plot the stock price chart
chartSeries(MSFT.rtn, theme = "white")

#jpeg(filename = "MSFT_rtn.jpeg",height = 200, width = 300, units = 'mm', res = 300)
#chartSeries(MSFT.rtn, theme = "white")
#dev.off()

rtn=MSFT.rtn

### Test autocorrelation in the mean equation
Box.test(rtn,lag=5,type = "Ljung")
### Test autocorrelation in the volatility equation
Box.test(rtn^2,lag=5,type = "Ljung")

par(mfrow=c(2,2),mar = c(4, 4, 3, 2))
acf(rtn, ylim = c(0, 1))
pacf(rtn, ylim = c(0, 1))
acf(rtn^2, ylim = c(0, 1))
pacf(rtn^2, ylim = c(0, 1))

par(mfrow=c(1,1))
#GARCH(1,1)N
spec1 = ugarchspec(mean.model = list(armaOrder = c(1,0), include.mean = TRUE), 
                   variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                   distribution.model = "norm")
fit1 = ugarchfit(spec=spec1,data=rtn,solver='hybrid')
fit1
plot(fit1, which = 9)

# GARCH(1,1)-std
spec2 = ugarchspec(mean.model = list(armaOrder = c(1,0), include.mean = TRUE), 
                   variance.model = list(model = 'sGARCH', garchOrder = c(1,1)),
                   distribution.model = "std")
fit2 = ugarchfit(spec=spec2,data=rtn,solver='hybrid')
fit2
plot(fit2, which = 9)

#jpeg(filename = "volatility.jpeg", height = 120, width = 210, units = "mm", res = 300)
# Set tighter margins: c(bottom, left, top, right)
par(mar = c(3, 3, 2, 1))

# Extract conditional standard deviation (σ_t)
sigma2 <- sigma(fit2)
time_index <- as.Date(time(sigma2))  # ensure Date format

# Plot without default X-axis
plot(time_index, sigma2, type = "l", col = "mediumorchid", lwd = 2,
     ylab = expression(sigma[t]),
     xlab = "Time",
     main = "Conditional Standard Deviation",
     xaxt = "n")

# Add custom X-axis: every year
tick_seq <- seq(from = as.Date(format(min(time_index), "%Y-01-01")),
                to   = as.Date(format(max(time_index), "%Y-01-01")),
                by   = "1 year")
axis.Date(1, at = tick_seq, format = "%Y", las = 1, cex.axis = 0.8)

# Add horizontal dotted grid lines
abline(h = pretty(sigma2), lty = "dotted", col = "gray50")

#dev.off()



#jpeg(filename = "volatility2.jpeg", height = 120, width = 210, units = "mm", res = 300)
par(mar = c(3, 3, 2, 1))  # default is c(5, 4, 4, 2) + 0.1
# Extract conditional variance and time index
sigma2_t <- sigma(fit2)^2
time_index <- as.Date(time(sigma2_t))

# Custom plot without default x-axis
plot(time_index, sigma2_t, type = "l", lwd = 1.5, col = "royalblue2",
     main = expression("Conditional Variance " ~ sigma[t]^2),
     ylab = expression(sigma[t]^2),
     xlab = "Time", xaxt = "n")

# Add horizontal dotted grid lines
abline(h = pretty(sigma2_t), lty = "dotted", col = "magenta3")

# Add X-axis: show every year with smaller font
tick_seq <- seq(from = as.Date(cut(min(time_index), "year")),
                to = max(time_index), by = "1 year")
axis.Date(1, at = tick_seq, format = "%Y", las = 1, cex.axis = 0.7)
#dev.off()