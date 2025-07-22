#install.packages("quantmod")     # Only run once if not installed

#setwd("D:\\統計研習營_2025\\R codes")
library(quantmod)
Sys.setlocale("LC_ALL", "English")

# 下載 VIX 資料
getSymbols("^VIX", src = "yahoo", from = "2019-01-01", to = Sys.Date())

# 取收盤價，並移除缺失值
vix_close <- na.omit(Cl(VIX))


# 畫圖並輸出成 JPEG
#jpeg(filename = "VIX.jpeg", height = 200, width = 300, units = 'mm', res = 300)
plot(vix_close,
     main = "VIX Closing Prices (2019–Present)",
     xlab = "Date",
     ylab = "VIX",
     col = "deepskyblue",
     cex.main = 1.8,   # Title size
     cex.lab = 1.5,    # Axis label size
     cex.axis = 1.3    # Tick label size
)


#dev.off()
