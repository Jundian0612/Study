# Load required packages
library(quantmod)
library(tseries)
library(urca)
library(ggplot2)
library(scales)  # for custom date formatting
#setwd("D:\\統計研習營_2025\\R codes")
Sys.setlocale("LC_ALL", "English")

# Set date range
start_date <- as.Date("2012-01-01")
end_date <- as.Date("2024-12-31")

# Get stock data
getSymbols(c("META", "GOOG"), from = start_date, to = end_date, src = "yahoo")

# Extract adjusted prices
meta <- Ad(META)
goog <- Ad(GOOG)

# Merge and clean
prices <- na.omit(merge(meta, goog))
colnames(prices) <- c("META", "GOOG")

# Standardize prices
prices_std <- scale(prices)

# Convert to data frame
df_std <- data.frame(Date = index(prices_std), coredata(prices_std))

# Define rename date
rename_date <- as.Date("2022-06-09")
#jpeg(filename = "Co-move.jpeg", height = 200, width = 300, units = 'mm', res = 300)

ggplot(df_std, aes(x = Date)) +
  geom_line(aes(y = META, color = "META")) +
  geom_line(aes(y = GOOG, color = "GOOG")) +
  geom_vline(xintercept = as.numeric(rename_date), 
             linetype = "dotted", color = "red", size = 1) +
  annotate("text", x = rename_date, y = 2.5, label = "META rename", 
           angle = 90, vjust = -0.5, hjust = 1, size = 5, color = "red") +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "1 year") +  # updated to "01/2020" format
  labs(title = "Standardized Prices: META vs. GOOG (2012–2024)",
       y = "Standardized Price", color = "") +
  scale_color_manual(values = c("META" = "blue", "GOOG" = "darkgreen")) +
  theme_minimal(base_size = 14)

#dev.off()
