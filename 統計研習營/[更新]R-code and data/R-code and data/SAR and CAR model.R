rm(list=ls())

library(maps)
library(raster)
library(geoR)
library(sp)
library(sf)
library(terra)

nc <- st_read("C:/Users/User/OneDrive/桌面/統計研習營/[更新]R-code and data/R-code and data/data/gadm41_TWN_2.shp")

#---------------Rainfall---------------
#----- Import Data -----
# Set working directory
setwd("C:/Users/User/OneDrive/桌面/統計研習營/[更新]R-code and data/R-code and data/data")

# Read the rainfall data for 2021
dr_2021 <- read.csv("dr_2021.csv")  # 1471 x 368

#--- data preprocessing ---
dr_coor <- dr_2021[, c(1, 2)]  
dr_rain <- dr_2021[, -c(1, 2, 368)]

# Calculate the maximum daily rainfall for each station
ym_dr <- apply(dr_rain, 1, max)

# Remove missing values (-99.9 indicates missing or incorrect values)
invalid_index <- which(ym_dr == -99.9)
dr_coor <- dr_coor[-invalid_index, ]
dr_rain <- dr_rain[-invalid_index, ]
ym_dr <- ym_dr[-invalid_index]

dim(dr_coor)    # 1412 * 2
dim(dr_rain)    # 1412 * 365
length(ym_dr)   # 1412


# ---Histogram and Basic Statistics
par(mfrow=c(1, 1), mar=c(2, 2, 1, 2))
hist(ym_dr, breaks=20, main=paste('Year =', 2021), xlab='Max. Daily Rainfall')
summary(ym_dr)

#----- Plots -----
# ---Whole Taiwan---
par(mfrow=c(1, 1), mar=c(3, 3, 1, 2))
xyz <- cbind(dr_coor, ym_dr)
r1 <- rasterFromXYZ(xyz)
r <- mask(r1, nc[!nc$GID_2%in%c("TWN.1.1_1", "TWN.1.2_1", "TWN.7.10_1"),])
plot(r, col=terrain.colors(100), xlim = c(119.85, 122.25), ylim = c(21.75, 25.45))
plot(nc$geometry[!nc$GID_2%in%c("TWN.1.1_1", "TWN.1.2_1", "TWN.7.10_1")], add=T)

# ---Cities of Taiwan---
NL_NAME_use <- nc$NL_NAME_2[c(3:16, 18:22)]
NL_NAME_combine <- list(3, c(4, 7, 14), 5, 6, 8, c(9, 10), c(11, 12), 13, 15, 16, 18, 19, 20, 21, 22)

# -----Prepare for CAR and SAR model-----
xyz <- cbind(dr_coor, ym_dr)
aaa <- c()
r1 <- rasterFromXYZ(xyz)
for(i in 1:length(NL_NAME_combine)){
  r <- mask(r1, nc[nc$NL_NAME_2%in%nc$NL_NAME_2[unlist(NL_NAME_combine[i])],])
  df <- rasterToPoints(r)
  print(nc$NL_NAME_2[unlist(NL_NAME_combine[i])])
  aaa <- rbind(aaa, colMeans(df))
}
aaa <- as.data.frame(aaa)
colnames(aaa)[3] <- "rainfall"
data_merged <- aaa
data_merged <- rbind(data_merged[2,],
                     data_merged[10,],
                     data_merged[3,],
                     data_merged[4,],
                     data_merged[12,],
                     data_merged[6,],
                     data_merged[14,],
                     data_merged[11,],
                     data_merged[5,],
                     data_merged[7,],
                     data_merged[13,],
                     data_merged[8,],
                     data_merged[9,],
                     data_merged[15,],
                     data_merged[1,])

library(ggplot2)

# log rainfall plot
data_merged2 <- data_merged
data_merged2[,3] <- log(data_merged2[,3])
ggplot(data = nc) +
  geom_sf(color = "black", fill = "white") +
  coord_sf(xlim = c(119.85, 122.25), ylim = c(21.75, 25.45), expand = FALSE) +
  geom_point(data = data_merged2, aes(x = x, y = y, color = rainfall), size = 3, alpha=0.8) + 
  labs(x = "", y = "") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()   
  )

# ---------------find neighbors---------------
library(spdep)   
library(sf)       
library(RColorBrewer)
library(dplyr)

# load shapefile
shp <- nc
shp <- shp %>%
  filter(!NL_NAME_2 %in% c("澎湖縣", "馬祖列島", "金門縣")) %>%
  mutate(region_group = case_when(
    NL_NAME_2 %in% c("新竹市", "新竹縣") ~ "新竹地區",
    NL_NAME_2 %in% c("嘉義市", "嘉義縣") ~ "嘉義地區",
    NL_NAME_2 %in% c("台北市", "新北市", "基隆市") ~ "北北基",
    TRUE ~ NL_NAME_2 # other cities remain the same
  ))

# merge come cities (e.g. Taipei, New Taipei and Keelung)
shp_merged <- shp %>%
  group_by(region_group) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")
# Create adjacency
nb <- poly2nb(shp_merged, queen = TRUE)
shp_merged$neighbor_count <- sapply(nb, length)
# Calculate center point
centroids <- st_coordinates(st_centroid(shp_merged))

# Draw adjacent lines
par(mfrow=c(1, 1), mar=c(3, 3, 1, 2))
plot(st_geometry(shp_merged), border = "black", xlim = c(119.85, 122.25), ylim = c(21.75, 25.45))
plot(nb, coords = centroids, add = TRUE, col = "blue", pch=16)

# Convert to adjacency matrix (binary)
A <- nb2mat(nb, style = "B", zero.policy = TRUE)

# Create the M matrix
n <- nrow(A)
M <- diag(rowSums(A))

# Use ggplot to draw Number of neighbors
ggplot(shp_merged) +
  geom_sf(aes(fill = neighbor_count), color = "black") +
  coord_sf(xlim = c(119.85, 122.25), ylim = c(21.75, 25.45), expand = FALSE) +
  #labs(title = "Number of neighbors") +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()   
  )

#--------------- Temperature ---------------
#--- Import Data ---
setwd("C:/Users/User/OneDrive/桌面/統計研習營/[更新]R-code and data/R-code and data/data")
tpname <- paste("temp", 1:25, ".csv", sep = "")
tp <- c()
for (i in 1:25){tp[[i]] <- read.csv(tpname[i])}
gc()
dim(tp[[i]]) # 126 * 23

#--- Data Preprocessing ---
aa <- c(); bb <- c()
for(i in 1:25){aa[i] <- tp[[i]]$Lon[1]; bb[i] <- tp[[i]]$Lat[1]}
tp_coor2 <- data.frame(LON=aa, LAT=bb)
tp_coor2  # coordinates (25 * 2)

tp_jud <- c()
for(i in 1:25){
  tp_jud[[i]] <- tp[[i]][tp[[i]]$Time%in%seq(1960, 2021),]  # tp but only from 1960 to 2021
}
# if stations have any missing values between 1960 and 2021
for(i in 1:25){if(any(tp_jud[[i]][-23]==-99.9)){print(i)}}

tp_temp <- c()  # the annual average temp. of 20 stations between 1960 and 2021
tp_coor <- matrix(nrow=20, ncol=2)
ccc <- 1
for(i in 1:25){
  if(!(i %in% c(8, 10, 14, 18, 25, 6, 11, 22))){
    tp_temp[[ccc]] <- tp_jud[[i]]$Year  
    tp_coor[ccc,1] <- tp_coor2[i,1]
    tp_coor[ccc,2] <- tp_coor2[i,2]
    ccc <- ccc + 1}}
length(tp_temp)  # 20
length(tp_temp[[1]])  # 62

mean_temp <- c() # mean temperature of 20 stations (length = 62)
ff <- tp_temp[[1]]
for(i in 2:17){ff <- ff + tp_temp[[i]]}
mean_temp <- ff / 17

lowess_temp <- lowess(mean_temp)$y  # smoothed annual average temperature
adj_temp <- as.vector(scale(lowess_temp, center= TRUE, scale=TRUE)); adj_temp # [seq(1, 30)]
length(adj_temp)  # 62

rm(tp)

tp_data_2021 <- c()
tp_coor_2021 <- tp_coor[1:17,]
for(i in 1:17){
  tp_data_2021[i] <- tp_temp[[i]][62]
}
tp_data_2021 <- scale(tp_data_2021)
tp_2021 <- as.data.frame(cbind(tp_coor_2021,tp_data_2021))
#--- Plots ---
# stations
plot(tp_coor_2021, pch=16, col='blue', xlim=c(119.85, 122.05), ylim=c(21.95, 25.25), 
     xlab='Longitude', ylab='Latitude')
plot(nc$geometry[!nc$GID_2%in%c("TWN.1.1_1", "TWN.1.2_1", "TWN.7.10_1")], add=T)

# standardized temperature
ggplot(data = nc) +
  geom_sf(color = "black", fill = "white") +
  coord_sf(xlim = c(119.85, 122.25), ylim = c(21.75, 25.45), expand = FALSE) +
  geom_point(data = as.data.frame(tp_2021), aes(x = V1, y = V2, color = tp_data_2021), size = 3, alpha=0.8) + 
  #ggtitle("Thresholds") +
  #scale_color_viridis() + 
  scale_color_gradient(name = "Std. Temp") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()   
  )
#-----Use kriging on temperature data-----
library(lattice)
library(gstat)
library(geoR)
vario100 <- variog(data=tp_data_2021, coords=tp_coor_2021, max.dist=5)
ols <- variofit(vario100, ini=c(1,0.5), fix.nug=TRUE, wei="equal")
#summary(ols)
wls <- variofit(vario100, ini=c(1,0.5), fix.nug=TRUE)
#summary(wls)
ml <- likfit(data=tp_data_2021, coords=tp_coor_2021, ini.cov.pars = c(1, 0.5), fix.nug = TRUE)
#ml
#summary(ml)
reml <- likfit(data=tp_data_2021, coords=tp_coor_2021, ini.cov.pars = c(1, 0.5), fix.nug = TRUE, lik.met = "REML")
#summary(reml)

par(mfrow=c(1, 1))
plot(vario100, pch=16, main="Estimations of variogram model")
lines(ols, lwd=3, lty=2,col=2)
lines(wls,lwd=3,lty=3,col=3)
lines(ml, lwd=3,lty=4,col=4)
lines(reml, lwd=3,lty = 6,col=6)
legend("topleft",c("OLS","WLS","MLE","REML"),
       lty=c(2, 3, 4, 6),col=c(2, 3, 4, 6),lwd=2)

# use WLS?
tp_2021 <- as.data.frame(tp_2021)  
coordinates(tp_2021) <- ~ V1 + V2  
spdf <- dr_coor
coordinates(spdf) <- ~ LON + LAT

aaa <- variogram(tp_data_2021~1, tp_2021)
wls_fit <- fit.variogram(aaa, model=vgm('Exp'))
mle_krig <- krige(tp_data_2021~1, tp_2021, newdata=spdf, model=wls_fit)
mle_df <- as.data.frame(mle_krig)

#plot
par(mfrow=c(1, 1), mar=c(3, 3, 1, 2))
xyz <- cbind(dr_coor, mle_df$var1.pred)
r1 <- rasterFromXYZ(xyz)
r <- mask(r1, nc[!nc$GID_2%in%c("TWN.1.1_1", "TWN.1.2_1", "TWN.7.10_1"),])
my_blues_fun <- colorRampPalette(c("#253494", "#2c7fb8", "#41b6c4", "#a1dab4", "#ffffcc"))
my_blues <- my_blues_fun(100)
plot(r, main=paste(''), col=my_blues, xlim = c(119.85, 122.25), ylim = c(21.75, 25.45))
plot(nc$geometry[!nc$GID_2%in%c("TWN.1.1_1", "TWN.1.2_1", "TWN.7.10_1")], add=T)

# -----Prepare for CAR and SAR model-----
xyz <- cbind(dr_coor, mle_df$var1.pred)
aaa <- c()
r1 <- rasterFromXYZ(xyz)
for(i in 1:length(NL_NAME_combine)){
  r <- mask(r1, nc[nc$NL_NAME_2%in%nc$NL_NAME_2[unlist(NL_NAME_combine[i])],])
  df <- rasterToPoints(r)
  print(nc$NL_NAME_2[unlist(NL_NAME_combine[i])])
  aaa <- rbind(aaa, colMeans(df))
}
aaa <- as.data.frame(aaa)
colnames(aaa)[3] <- "temp"
tp_merged <- aaa
tp_merged <- rbind(tp_merged[2,],
                   tp_merged[10,],
                   tp_merged[3,],
                   tp_merged[4,],
                   tp_merged[12,],
                   tp_merged[6,],
                   tp_merged[14,],
                   tp_merged[11,],
                   tp_merged[5,],
                   tp_merged[7,],
                   tp_merged[13,],
                   tp_merged[8,],
                   tp_merged[9,],
                   tp_merged[15,],
                   tp_merged[1,])

# standardized temperature
library(ggplot2)
ggplot(data = nc) +
  geom_sf(color = "black", fill = "white") +
  coord_sf(xlim = c(119.85, 122.25), ylim = c(21.75, 25.45), expand = FALSE) +
  geom_point(data = tp_merged, aes(x = x, y = y, color = temp), size = 3, alpha=0.8) + 
  labs(x = "", y = "") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()   
  )

data_sf <- st_as_sf(tp_merged, coords = c("x", "y"), crs = 4326)
matched <- st_join(data_sf, shp_merged, join = st_within)
temp_summary <- matched %>%
  group_by(region_group) %>%
  summarise(temp = mean(temp, na.rm = TRUE))
temp_summary_df <- st_drop_geometry(temp_summary)
shp_with_temp <- left_join(shp_merged, temp_summary_df, by = "region_group")

# standardized temperature
ggplot(shp_with_temp) +
  geom_sf(aes(fill = temp), color = "black") +
  scale_fill_gradientn(
    colors = c("#253494", "#2c7fb8", "#41b6c4", "#a1dab4", "#ffffcc"),
    name = "Std. Temp."
  ) +
  #labs(title = "Temperature by Region") +
  coord_sf(xlim = c(119.85, 122.25), ylim = c(21.75, 25.45), expand = FALSE) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()   
  )
#---------------CAR model---------------
library(CARBayes)
# see https://www4.stat.ncsu.edu/~bjreich/st533/CAR
# and https://www4.stat.ncsu.edu/~bjreich/SpatialStats/code/CAR.html
X <- tp_merged$temp
Y <- log(data_merged$rainfall)
tick <- proc.time()[3]
model1  <- S.CARleroux(Y~X, 
                       family="gaussian", 
                       W=A, 
                       burnin=10000, 
                       n.sample=20000,
                       thin=10,
                       verbose=FALSE)
tock <- proc.time()[3]

(tock-tick)/60 # time in minutes
model1
samples <- model1$samples
phi_post_mean <- apply(samples$phi, 2, mean)

#---------------results of CAR model---------------
data_sf <- st_as_sf(data_merged2, coords = c("x", "y"), crs = 4326)
matched <- st_join(data_sf, shp_merged, join = st_within)
rain_summary <- matched %>%
  group_by(region_group) %>%
  summarise(rainfall = mean(rainfall, na.rm = TRUE))
rain_summary_df <- st_drop_geometry(rain_summary)
shp_with_rain <- left_join(shp_merged, rain_summary_df, by = "region_group")

#range(c(shp_with_rain$rainfall, shp_CAR$rainfall, shp_SAR$rainfall))
color_limits <- c(5.01, 5.76)

# p1: rainfall data
p1 <- ggplot(shp_with_rain) +
  geom_sf(aes(fill = rainfall), color = "black") +
  scale_fill_gradientn(
    colors = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
    limits = color_limits,
    name = "Log Rainfall"
  ) +
 # labs(title = "Rainfall by Region (Data)") +
  coord_sf(xlim = c(119.85, 122.25), ylim = c(21.75, 25.45), expand = FALSE) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()   
  )


# Draw the result of CAR
CAR_merged <- cbind(data_merged[,1:2], model1$fitted.values)
colnames(CAR_merged)[3] <- "rainfall"
CAR_sf <- st_as_sf(CAR_merged, coords = c("x", "y"), crs = 4326)
matched <- st_join(CAR_sf, shp_merged, join = st_within)
CAR_summary <- matched %>%
  group_by(region_group) %>%
  summarise(rainfall = mean(rainfall, na.rm = TRUE))
CAR_summary_df <- st_drop_geometry(CAR_summary)
shp_CAR <- left_join(shp_merged, CAR_summary_df, by = "region_group")

# p2: CAR model
p2 <- ggplot(shp_CAR) +
  geom_sf(aes(fill = rainfall), color = "black") +
  scale_fill_gradientn(
    colors = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
    limits = color_limits,
    name = "Log Rainfall"
  ) +
  #labs(title = "Rainfall by Region (CAR)") +
  coord_sf(xlim = c(119.85, 122.25), ylim = c(21.75, 25.45), expand = FALSE) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()   
  )

# trace plots
plot(samples$beta[,1], type = "l",
     main = '',
     #main = "Trace Plot: Intercept",
     xlab = "Iteration", ylab = "Value")
plot(samples$beta[,2], type = "l",
     main = '',
     #main = "Trace Plot: Temperature",
     xlab = "Iteration", ylab = "Value")
plot(samples$rho, type = "l",
     main = '',
     #main = "Trace Plot: Rho",
     xlab = "Iteration", ylab = "Value")
plot(samples$nu2, type = "l",
     main = '',
     #main = "Trace Plot: Spatial variance",
     xlab = "Iteration", ylab = "Value")
plot(samples$tau2, type = "l",
     main = '',
     #main = "Trace Plot: Residual variance",
     xlab = "Iteration", ylab = "Value")

#---------------SAR model---------------
library(spatialreg)   

df <- left_join(data_merged, tp_merged, by = c("x", "y"))
df$log_rainfall <- log(df$rainfall)
df
# Build coordinate matrix
coords <- as.matrix(df[, c("x", "y")])

# Build adjacency list
nb <- poly2nb(shp_merged, queen = TRUE)  # Queen adjacency

# Building a spatial weight matrix
listw <- nb2listw(nb, style = "W")
df$region_group <- shp_merged$region_group  # Make sure each column has a corresponding name

# fit the SAR model
sar_model <- lagsarlm(log_rainfall ~ temp, data = df, listw = listw, method = "eigen")

summary(sar_model)

df$fitted <- fitted(sar_model)

# Draw the result of SAR
SAR_merged <- cbind(data_merged[,1:2], df$fitted)
colnames(SAR_merged)[3] <- "rainfall"
SAR_sf <- st_as_sf(SAR_merged, coords = c("x", "y"), crs = 4326)
matched <- st_join(SAR_sf, shp_merged, join = st_within)
SAR_summary <- matched %>%
  group_by(region_group) %>%
  summarise(rainfall = mean(rainfall, na.rm = TRUE))
SAR_summary_df <- st_drop_geometry(SAR_summary)
shp_SAR <- left_join(shp_merged, SAR_summary_df, by = "region_group")

# p3: SAR model
p3 <- ggplot(shp_SAR) +
  geom_sf(aes(fill = rainfall), color = "black") +
  scale_fill_gradientn(
    colors = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494"),
    limits = color_limits,
    name = "Log Rainfall"
  ) +
  #labs(title = "Rainfall by Region (SAR)") +
  coord_sf(xlim = c(119.85, 122.25), ylim = c(21.75, 25.45), expand = FALSE) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()   
  )


library(patchwork)
p1 + p3
p1 + p2
p1 + p3 + p2
