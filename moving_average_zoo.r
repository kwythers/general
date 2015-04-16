# datafill example

require(xts)
require(TTR)
require(PerformanceAnalytics)
require(RColorBrewer)
require(mice)

dd <- read.table("~/Downloads/ts.txt", skip=1)
dd <- dd[,-1] # cut off the row numbers

colnames(dd) <- c("temp", "date", "time") # name the three remaining columns

dt <- as.POSIXct(paste(dd$date, dd$time)) # create a vector of datetimes (dt)
tempdata <- xts(dd$temp, order.by = dt) # create a time series ordered by dt
colnames(tempdata) <- "temp" # rename the column

tempdata$temp_missing <- tempdata$temp # create a new column for missing data

# create some missing values
tempdata$temp_missing[10,1] <- NA
tempdata$temp_missing[15:16,1] <- NA
tempdata$temp_missing[30:32,1] <- NA
tempdata$temp_missing[40:44,1] <- NA
tempdata$temp_missing[54:60,1] <- NA

##### first approach, use multiple imputation (mice) to fill missing data NEEDS TWO COLUMNS
# tempdata$mi <- mice(tempdata)

##### second approach 
# create a moving average function (mav) with different window widths
mav2 <- function(x,k=3){
  x <- c(rep(NA, k),x,rep(NA,k)) # add NA on both sides
  n <- length(x)
  return(sapply((k+1):(n-k), function(i) sum(x[(i-k):(i+k)],na.rm=TRUE)/(2*k+1-sum(is.na(x[(i-k):(i+k)])))))
}
mav3 <- function(x,k=3){
  x <- c(rep(NA, k),x,rep(NA,k)) # add NA on both sides
  n <- length(x)
  return(sapply((k+1):(n-k), function(i) sum(x[(i-k):(i+k)],na.rm=TRUE)/(2*k+1-sum(is.na(x[(i-k):(i+k)])))))
}
mav5 <- function(x,k=5){
  x <- c(rep(NA, k),x,rep(NA,k)) # add NA on both sides
  n <- length(x)
  return(sapply((k+1):(n-k), function(i) sum(x[(i-k):(i+k)],na.rm=TRUE)/(2*k+1-sum(is.na(x[(i-k):(i+k)])))))
}
mav10 <- function(x,k=10){
  x <- c(rep(NA, k),x,rep(NA,k)) # add NA on both sides
  n <- length(x)
  return(sapply((k+1):(n-k), function(i) sum(x[(i-k):(i+k)],na.rm=TRUE)/(2*k+1-sum(is.na(x[(i-k):(i+k)])))))
}
# apply mav functions to fill missing data in new columns
tempdata$temp_mav2 <- mav2(tempdata$temp_missing)
# tempdata$temp_mav3 <- mav3(tempdata$temp_missing)
# tempdata$temp_mav5 <- mav5(tempdata$temp_missing)
tempdata$temp_mav10 <- mav10(tempdata$temp_missing)

# third approach, use splines() to fill missing data
# tempdata$temp_spline <- NA # create new column for spline filling
miss <- which(is.na(tempdata$temp_missing)) # vector of missing data positions
a <- splinefun(tempdata$temp_missing) # create spline function (a) on temp_missing
tempdata$temp_spline <- tempdata$temp_missing # create new column for spline filling
tempdata$temp_spline[miss] <- a(miss) # fill the missing values with spline function (a)

# Set a color scheme: 
tsRainbow <- rainbow(ncol(tempdata)) 
# Plot the overlayed series 
plot(as.zoo(tempdata), ylab = "Temperature", xlab = "Time", main = "Ambient Temps",         
     col = tsRainbow, screens = 1) 
# Set a legend in the upper left hand corner to match color to return series 
legend(x = "topleft", legend = c("temp", "temp_missing", "temp_mav2", "temp_mav10", "temp_spline"), 
       lty = 1,col = tsRainbow)





