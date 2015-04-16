# load the data
require(WDI)
require(reshape2)
require(ggplot2)
require(scales)
require(useful)
require(forecast)
require(xts)
require(vars)
require(coefplot)

# pull the data
gdp <- WDI(country=c("US", "CA", "GB", "DE", "CN", "JP", "SG", "IL"), 
           indicator=c("NY.GDP.PCAP.CD", "NY.GDP.MKTP.CD"),
           start=1960, end=2011)

# give it good names
names(gdp) <- c("iso2c", "Country", "Year", "PerCapGDP", "GDP")
  
ggplot(gdp, aes(Year, PerCapGDP, color=Country, linetype=Country)) +
  geom_line() + 
  scale_y_continuous(label=dollar) + 
  theme_bw()

# absolute GDP
ggplot(gdp, aes(Year, GDP, color=Country, linetype=Country)) +
  geom_line() +
  scale_y_continuous(label=multiple_format(extra=dollar, multiple="M")) + 
  theme_bw()

# pull out US data

us <- gdp$PerCapGDP[gdp$Country == "United States"]

# convert to time series
us <- ts(us, start = min(gdp$Year), end = max(gdp$Year))

# quick look
plot(us, ylab = "Per Capita GDP", xlab = "YEAR")

# autocorrelations and parital autocorrelations
acf(us)
pacf(us)

# calculate optimal number of diffs
ndiffs(us)

# plot diffs
plot(diff(us, 2))

# get arima components
usBest <- auto.arima(x = us)

# do the residuals resemble white noise?
acf(usBest$residuals)
pacf(usBest$residuals)

# get the ARIMA coefficients
coef(usBest)

# predict 50 years into the future and include the standard error
predict(usBest, n.ahead = 50, se.fit = TRUE)

# visualize the predictions 50 years out
theForcast <- forecast(object = usBest, h = 10)

# plot forcast
plot(theForcast)

##### work with multiple time series

# cast data.frame to wide format
gdpCast <- dcast(Year ~ Country, 
                  data = gdp[, c("Country", "Year", "PerCapGDP")],
                  value.var = "PerCapGDP")

# convert to time series
gdpTS <- ts(data = gdpCast[, -1], start = min(gdpCast$Year), end = max(gdpCast$Year))

# build plot and legend using base graphics
plot(gdpTS, plot.type = "single", col = 1:8)
legend("topleft", legend=colnames(gdpTS), ncol = 2, lty = 1, col = 1:8, cex = 0.9)

# remove first ten years of data since Germany is missing for those years, for some reason
# this reverts back to matrix, loosing ts class
gdpTS1970_2010 <- gdpTS[which(rownames(gdpTS) > 10), ]

# remove Germany
gdpTS_noGermany <- gdpTS[, which(colnames(gdpTS) != "Germany")]
plot(gdpTS_noGermany, plot.type = "single", col = 1:8)
legend("topleft", legend=colnames(gdpTS_noGermany), ncol = 2, lty = 1, col = 1:8, cex = 0.9)

numDiffs <- ndiffs(gdpTS_noGermany)
gdpDiffed <- diff(gdpTS_noGermany, differences = numDiffs)
plot(gdpDiffed, plot.type = "single", col = 1:7)
legend("bottomleft", legend = colnames(gdpDiffed), ncol = 2, lty = 1, col = 1:7, cex = .9)

# fit separate regression (lm) model to each time series on the lags of itself
# and the other time series
gdpVar <- VAR(gdpDiffed, lag.max = 12)
# chosen order
gdpVar$p
# names of each of the models
names(gdpVar$varresult)
# each model is actually an lm object
class(gdpVar$varresult$Japan)
class(gdpVar$varresult$Canada)
# each model has its own coeficients
head(coef(gdpVar$varresult$Japan))
head(coef(gdpVar$varresult$Canada))
# look at the coeficients
coefplot(gdpVar$varresult$Japan)
coefplot(gdpVar$varresult$Canada)

# produce predictions
predict(gdpVar, n.ahead = 5)

##### GARCH

require(quantmod)
require(rugarch)
att <- getSymbols("T", auto.assign=FALSE)
head(att)
# plull out closing data
attClose <- att$T.Close
attSpec <- ugarchspec(variance.model = list(model = "sGARCH",
                                            garchOrder = c(1, 1)),
                      mean.model = list(armaOrder = c(1, 1)), 
                      distribution.model = "std")
# fit the model
attGarch <- ugarchfit(spec = attSpec, data = attClose)

# look at residuals
plot(attGarch@fit$residuals, typ ="l")
plot(attGarch, which = 10)

##### compare models with different mean specifications
# ARMA(1, 1)
attSpec1 <- ugarchspec(variance.model = list(model = "sGARCH",
                                             garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 1)), 
                       distribution.model = "std")
# ARMA(0, 0)
attSpec2 <- ugarchspec(variance.model = list(model = "sGARCH",
                                             garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(0, 0)), 
                       distribution.model = "std")
# ARMA(0, 2)
attSpec3 <- ugarchspec(variance.model = list(model = "sGARCH",
                                             garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(0, 2)), 
                       distribution.model = "std")
# ARMA(1, 2)
attSpec4 <- ugarchspec(variance.model = list(model = "sGARCH",
                                             garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 2)), 
                       distribution.model = "std")
# fit models
attGarch1 <- ugarchfit(spec = attSpec1, data = attClose)
attGarch2 <- ugarchfit(spec = attSpec2, data = attClose)
attGarch3 <- ugarchfit(spec = attSpec3, data = attClose)
attGarch4 <- ugarchfit(spec = attSpec4, data = attClose)

infocriteria(attGarch1)
infocriteria(attGarch2)
infocriteria(attGarch3)
infocriteria(attGarch4)

# make predictions
attPred <- ugarchboot(attGarch, n.ahead = 50, 
                      method = c("Partial", "Full") [1])
# plot the predictions
plot(attPred, which = 2)


