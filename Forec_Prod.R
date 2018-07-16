install.packages('xts')
install.packages('forecast')
library(dplyr)
library(xts)
library(forecast)

#Loading the data
bev <- read.csv('Beverage producer sales.csv')
glimpse(bev)

# Create the dates object as an index for our xts object
dates <- seq(as.Date("2014-01-19"), length = 176, by = "weeks")

# Create an xts object 
bev_xts <- xts(bev, order.by = dates)
head(bev_xts)

# Create the individual region sales as their own objects
MET_hi <- bev_xts[,"MET.hi"]
MET_lo <- bev_xts[,"MET.lo"]
MET_sp <- bev_xts[,"MET.sp"]

# Sum the region sales together
MET_t <- MET_hi + MET_lo + MET_sp

# Plot the metropolitan region total sales
plot(MET_t)

# Split the data into training and validation
MET_t_train <- MET_t[index(MET_t) < "2017-01-01"]
MET_t_valid <- MET_t[index(MET_t) >= "2017-01-01"]

# Use auto.arima() function for metropolitan sales
(MET_t_model <- auto.arima(MET_t_train))

# Forecast the first 22 weeks of 2017
forecast_MET_t <- forecast(MET_t_model, h = 22)

# Plot this forecast
plot(forecast_MET_t)

####Visualizing Forecast####

# Convert forecast to an xts object
for_dates <- seq(as.Date("2017-01-01"), length = 22, by = "weeks")
for_MET_t_xts <- xts(forecast_MET_t$mean, order.by = for_dates)

# Plot the validation data set
plot(MET_t_valid, main = 'Forecast Comparison', ylim = c(4000, 8500))

# Overlay the forecast of 2017
lines(for_MET_t_xts, col = "blue")

# Convert the forecast and validation data set to numeric values
for_MET_t <- as.numeric(forecast_MET_t$mean)
v_MET_t <- as.numeric(MET_t_valid)

# Calculate the MAE
(MAE <- mean(abs(for_MET_t - v_MET_t)))

# Calculate the MAPE
(MAPE <- 100*mean(abs((for_MET_t - v_MET_t)/v_MET_t)))


####Confidence Intervals for Forecast####

# Convert the limits to xts objects
lower <- xts(forecast_MET_t$lower[,2], order.by = for_dates)
upper <- xts(forecast_MET_t$upper[,2], order.by = for_dates)

# Adding confidence intervals of forecast to plot
lines(lower, col = "blue", lty = "dashed")
lines(upper, col = "blue", lty = "dashed")

# Calculating price elasticity
bev_xts_train <- bev_xts[index(MET_t) < "2017-01-01"]
bev_xts_valid <- bev_xts[index(MET_t) >= "2017-01-01"]


# Extract the sales and prices for the high end product and save them as a vector
MET_hi <- as.vector(bev_xts_train[,"MET.hi"])
MET_hi_p <- as.vector(bev_xts_train[,"MET.hi.p"])

# Save as a data frame after taking the log of sales and prices
MET_hi_train <- data.frame(log(MET_hi),log(MET_hi_p))
colnames(MET_hi_train) <- c("log_sales", "log_price")

# Calculate the regression
model_MET_hi <- lm(log_sales ~ log_price, data = MET_hi_train)
model_MET_hi


# Create date indices for New Year's week
n.dates <- as.Date(c("2014-12-28", "2015-12-27", "2016-12-25"))

# Create xts objects for New Year's
newyear <- as.xts(rep(1, 3), order.by = n.dates)


# Create sequence of dates for merging
dates_train <- seq(as.Date("2014-01-19"), length = 154, by = "weeks")

# Merge training dates into New Year's object
newyear <- merge(newyear, dates_train, fill = 0)
head(newyear, n = 53)

# Regression for holiday / promotional effects
# Add newyear variable to your data frame
MET_hi_train <- data.frame(MET_hi_train, as.vector(newyear))

# Build regressions for the product
model_MET_hi_full <- lm(log_sales ~ log_price + newyear, data = MET_hi_train)

summary(model_MET_hi_full)

# Subset the validation prices
l_MET_hi_p_valid <- as.vector(log(bev_xts_valid[,"MET.hi.p"]))

# Create a validation data frame
MET_hi_valid <- data.frame(l_MET_hi_p_valid)
colnames(MET_hi_valid) <- "log_price"
head(MET_hi_valid)

# Predict the log of sales for your high end product
pred_MET_hi <- predict(model_MET_hi, MET_hi_valid)

# Convert predictions out of log scale
pred_MET_hi <- exp(pred_MET_hi)

# Convert to an xts object
dates_valid <- seq(as.Date("2017-01-01"), length = 22, by = "weeks")
pred_MET_hi_xts <- xts(pred_MET_hi, order.by = dates_valid)

# Plot the forecast
plot(pred_MET_hi_xts)

# Calculate and print the MAPE
MET_hi_v <- bev_xts_valid[,"MET.hi"]
(MAPE <- 100*mean(abs((pred_MET_hi_xts - MET_hi_v)/MET_hi_v)))

# Calculate the residuals from the model
MET_hi_full_res <- residuals(model_MET_hi_full)

# Convert the residuals to an xts object
MET_hi_full_res <- xts(MET_hi_full_res, order.by = dates_train)

# Plot the histogram of the residuals
hist(MET_hi_full_res)

# Plot the residuals over time
plot(MET_hi_full_res)


# Build an ARIMA model on the residuals: MET_hi_arima
MET_hi_arima <- auto.arima(MET_hi_full_res)

# Look at a summary of the model 
summary(MET_hi_arima)

# Forecast 22 weeks with your model: for_MET_hi_arima
for_MET_hi_arima <- forecast(MET_hi_arima, h = 22)

# Print first 10 observations 
head(for_MET_hi_arima, n = 10)


# Visualizing residual forecasts

# Convert your forecasts into an xts object
dates_valid <- seq(as.Date("2017-01-01"), length = 22, by = "weeks")
for_MET_hi_arima <- xts(for_MET_hi_arima$mean, order.by = dates_valid)

# Plot the forecast
plot(for_MET_hi_arima)



# Convert your residual forecast to the exponential version
for_MET_hi_arima <- exp(for_MET_hi_arima)

# Multiply your forecasts together
for_MET_hi_final <- pred_MET_hi_xts * for_MET_hi_arima

# Plot the final forecast
plot(for_MET_hi_final, ylim = c(1000, 4300))

# Overlay the validation data set
lines(MET_hi_v, col = "blue")

#Calculating transfer function MAPE and MAE
# Calculate the MAE
MAE <- mean(abs(for_MET_hi_final - MET_hi_v))

# Calculate the MAPE
(MAPE <- 100*mean(abs((for_MET_hi_final - MET_hi_v)/MET_hi_v)))

# Build an ARIMA model using the auto.arima function
MET_hi_model_arima <- auto.arima(MET_hi)

# Forecast the ARIMA model
for_MET_hi <- forecast(MET_hi_model_arima, h = 22)

# Save the forecast as an xts object
dates_valid <- seq(as.Date("2017-01-01"), length = 22, by = "weeks")
for_MET_hi_xts <- xts(for_MET_hi$mean, order.by = dates_valid)

# Calculate the MAPE of the forecast
(MAPE <- 100*mean(abs((for_MET_hi_xts - MET_hi_v)/MET_hi_v)))

# Ensembling of Forecasts
# Ensemble the two forecasts together
for_MET_hi_en <- (for_MET_hi_xts + pred_MET_hi_xts)/2

# Calculate the MAE and MAPE

(MAE <- mean(abs(for_MET_hi_en - MET_hi_v)))
(MAPE <- 100*mean(abs((for_MET_hi_en - MET_hi_v)/MET_hi_v)))

##

