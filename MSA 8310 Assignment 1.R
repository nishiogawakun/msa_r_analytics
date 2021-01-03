library(prophet)
library(Amelia)
library(lubridate)
library(zoo)
library(ggplot2)

pm_data <- read.csv("2016-PM2.5.csv",col.names = c('ds', 'y'))
pm_data$ds <- as.POSIXct(pm_data$ds,format= "%m/%d/%Y")

##pm_data <- na.omit(pm_data)
ggplot(pm_data, aes(x = ds, y = y)) + geom_line() + theme_minimal()


roll_mean <- rollmean(pm_data$y, k = 7)
fill <- rep(NA,6)
roll_mean <- append(fill, roll_mean)
pm_data$roll_mean <- roll_mean
ggplot(pm_data, aes(x = ds, y = roll_mean)) + geom_line() + theme_minimal()

##lam = BoxCox.lambda(df$value, method = "loglik")

model <- prophet(pm_data, changepoint.prior.scale = 0.001, yearly.seasonality = T,weekly.seasonality = T)

future <- make_future_dataframe(model, periods = 365, freq = 'day')


# future <- pm_data$ds %m+% years(1)
# future <- as.data.frame(future)
# colnames(future) <- 'ds'
# future$ds[1466] <- pm_data$ds[1465] %m+% hours(1)

  
forecast <- predict(model, future)

plot(model, forecast)
prophet_plot_components(model, forecast)
