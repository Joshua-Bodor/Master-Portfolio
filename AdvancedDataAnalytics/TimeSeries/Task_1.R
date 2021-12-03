#C
#1.	Provide a line graph visualizing the realization of the time series.
data <- read.csv('medical_time_series.csv')
ts_data <- ts(data[, 2])

ts.plot(ts_data)


#2.	Describe the time step formatting of the realization, 
#including any gaps in measurement and the length of the sequence.
data$Day


#3.	Evaluate the stationarity of the time series.
library(tseries)
adf.test(ts_data)
#nonstationary b/c p-val > 0.05


#4.	Explain the steps used to prepare the data for analysis, 
#including the training and test set split.
ts_data.train <- ts_data[1:580]
ts_data.test <- data[580:731, 2]
ts_data.test <- ts(ts_data.test, start=580)

diff_ts <- diff(ts_data)
ts.plot(diff_ts)
adf.test(diff_ts)

#D
#1.	Report the annotated findings with visualizations of your data analysis, 
#including the following elements:
#a.	the presence or lack of a seasonal component
ts_data_months <- ts(data[, 2], start=1, frequency=30)
decomposed_ts <- decompose(ts_data_months)
plot(decomposed_ts$seasonal)

#b.	trends
plot(decomposed_ts$trend)

#c.	auto correlation function
acf(ts_data, lag.max=365)
pacf(ts_data, lag.max=10)
#https://towardsdatascience.com/significance-of-acf-and-pacf-plots-in-time-series-analysis-2fa11a5d10a8
acf(diff_ts, lag.max=10)
pacf(diff_ts, lag.max = 10)


#d.	spectral density
spectrum(diff_ts)


#e.	the decomposed time series
plot(decomposed_ts)

#f.	confirmation of the lack of trends in the residuals of the decomposed series
res <- ts(na.omit(decomposed_ts$random))
adf.test(res)

#2.	Identify an autoregressive integrated moving average (ARIMA) model that 
#takes into account the observed trend and seasonality of the time series data.
library(forecast)

a <- c()
b <- c()

i <- 1
for (p in 0:2) {
  for (d in 0:1) {
    for (q in 0:4) {
      model <- arima(diff_ts, order=c(p,d,q))
      a[i] <- AIC(model)
      b[i] <- BIC(model)
      print(paste('model', as.character(i), ' -- AIC: ', round(a[i], 2), 
                  ' BIC: ', round(b[i], 2), ' Params: ', p, d, q, sep=''))
      i <- i+1
    }
  }
}

min(a)
min(b)




#3.	Perform a forecast using the derived ARIMA model.
fin_model <- arima(ts_data.train, order = c(1,0,2))

pred <- predict(fin_model, n.ahead=150)

upper_bound <- pred$pred+2*pred$se
lower_bound <- pred$pred-2*pred$se
predictions <- pred$pred

plot.ts(ts_data, main='Forecast of Revenue', ylab='Revenue (Millions of Dollars)', xlab='Time (Days)')
lines(predictions, col='blue')
lines(upper_bound, col='red')
lines(lower_bound, col='red')
lines(ts_data.test, col='purple')
legend('topleft', 
       legend=c('Training Data', 'Predictions', 'Error Boundaries', 'Test Data'),
       col=c('black', 'blue', 'red', 'purple'),
       pch=15
       )





mean((ts_data.test - pred$pred)^2)

ts.test.monthly <- ts(data[580:731, 2], start=580, frequency=30)
decomposed.test <- decompose(ts.test.monthly)
pred.monthly <- ts(pred$pred, start=580, frequency=30)
mean((na.omit(decomposed.test$trend) - na.omit(pred.monthly))^2)

