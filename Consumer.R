#### West_ConsumerSales

df <- Sales.Summary.Monthly
Consumer_West <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="West")
                        &(Sales.Summary.Monthly$Segment=="Consumer"))
Consumer_West <- Consumer_West[order(Consumer_West$Month),]
Consumer_West$MonthNum <-c (1:nrow(Consumer_West))
West_Consumer_Sales <- Consumer_West[,c("MonthNum","TotalMonthlySales")]
West_Consumer_Sales <- rbind(West_Consumer_Sales, West_Consumer_Sales[37:48,])
West_Consumer_Sales <- log(West_Consumer_Sales)
West_Consumer_ts <- ts(West_Consumer_Sales[,2], start = c(2014,1), frequency = 12)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "West Region (Consumer products)", main = "Plot of West Region Sales in Consumer Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2016,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2017,1))
plot(test)
library(forecast)
arima1 <- auto.arima(West_Consumer_ts, trace = TRUE, ic = "bic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(West_Consumer_ts, arima1, xlim = c(2014,2018), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)

plot(arima1$x, col = "red")
lines(fitted(arima1),col="blue")



arima1.forecast <- data.frame(arima1.forecast)
d <- data.frame(arima1.forecast)
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)
data <- data.frame(data$names, data$Point.Forecast)
library(ggplot2)
p <- ggplot(data, aes(x = data.names, y = data.Point.Forecast )) + geom_line(aes(group=0), colour="#000099") +  # Blue lines
  geom_point(size=3, colour="#CC0000")
p <- p + geom_text(aes(label = round(data.Point.Forecast)), size =4, hjust = 0.5, vjust = 3)
p <- p + xlab("Month")+ylab("USD")+ggtitle("Forecast for the next 2 years")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

plot.ts(West_Consumer_ts)
model <- HoltWinters(West_Consumer_ts)
plot(model, main = "Plot")



######
#############################################
#############################################

Consumer_East <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="East")
                        &(Sales.Summary.Monthly$Segment=="Consumer"))
Consumer_East <- Consumer_East[order(Consumer_East$Month),]
Consumer_East$MonthNum <-c (1:nrow(Consumer_East))
East_Consumer_Sales <- Consumer_East[,c("MonthNum","TotalMonthlySales")]
East_Consumer_Sales <- rbind(East_Consumer_Sales, East_Consumer_Sales[37:48,])
East_Consumer_ts <- ts(East_Consumer_Sales[,2], start = c(2014,1), frequency = 12)
West_Consumer_ts <- (East_Consumer_ts)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "East Region (Consumer products)", main = "Plot of East Region Sales in Consumer Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(West_Consumer_ts, trace = TRUE, ic = "aic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)

arima1.forecast <- data.frame(arima1.forecast)
d <- data.frame(arima1.forecast)
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)
data <- data.frame(data$names, data$Point.Forecast)
library(ggplot2)
p <- ggplot(data, aes(x = data.names, y = data.Point.Forecast )) + geom_line(aes(group=0), colour="#000099") +  # Blue lines
  geom_point(size=3, colour="#CC0000")
p <- p + geom_text(aes(label = round(data.Point.Forecast)), size =4, hjust = 0.5, vjust = 3)
p <- p + xlab("Month")+ylab("USD")+ggtitle("Forecast for the next 2 years")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p








############################################################################
##################################################################
############################################################


Consumer_South <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="South")
                         &(Sales.Summary.Monthly$Segment=="Consumer"))
Consumer_South <- Consumer_South[order(Consumer_South$Month),]
Consumer_South$MonthNum <-c (1:nrow(Consumer_South))
South_Consumer_Sales <- Consumer_South[,c("MonthNum","TotalMonthlySales")]
South_Consumer_Sales <- rbind(South_Consumer_Sales, South_Consumer_Sales[37:48,])
South_Consumer_Sales <- diff(South_Consumer_Sales, d = 5)
South_Consumer_ts <- ts(South_Consumer_Sales[,2], start = c(2014,1), frequency = 12)
West_Consumer_ts <- diff(South_Consumer_ts, d = 5)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "South Region (Consumer products)", main = "Plot of South Region Sales in Consumer Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(West_Consumer_ts, trace = TRUE, ic = "bic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)

arima1.forecast <- data.frame(arima1.forecast)
d <- data.frame(arima1.forecast)
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)
data <- data.frame(data$names, data$Point.Forecast)
library(ggplot2)
p <- ggplot(data, aes(x = data.names, y = data.Point.Forecast )) + geom_line(aes(group=0), colour="#000099") +  # Blue lines
  geom_point(size=3, colour="#CC0000")
p <- p + geom_text(aes(label = round(data.Point.Forecast)), size =4, hjust = 0.5, vjust = 3)
p <- p + xlab("Month")+ylab("USD")+ggtitle("Forecast for the next 2 years")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p





####################################################################################
###################################################################################
#######################################################################################

Consumer_Central <-subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="Central")
                          &(Sales.Summary.Monthly$Segment=="Consumer")) 
Consumer_Central <- Consumer_Central[order(Consumer_Central$Month),]
Consumer_Central$MonthNum <-c (1:nrow(Consumer_Central))
Consumer_Central_Sales <- Consumer_Central[,c("MonthNum","TotalMonthlySales")]
Consumer_Central_Sales <- rbind(Consumer_Central_Sales, Consumer_Central_Sales[37:48,])
Consumer_Central_Sales <- log(Consumer_Central_Sales)
Central_Consumer_ts <- ts(Consumer_Central_Sales[,2], start = c(2014,1), frequency = 12)
West_Consumer_ts <- Central_Consumer_ts
autoplot(West_Consumer_ts, xlab = "Years", ylab = "Central Region (Consumer products)", main = "Plot of Central Region Sales in Consumer Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(train, trace = TRUE, ic = "aic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)

arima1.forecast <- data.frame(arima1.forecast)
d <- data.frame(arima1.forecast)
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)
data <- data.frame(data$names, data$Point.Forecast)
library(ggplot2)
p <- ggplot(data, aes(x = data.names, y = data.Point.Forecast )) + geom_line(aes(group=0), colour="#000099") +  # Blue lines
  geom_point(size=3, colour="#CC0000")
p <- p + geom_text(aes(label = round(data.Point.Forecast)), size =4, hjust = 0.5, vjust = 3)
p <- p + xlab("Month")+ylab("USD")+ggtitle("Forecast for the next 2 years")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p


#### West_ConsumerSales

df <- Sales.Summary.Monthly
Consumer_West <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="West")
                        &(Sales.Summary.Monthly$Segment=="Corporate"))
Consumer_West <- Consumer_West[order(Consumer_West$Month),]
Consumer_West$MonthNum <-c (1:nrow(Consumer_West))
West_Consumer_Sales <- Consumer_West[,c("MonthNum","TotalMonthlySales")]
West_Consumer_Sales <- rbind(West_Consumer_Sales, West_Consumer_Sales[37:48,])
West_Consumer_Sales <- log(West_Consumer_Sales)
West_Consumer_ts <- ts(West_Consumer_Sales[,2], start = c(2014,1), frequency = 12)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "West Region (Corporate products)", main = "Plot of West Region Sales in Corporate Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2016,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2017,1))
plot(test)
library(forecast)
arima1 <- auto.arima(West_Consumer_ts, trace = TRUE, ic = "bic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)

arima1.forecast <- data.frame(arima1.forecast)
d <- data.frame(arima1.forecast)
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)
data <- data.frame(data$names, data$Point.Forecast)
library(ggplot2)
p <- ggplot(data, aes(x = data.names, y = data.Point.Forecast )) + geom_line(aes(group=0), colour="#000099") +  # Blue lines
  geom_point(size=3, colour="#CC0000")
p <- p + geom_text(aes(label = round(data.Point.Forecast)), size =4, hjust = 0.5, vjust = 3)
p <- p + xlab("Month")+ylab("USD")+ggtitle("Forecast for the next 2 years")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

################################################
#############################################
#############################################

Consumer_East <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="East")
                        &(Sales.Summary.Monthly$Segment=="Corporate"))
Consumer_East <- Consumer_East[order(Consumer_East$Month),]
Consumer_East$MonthNum <-c (1:nrow(Consumer_East))
East_Consumer_Sales <- Consumer_East[,c("MonthNum","TotalMonthlySales")]
East_Consumer_Sales <- rbind(East_Consumer_Sales, East_Consumer_Sales[37:48,])
East_Consumer_ts <- ts(East_Consumer_Sales[,2], start = c(2014,1), frequency = 12)
West_Consumer_ts <- log(East_Consumer_ts)
West_Consumer_ts <- diff(West_Consumer_ts, d = 3)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "East Region (Corporate products)", main = "Plot of East Region Sales in Corporate Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(West_Consumer_ts, trace = TRUE, ic = "aic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)



arima1.forecast <- data.frame(arima1.forecast)
d <- data.frame(arima1.forecast)
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)
data <- data.frame(data$names, data$Point.Forecast)
library(ggplot2)
p <- ggplot(data, aes(x = data.names, y = data.Point.Forecast )) + geom_line(aes(group=0), colour="#000099") +  # Blue lines
  geom_point(size=3, colour="#CC0000")
p <- p + geom_text(aes(label = round(data.Point.Forecast)), size =4, hjust = 0.5, vjust = 3)
p <- p + xlab("Month")+ylab("USD")+ggtitle("Forecast for the next 2 years")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

############################################################################
##################################################################
############################################################


Consumer_South <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="South")
                         &(Sales.Summary.Monthly$Segment=="Corporate"))
Consumer_South <- Consumer_South[order(Consumer_South$Month),]
Consumer_South$MonthNum <-c (1:nrow(Consumer_South))
South_Consumer_Sales <- Consumer_South[,c("MonthNum","TotalMonthlySales")]
South_Consumer_Sales <- rbind(South_Consumer_Sales, South_Consumer_Sales[37:48,])
South_Consumer_Sales <- log(South_Consumer_Sales)
South_Consumer_ts <- ts(South_Consumer_Sales[,2], start = c(2014,1), frequency = 12)
West_Consumer_ts <- diff(South_Consumer_ts, d=5)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "South Region (Corporate products)", main = "Plot of South Region Sales in Corporate Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(West_Consumer_ts, trace = TRUE, ic = "aic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)

arima1.forecast <- data.frame(arima1.forecast)
d <- data.frame(arima1.forecast)
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)
data <- data.frame(data$names, data$Point.Forecast)
library(ggplot2)
p <- ggplot(data, aes(x = data.names, y = data.Point.Forecast )) + geom_line(aes(group=0), colour="#000099") +  # Blue lines
  geom_point(size=3, colour="#CC0000")
p <- p + geom_text(aes(label = round(data.Point.Forecast)), size =4, hjust = 0.5, vjust = 3)
p <- p + xlab("Month")+ylab("USD")+ggtitle("Forecast for the next 2 years")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p



####################################################################################
###################################################################################
#######################################################################################

Consumer_Central <-subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="Central")
                          &(Sales.Summary.Monthly$Segment=="Corporate")) 
Consumer_Central <- Consumer_Central[order(Consumer_Central$Month),]
Consumer_Central$MonthNum <-c (1:nrow(Consumer_Central))
Consumer_Central_Sales <- Consumer_Central[,c("MonthNum","TotalMonthlySales")]
Consumer_Central_Sales <- rbind(Consumer_Central_Sales, Consumer_Central_Sales[37:48,])
Consumer_Central_Sales <- log(Consumer_Central_Sales)
Central_Consumer_ts <- ts(Consumer_Central_Sales[,2], start = c(2014,1), frequency = 12)
West_Consumer_ts <- diff(Central_Consumer_ts, d = 3)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "Central Region (Corporate products)", main = "Plot of Corporate Region Sales in Consumer Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1), end = c(2018,7))
plot(test)
library(forecast)
arima1 <- auto.arima(West_Consumer_ts, trace = TRUE, ic = "bic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)


arima1.forecast <- data.frame(arima1.forecast)
d <- data.frame(arima1.forecast)
names <- rownames(d)
rownames(d) <- NULL
data <- cbind(names,d)
data <- data.frame(data$names, data$Point.Forecast)
library(ggplot2)
p <- ggplot(data, aes(x = data.names, y = data.Point.Forecast )) + geom_line(aes(group=0), colour="#000099") +  # Blue lines
  geom_point(size=3, colour="#CC0000")
p <- p + geom_text(aes(label = round(data.Point.Forecast)), size =4, hjust = 0.5, vjust = 3)
p <- p + xlab("Month")+ylab("USD")+ggtitle("Forecast for the next 2 years")
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

#### West_ConsumerSales

df <- Sales.Summary.Monthly
Consumer_West <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="West")
                        &(Sales.Summary.Monthly$Segment=="Home Office"))
Consumer_West <- Consumer_West[order(Consumer_West$Month),]
Consumer_West$MonthNum <-c (1:nrow(Consumer_West))
West_Consumer_Sales <- Consumer_West[,c("MonthNum","TotalMonthlySales")]
West_Consumer_Sales <- rbind(West_Consumer_Sales, West_Consumer_Sales[35:48,])
West_Consumer_Sales <- (West_Consumer_Sales)

West_Consumer_ts <- ts(West_Consumer_Sales[,2], start = c(2014,1), end = c(2018,10),frequency = 12)

West_Consumer_ts <- diff(West_Consumer_ts, d = 5)

autoplot(West_Consumer_ts, xlab = "Years", ylab = "West Region (Home Office products)", main = "Plot of West Region Sales in Home Office Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(train, trace = TRUE, ic = "bic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)


################################################
#############################################
#############################################

Consumer_East <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="East")
                        &(Sales.Summary.Monthly$Segment=="Home Office"))
Consumer_East <- Consumer_East[order(Consumer_East$Month),]
Consumer_East$MonthNum <-c (1:nrow(Consumer_East))
East_Consumer_Sales <- Consumer_East[,c("MonthNum","TotalMonthlySales")]
East_Consumer_Sales <- rbind(East_Consumer_Sales, East_Consumer_Sales[37:44,])
East_Consumer_ts <- ts(East_Consumer_Sales[,2], start = c(2014,1),end = c(2018,8), frequency = 12)
West_Consumer_ts <- East_Consumer_ts
West_Consumer_ts <- log(West_Consumer_ts)
West_Consumer_ts <- (West_Consumer_ts)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "East Region (Home Office products)", main = "Plot of East Region Sales in Home Office Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(train, trace = TRUE, ic = "aic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)


############################################################################
##################################################################
############################################################


Consumer_South <- subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="South")
                         &(Sales.Summary.Monthly$Segment=="Home Office"))
Consumer_South <- Consumer_South[order(Consumer_South$Month),]
Consumer_South$MonthNum <-c (1:nrow(Consumer_South))
South_Consumer_Sales <- Consumer_South[,c("MonthNum","TotalMonthlySales")]
South_Consumer_Sales <- rbind(South_Consumer_Sales, South_Consumer_Sales[37:48,])
#South_Consumer_Sales <- log(South_Consumer_Sales)
South_Consumer_ts <- ts(South_Consumer_Sales[,2], start = c(2014,1), frequency = 12)
West_Consumer_ts <- diff(South_Consumer_ts, d=5)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "South Region (Home Office products)", main = "Plot of South Region Sales in Home Office Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(train, trace = TRUE, ic = "bic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)


####################################################################################
###################################################################################
#######################################################################################

Consumer_Central <-subset(Sales.Summary.Monthly,(Sales.Summary.Monthly$Region=="Central")
                          &(Sales.Summary.Monthly$Segment=="Home Office")) 
Consumer_Central <- Consumer_Central[order(Consumer_Central$Month),]
Consumer_Central$MonthNum <-c (1:nrow(Consumer_Central))
Consumer_Central_Sales <- Consumer_Central[,c("MonthNum","TotalMonthlySales")]
Consumer_Central_Sales <- rbind(Consumer_Central_Sales, Consumer_Central_Sales[37:47,])
Consumer_Central_Sales <- log(Consumer_Central_Sales)
Central_Consumer_ts <- ts(Consumer_Central_Sales[,2], start = c(2014,1), frequency = 12)
West_Consumer_ts <- diff(Central_Consumer_ts, d = 5)
autoplot(West_Consumer_ts, xlab = "Years", ylab = "Central Region (Home Office products)", main = "Plot of Central Region Sales in Home Office Segment")
ggseasonplot(West_Consumer_ts, main = "Seasonal Plot of Monthly Sales",
             ylab = "US Dollars")
train <- window(West_Consumer_ts, start = c(2014,1),
                end = c(2017,12))
plot(train)
test <- window(West_Consumer_ts, start = c(2018,1))
plot(test)
library(forecast)
arima1 <- auto.arima(train, trace = TRUE, ic = "bic")
summary(arima1)
confint(arima1)
plot.ts(arima1$residuals, main = "Residuals")
Box.test(arima1$residuals, lag=20, type = "Ljung-Box")
acf(arima1$residuals, lag.max = 24, main = "ACF of the Model")
Box.test(arima1$residuals^2, lag = 20, type = "Ljung-Box")
library(tseries)
jarque.bera.test(arima1$residuals)
arima1.forecast <- forecast(arima1, h = 24)
plot(arima1.forecast, main = "Forecast")
library(TSPred)
plotarimapred(test, arima1, xlim = c(2014,2019), range.percent = 0.05, main = "Comparison with existing values")
accuracy(arima1.forecast, test)


