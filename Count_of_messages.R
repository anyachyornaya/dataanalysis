rm(list = ls())

library(data.table)
library(forecast)
library(xts)

mess <- read.csv("C:/Users/malofeev/Desktop/Count_of_messages.csv", sep = ";")

mess$Depth <- as.numeric(gsub(",", ".", mess$Depth))
mess$WomanRate <- as.numeric(gsub(",", ".", mess$WomanRate))
mess$Date <- as.Date(mess$Date, format = "%d.%m.%Y")

ts_data <- subset(mess, select = colnames(mess)[1:6])
# ts_data <- ts(ts_data, start = mess$Date[1])
ts1 <- mess$VL
ts2 <- mess$Persons
ts3 <- mess$Visits
ts4 <- mess$Depth
ts5 <- mess$Views
ts6 <- mess$WomanRate

plot(ts1)
plot(ts2)
plot(ts3)
plot(ts4)
plot(ts5)
plot(ts6)

boxplot(ts1, main="VL", boxwex=0.1)
boxplot(ts2, main="Persons", boxwex=0.1)
boxplot(ts3, main="Visits", boxwex=0.1)
boxplot(ts4, main="Depth", boxwex=0.1)
boxplot(ts5, main="Views", boxwex=0.1)
boxplot(ts6, main="WomanRate", boxwex=0.1)

outliers_vl <- boxplot.stats(ts1)$out
outliers_1_bp <- which(ts1 %in% outliers_vl)
rm(outliers_vl)
outliers_vl <- boxplot.stats(ts2)$out
outliers_2_bp <- which(ts2 %in% outliers_vl)
rm(outliers_vl)
outliers_vl <- boxplot.stats(ts3)$out
outliers_3_bp <- which(ts3 %in% outliers_vl)
rm(outliers_vl)
outliers_vl <- boxplot.stats(ts4)$out
outliers_4_bp <- which(ts4 %in% outliers_vl)
rm(outliers_vl)
outliers_vl <- boxplot.stats(ts5)$out
outliers_5_bp <- which(ts5 %in% outliers_vl)
rm(outliers_vl)
outliers_vl <- boxplot.stats(ts6)$out
outliers_6_bp <- which(ts6 %in% outliers_vl)
rm(outliers_vl)

mean_1 <- mean(ts1)
mean_2 <- mean(ts2)
mean_3 <- mean(ts3)
mean_4 <- mean(ts4)
mean_5 <- mean(ts5)
mean_6 <- mean(ts6)

median_1 <- median(ts1)
median_2 <- median(ts2)
median_3 <- median(ts3)
median_4 <- median(ts4)
median_5 <- median(ts5)
median_6 <- median(ts6)

var_1 <- var(ts1)
var_2 <- var(ts2)
var_3 <- var(ts3)
var_4 <- var(ts4)
var_5 <- var(ts5)
var_6 <- var(ts6)

std_1 <- sd(ts1)
std_2 <- sd(ts2)
std_3 <- sd(ts3)
std_4 <- sd(ts4)
std_5 <- sd(ts5)
std_6 <- sd(ts6)

up_line_1 <- mean_1+2*std_1
up_line_2 <- mean_2+2*std_2
up_line_3 <- mean_3+2*std_3
up_line_4 <- mean_4+2*std_4
up_line_5 <- mean_5+2*std_5
up_line_6 <- mean_6+2*std_6

down_line_1 <- mean_1-2*std_1
down_line_2 <- mean_2-2*std_2
down_line_3 <- mean_3-2*std_3
down_line_4 <- mean_4-2*std_4
down_line_5 <- mean_5-2*std_5
down_line_6 <- mean_6-2*std_6

outliers_1 <- which(ts1 > up_line_1 | ts1 < down_line_1)
outliers_2 <- which(ts2 > up_line_2 | ts2 < down_line_2)
outliers_3 <- which(ts3 > up_line_3 | ts3 < down_line_3)
outliers_4 <- which(ts4 > up_line_4 | ts4 < down_line_4)
outliers_5 <- which(ts5 > up_line_5 | ts5 < down_line_5)
outliers_6 <- which(ts6 > up_line_6 | ts6 < down_line_6)

outliers_2 <- c(7, 8, 21, 30)
outliers_5 <- outliers_5_bp
# many new visiters => many views because people come here first time and look on the pages
# in other ts values  dont see like anomaly

hist(ts1, breaks = 30)
hist(ts2, breaks = 30)
hist(ts3, breaks = 30)
hist(ts4, breaks = 30)
hist(ts5, breaks = 30)
hist(ts6, breaks = 30)

correct_ts <- function(ts, outliers, up_line, down_line){
  result <- ts
  for(i in outliers){
    if(i != 1 & i != length(result)){
      result[i] <- (result[i-1] + result[i+1])/2
    } else {
      result[i] <- mean(result[result < up_line & result > down_line])
    }
  }
  return(result)
}

ts2 <- correct_ts(ts2, outliers_2, up_line_2, down_line_2)
ts3 <- correct_ts(ts3, outliers_3, up_line_3, down_line_3)
ts4 <- correct_ts(ts4, outliers_4, up_line_4, down_line_4)
ts5 <- correct_ts(ts5, outliers_5, up_line_5, down_line_5)
ts6 <- correct_ts(ts6, outliers_6, up_line_6, down_line_6)

rm(mean_1, mean_2, mean_3, mean_4, mean_5, mean_6)
rm(median_1, median_2, median_3, median_4, median_5, median_6)
rm(var_1, var_2, var_3, var_4, var_5, var_6)
rm(std_1, std_2, std_3, std_4, std_5, std_6)
rm(up_line_1, up_line_2, up_line_3, up_line_4, up_line_5, up_line_6)
rm(down_line_1, down_line_2, down_line_3, down_line_4, down_line_5, down_line_6)
rm(outliers_1, outliers_2, outliers_3, outliers_4, outliers_5, outliers_6)
rm(outliers_1_bp, outliers_2_bp, outliers_3_bp, outliers_4_bp, outliers_5_bp, outliers_6_bp)

plot(ts1)
plot(ts2)
plot(ts3)
plot(ts4)
plot(ts5)
plot(ts6)

hist(ts1, breaks = 30)
hist(ts2, breaks = 30)
hist(ts3, breaks = 30)
hist(ts4, breaks = 30)
hist(ts5, breaks = 30)
hist(ts6, breaks = 30)

qqnorm(ts1)
qqnorm(ts2)
qqnorm(ts3)
qqnorm(ts4)
qqnorm(ts5)
qqnorm(ts6)

library(moments)

excess_1 <- kurtosis(ts1)
excess_2 <- kurtosis(ts2)
excess_3 <- kurtosis(ts3)
excess_4 <- kurtosis(ts4)
excess_5 <- kurtosis(ts5)
excess_6 <- kurtosis(ts6)

assimetry_1 <- skewness(ts1)
assimetry_2 <- skewness(ts2)
assimetry_3 <- skewness(ts3)
assimetry_4 <- skewness(ts4)
assimetry_5 <- skewness(ts5)
assimetry_6 <- skewness(ts6)

# blije vsex k normalnomu raspredeleniu 2 i 6 no 2 imeet slishkom skoshenuu verhushku o chem govorit excess_2
# a 6 imeet levostoronuu assimetriu

library(tseries)

jarque.bera.test(ts1)
jarque.bera.test(ts2)
jarque.bera.test(ts3)
jarque.bera.test(ts4)
jarque.bera.test(ts5)
jarque.bera.test(ts6)

rm(excess_1, excess_2, excess_3, excess_4, excess_5, excess_6)
rm(assimetry_1, assimetry_2, assimetry_3, assimetry_4, assimetry_5, assimetry_6)

acf(ts1)
pacf(ts1)
acf(ts2)
pacf(ts2)
acf(ts3)
pacf(ts3)
acf(ts4)
pacf(ts4)
acf(ts5)
pacf(ts5)
acf(ts6)
pacf(ts6)


ts_data <- data.frame(ts1 = ts1, ts2 = ts2, ts3 = ts3, ts4 = ts4, ts5 = ts5, ts6 = ts6)

cor(ts_data)

cor.test(ts1, ts2)
cor.test(ts1, ts3) #bad but can try
cor.test(ts1, ts4)
cor.test(ts1, ts5) #bad
cor.test(ts1, ts6) #bad

# 3 part dont know

library(forecast)

auto.arima(ts2)
ets(ts2)
auto.arima(ts3)
ets(ts3)
auto.arima(ts4)
ets(ts4)

auto.arima(ts1)
ets(ts1)
summary(lm(ts1 ~ 1 + ts2 + ts3 + ts4, data = ts_data))
summary(lm(ts1 ~ 1 + ts2 + ts4, data = ts_data))
summary(lm(ts1 ~ 1 + ts4, data = ts_data))
AIC(lm(ts1 ~ 1 + ts2 + ts3 + ts4, data = ts_data))
AIC(lm(ts1 ~ 1 + ts2 + ts4, data = ts_data))
AIC(lm(ts1 ~ 1 + ts4, data = ts_data))
BIC(lm(ts1 ~ 1 + ts2 + ts3 + ts4, data = ts_data))
BIC(lm(ts1 ~ 1 + ts2 + ts4, data = ts_data))
BIC(lm(ts1 ~ 1 + ts4, data = ts_data))

summary(lm(ts1 ~ 1 + ts3 * ts4, data = ts_data))
