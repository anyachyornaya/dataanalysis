#__________________________________________
#__________________________________________
#3 Загрузка и анализ временных рядов в R
#__________________________________________
#__________________________________________
#3.1 Чтение данных из файла типа .xlsx
library("xlsx")
library("moments")
library(tidyverse)
df <- data.frame(read.xlsx2('C:/data1.xlsx', sheetIndex = 1, header = T,
                            colClasses = c("Date", "integer", "character", "character", "character", "character")))
#3.2 Удаление пропусков
df<-na.omit(df)
#3.3 Основная информация о data frame
str(df)
head(df)
#3.4 Кросс-визуализация
plot(df, main = 'Кросс-визуализация исходных данных', cex = 0.1)
plot(df$weekday~df$closing.period)
#__________________________________________
#__________________________________________
#4 Описательные статистики
#__________________________________________
#__________________________________________
summary(df)
output

#volume
plot(df$date, df$volume, main = paste0('Ряд volume. График.'))
plot.ts(x=df$date, y=df$volume)
hist(df$volume, main = paste0('Ряд volume. Гистограмма.'))
boxplot(df$volume, main = paste0('Ряд volume. Ящичковая диаграмма.'))
#коэффициент вариации
coef_variety <- sd(df$volume)/mean(df$volume)
#QQ
qqnorm(df$volume, main = 'QQ-plot')
qqline(df$volume)
#коэффициент эксцесса
skewness(df$volume)
#коэффициент ассиметрии
kurtosis(df$volume)
#Тест Жарке-Бера
jarque.test(df$volume) #p-value < 0.05 Гипотеза о нормальности ряда отклоняется
