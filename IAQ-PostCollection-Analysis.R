library(plyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(hms)
library(lubridate)
library(GGally)
library(plotly)

#convert data stored on SD cards from .txt into .csv using Excel or your own preference and
#insert their file path below.

#loading of sensor data - Summer data!
Sstudy <- read_csv("/Users/Dylan/Desktop/MScDataStudySD.csv")
Skitchen <- read_csv("/Users/Dylan/Desktop/MScDataKitchenSD.csv")

#loading of sensor data - Winter data!
study <- read_csv("/Users/Dylan/Desktop/WinterBedroomDataIAQ.csv")
kitchen <- read_csv("/Users/Dylan/Desktop/WinterKitchenDataIAQ.csv")

#glimpse into loaded data
glimpse(study)
glimpse(kitchen)

#change the values of the columns to the first row of data in the .csv file
#renaming column names with appropriate names - Summer
Sstudy <- Sstudy %>%
rename(
  date = X1,
  time =X2,
  temp =`20.34`,
  pres =`1008.31`,
  hum = `79.1`,
  gas =`7.67`,
  iaq = `0`,
  loc = Bedroom
)

Sstudy$date <- as.Date(Sstudy$date, "%d/%m/%Y")


#renaming column names with appropriate names - Autumn
study <- study %>% 
  rename(
    date = X1,
    time =X2,
    temp =`21.98`,
    pres =`1032.48`,
    hum = `58`,
    gas =`42.38`,
    iaq = `0`,
    loc = Bedroom
  )

#converting from char to date data type
study$date <- as.Date(study$date, "%d/%m/%Y")

#Summer data
Skitchen <- Skitchen %>%
  rename(
    date = X1,
    time =X2,
    temp =`19.87`,
    pres =`1010.57`,
    hum = `58.5`,
    gas =`90.3`,
    iaq = `0`,
    loc = Kitchen
  )

#converting from char to date data type
Skitchen$date <- as.Date(Skitchen$date, "%d/%m/%Y")

#Winter data
 kitchen <- kitchen %>% 
  rename(
    date = X1,
    time =X2,
    temp =`21.54`,
    pres =`1032.44`,
    hum = `53.8`,
    gas =`44.28`,
    iaq = `0`,
    loc = Kitchen
  )
#converting from char to date data type
kitchen$date <- as.Date(kitchen$date, "%d/%m/%Y")

#computing minima and maxima of study and kitchen gas resistance - Autumn
min(kitchen$gas)
max(kitchen$gas)
mean(kitchen$gas)
sd(kitchen$gas)

min(study$gas)
max(study$gas)
mean(study$gas)
sd(study$gas)

#computing minima and maxima of study and kitchen gas resistance - Summer
min(Skitchen$gas)
max(Skitchen$gas)
mean(Skitchen$gas)
sd(Skitchen$gas)

min(Sstudy$gas)
max(Sstudy$gas)
mean(Sstudy$gas)
sd(Sstudy$gas)

#study scatterplot matrix. Scatterplots of each pair 
#of numeric variable are drawn on the left part of the figure. Pearson correlation is displayed on the right. 
#Variable distribution is available on the diagonal.
ggpairs(data=study, columns=c("gas", "hum", "temp"), title="Indoor Air Quality - Study - Autumn")

#kitchen scatterplot matrix
ggpairs(data=kitchen, columns=c("gas", "hum", "temp"), title="Indoor Air Quality - Kitchen - Autumn")

#autumn kitchen scatterplot matrix
ggpairs(data=Skitchen, columns=c("gas", "hum", "temp"), title="Indoor Air Quality - Kitchen - Summer")

#autumn scatterplot matrix
ggpairs(data=Sstudy, columns=c("gas", "hum", "temp"), title="Indoor Air Quality - Study - Summer")

#In order to identify trends in the collected gas resistance data, gas values are normalised for both 
#the kitchen and study. The Z-score normalization technique used: Z=  (x-μ )/σ
#where 𝑍 is the standard score, x is the raw score or gas resistance values, μ is the population mean,
#and σ is the population standard deviation. This normalisation takes place below:
kitchen$stdKitGas <-(kitchen$gas - mean(kitchen$gas)) / sd(kitchen$gas)
study$stdStudGas <-(study$gas - mean(study$gas)) / sd(study$gas)

Skitchen$SstdKitGas <-(Skitchen$gas - mean(Skitchen$gas)) / sd(Skitchen$gas)
Sstudy$SstdStudGas <-(Sstudy$gas - mean(Sstudy$gas)) / sd(Sstudy$gas)

kitchen$stdStudGas <- study$stdStudGas[match(kitchen$time, study$time)]
kitchen$studHum <- study$hum[match(kitchen$time, study$time)]

kitchen$SstdStudGas <- Sstudy$SstdStudGas[match(kitchen$time, Sstudy$time)]
kitchen$SstudHum <- Sstudy$hum[match(kitchen$time, Sstudy$time)]

kitchen$SstdKitGas <- Skitchen$SstdKitGas[match(kitchen$time, Skitchen$time)]
kitchen$SKitHum <- Skitchen$hum[match(kitchen$time, Skitchen$time)]

#------SUMMER DATA------#
fig12 <- kitchen %>%
  mutate(time2 = paste(today(), time) %>% as_datetime()) %>%
  ggplot(aes(time2), xlab("da")) +
  geom_line(aes(y=stdKitGas)) +
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H")

fig13 <- kitchen %>%
  mutate(time2 = paste(today(), time) %>% as_datetime()) %>%
  ggplot(aes(time2)) +
  geom_line(aes(y=stdStudGas), color = "blue") +
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H") 
  

fig14 <- subplot(fig12 , fig13, nrows = 2)
fig14 <- fig14 %>% layout(title = 'Z-Score Of Kitchen(Black) And Study(Blue) Gas Resistance Against Time ')
fig14

#------AUTUMN DATA------#
fig15 <- kitchen %>%
  mutate(time2 = paste(today(), time) %>% as_datetime()) %>%
  ggplot(aes(time2), xlab("da")) +
  geom_line(aes(y=SstdKitGas)) +
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H")

fig16 <- kitchen %>%
  mutate(time2 = paste(today(), time) %>% as_datetime()) %>%
  ggplot(aes(time2)) +
  geom_line(aes(y=SstdStudGas), color = "blue") +
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H") 


fig17 <- subplot(fig15 , fig16, nrows = 2)
fig17 <- fig17 %>% layout(title = 'Z-Score Of Kitchen(Black) And Study(Blue) Gas Resistance Against Time ')
fig17

#------AUTUMN & SUMMER DATA------#
fig18 <- kitchen %>%
  mutate(time2 = paste(today(), time) %>% as_datetime()) %>%
  ggplot(aes(time2), xlab("da")) +
  geom_line(aes(y=SstdKitGas), color = "red") +
  geom_line(aes(y=stdKitGas)) +
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H")

fig19 <- kitchen %>%
  mutate(time2 = paste(today(), time) %>% as_datetime()) %>%
  ggplot(aes(time2)) +
  geom_line(aes(y=SstdStudGas), color = "red") +
  geom_line(aes(y=stdStudGas), color = "blue") +
  scale_x_datetime(breaks = scales::date_breaks("60 mins"), date_labels = "%H") 


fig20 <- subplot(fig18 , fig19, nrows = 2)
fig20 <- fig20 %>% layout(title = 'Z-Score Of Kitchen(Black) And Study(Blue) Gas Resistance Against Time ')
fig20

#------CONTROLLED EXPERIMENT DATA------#
exp <- read_csv("/Users/Dylan/Desktop/expData.csv")

#controlled experiment data
exp <- exp %>% 
  rename(
    date = `26/08/2020`,
    time =`14:30:02`,
    temp =`22.22`,
    pres =`1009.88`,
    hum = `56`,
    gas =`304.11`,
    iaq = `45`,
    loc = Kitchen
  )

#converting from char to date data type
exp$date <- as.Date(exp$date, "%d/%m/%Y")

#removing any NA values from experiment data
exp<-exp[complete.cases(exp),]

#data exploration with time series and mean of study sensor data
fig21 <-  plot_ly(exp, x = ~time, y = ~gas, type = 'scatter', mode = 'lines')
fig21 <- fig21 %>% layout(title = 'Experimental IAQ Data',
                      xaxis = list(title = 'Time'),
                      yaxis = list (title = 'Gas Resistance'))
fig21



