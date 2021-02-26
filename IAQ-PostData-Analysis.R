library(plyr)
library(readr)
library(ggplot2)
library(GGally)
library(tidyverse)
library(mlbench)
library(plotly)
library(forecast)
library(fpp2)
library(TTR)
library(arsenal)
library(party)
library(ggpubr)
library(tseries)
library(scatterplot3d)
library(hms)
library(lubridate)


#convert data stored on SD cards from .txt into .csv using Excel or your own preference and
#insert their file path below.

#loading of sensor data - Summer data!
Sstudy <- read_csv("/Users/Dylan/Desktop/MScDataStudySD.csv")
Skitchen <- read_csv("/Users/Dylan/Desktop/MScDataKitchenSD.csv")

#exp <- read_csv("/Users/Dylan/Desktop/expData.csv")

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


#renaming column names with appropriate names - Winter
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

#controlled experiment data
# exp <- exp %>% 
#   rename(
#     date = `26/08/2020`,
#     time =`14:30:02`,
#     temp =`22.22`,
#     pres =`1009.88`,
#     hum = `56`,
#     gas =`304.11`,
#     iaq = `45`,
#     loc = Kitchen
#   )
# 
# #converting from char to date data type
# exp$date <- as.Date(exp$date, "%d/%m/%Y")
# #removing any NA values from experiment data
# exp<-exp[complete.cases(exp),]

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

#computing minima and maxima of study and kitchen gas resistance
min(kitchen$gas)
max(kitchen$gas)
mean(kitchen$gas)
sd(kitchen$gas)

min(study$gas)
max(study$gas)
mean(study$gas)
sd(study$gas)

#data exploration with time series and mean of study sensor data
gasPlot <- ggplot(study, aes(x=date, y=gas)) +
  geom_line() + 
  geom_hline(yintercept = mean(study$gas), color="blue")

tempPlot <-ggplot(study, aes(x=date, y=temp)) +
  geom_line() + 
  geom_hline(yintercept = mean(study$temp), color="blue")

humPlot <-ggplot(study, aes(x=date, y=hum)) +
  geom_line() + 
  geom_hline(yintercept = mean(study$hum), color="blue") 

iaqPlot <-ggplot(study, aes(x=date, y=iaq)) +
  geom_line() + 
  geom_hline(yintercept = mean(study$iaq), color="blue") 

fig <- subplot(tempPlot, humPlot, gasPlot, iaqPlot, nrows = 2)
fig

#data exploration with time series and mean of kitchen sensor data
gasPlot1 <- ggplot(kitchen, aes(x=time, y=gas)) +
  geom_line() + 
  geom_hline(yintercept = mean(kitchen$gas), color="blue")

tempPlot1 <-ggplot(kitchen, aes(x=time, y=temp)) +
  geom_line() + 
  geom_hline(yintercept = mean(kitchen$temp), color="blue")

humPlot1 <-ggplot(kitchen, aes(x=time, y=hum)) +
  geom_line() + 
  geom_hline(yintercept = mean(kitchen$hum), color="blue") 

iaqPlot1 <-ggplot(kitchen, aes(x=time, y=iaq)) +
  geom_line() + 
  geom_hline(yintercept = mean(kitchen$iaq), color="blue") 


fig1 <- subplot(tempPlot1, humPlot1, gasPlot1, iaqPlot1, nrows = 2)
fig1

#data exploration using boxplots; grouped by day for study sensor data
fig2 <- ggplot(study, aes(x = date, y = gas, 
        group = interaction(date))) + geom_boxplot()

fig3 <- ggplot(study, aes(x = date, y = temp, 
        group = interaction(date))) + geom_boxplot()

fig4 <- ggplot(study, aes(x = date, y = hum, 
        group = interaction(date))) + geom_boxplot()

fig5 <- ggplot(study, aes(x = date, y = iaq, 
        group = interaction(date))) + geom_boxplot()

fig6 <- subplot(fig3, fig4, fig2, fig5, nrows = 2)
fig6

#data exploration using boxplots; grouped by day for kitchen sensor data
fig7 <- ggplot(kitchen, aes(x = date, y = gas, 
                          group = interaction(date))) + geom_boxplot()

fig8 <- ggplot(kitchen, aes(x = date, y = temp, 
                          group = interaction(date))) + geom_boxplot()

fig9 <- ggplot(kitchen, aes(x = date, y = hum, 
                          group = interaction(date))) + geom_boxplot()

fig10 <- ggplot(kitchen, aes(x = date, y = iaq, 
                          group = interaction(date))) + geom_boxplot()

fig11 <- subplot(fig8, fig9, fig7, fig10, nrows = 2)
fig11

#further data exploration using tableby function on study sensor data to explore the mean(SD) & range
studyTable <- tableby(date ~ gas + temp + hum, total = FALSE, data = study)
summary(studyTable, text = TRUE)

#further data exploration using tableby function on kitchen sensor data to explore the mean(SD) & range
kitchenTable <- tableby(date ~ gas + temp + hum, total = FALSE, data = kitchen)
summary(kitchenTable, text = TRUE)

#scatter graph to visualise the very weak (-0.041) downhill (negative) linear relationship of gas resistance and time of study data
studyTimeScatter <- ggscatter(study, x = "time", y = "gas", 
                          add = "reg.line",add.params = list(color = "blue", fill = "lightgray"),
                          conf.int = TRUE,  cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Time", ylab = "Gas Resistance")
studyTimeScatter

#scatter graph to visualise the weak (-0.11) downhill (negative) linear relationship of gas resistance and time of kitchen data
kitchenTimeScatter <- ggscatter(kitchen, x = "time", y = "gas", 
                            add = "reg.line", add.params = list(color = "blue", fill = "lightgray"),conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "pearson",
                            xlab = "Time", ylab = "Gas Resistance")
kitchenTimeScatter

#pearson correlation test between humidity and gas variables for study sensor data
studyHumGasCor <- cor.test(study$hum, study$gas, method = "pearson")
studyHumGasCor

#scatter graph to visualise the strong(-0.69) downhill(negative) linear relationship of gas resistance and humidity of study sensor data
studyScatter <- ggscatter(study, x = "gas", y = "hum", 
                             add = "reg.line",add.params = list(color = "blue", fill = "lightgray"), conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             title = "Location - Study", xlab = "Gas Resistance", ylab = "Humidity")
studyScatter


#pearson correlation test between humidity and gas variables for kitchen sensor data
KitchenHumGasCor <- cor.test(kitchen$hum, kitchen$gas, method = "pearson")
KitchenHumGasCor

#scatter graph to visualise the strong(-0.62) downhill(negative) linear relationship of gas resistance and humidity of kitchen sensor data
kitchenScatter <- ggscatter(kitchen, x = "gas", y = "hum", 
                          add = "reg.line", add.params = list(color = "blue", fill = "lightgray"),conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          title = "Location - kitchen", xlab = "Gas Resistance", ylab = "Humidity")
kitchenScatter
subplot(studyScatter, kitchenScatter, nrows = 2)

#pearson correlation test between temperature and gas variables for study sensor data
studyTempGasCor <- cor.test(study$temp, study$gas, method = "pearson")
studyTempGasCor

#scatter graph to visualise the very weak(-0.11) downhill(negative) linear relationship of gas resistance and temperature of study sensor data
studyScatter1 <- ggscatter(study, x = "gas", y = "temp", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Gas Resistance", ylab = "Temperature")
studyScatter1

#pearson correlation test between temperature and gas variables for kitchen sensor data
KitchenTempGasCor <- cor.test(kitchen$temp, kitchen$gas, method = "pearson")
KitchenTempGasCor

#scatter graph to visualise the also very weak(-0.16) downhill(negative) linear relationship of gas resistance and temperature of kitchen sensor data
kitchenScatter1 <- ggscatter(kitchen, x = "gas", y = "temp", 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "pearson",
                            xlab = "Gas Resistance", ylab = "Humidity")
kitchenScatter1

#gas distribution using histogram for study sensor data
studyGasDist<-hist(study$gas, breaks=10, col="#56B4E9", xlab="Gas Resistance",
              main="Gas Distribution with Normal Curve")
xfit<-seq(min(study$gas),max(study$gas),length=40)
yfit<-dnorm(xfit,mean=mean(study$gas),sd=sd(study$gas))
yfit <- yfit*diff(studyGasDist$mids[1:2])*length(study$gas)
lines(xfit, yfit, col="#999999", lwd=2)
rug(study$gas,col="red")

#gas distribution using histogram for kitchen sensor data
kitchenGasDist<-hist(kitchen$gas, breaks=10, col="#56B4E9", xlab="Gas Resistance",
                   main="Gas Distribution with Normal Curve")
xfit<-seq(min(kitchen$gas),max(kitchen$gas),length=40)
yfit<-dnorm(xfit,mean=mean(kitchen$gas),sd=sd(kitchen$gas))
yfit <- yfit*diff(kitchenGasDist$mids[1:2])*length(kitchen$gas)
lines(xfit, yfit, col="#999999", lwd=2)
rug(kitchen$gas,col="red")

#QQplot of gas study sensor data with line going through the first and third quartile. 
#this can be used to judge (non-statistically) the goodness-of-fit of the QQ-plot to a
#straight line. 
qqnorm(study$gas) 
  qqline(study$gas)

#QQplot of gas kitchen sensor data with line going through the first and third quartile. 
#This can be used to judge (non-statistically) the goodness-of-fit of the QQ-plot to a
#straight line. 
qqnorm(kitchen$gas) 
  qqline(kitchen$gas)

#study scatterplot matrix. Scatterplots of each pair 
#of numeric variable are drawn on the left part of the figure. Pearson correlation is displayed on the right. 
#Variable distribution is available on the diagonal.
ggpairs(data=study, columns=c("gas", "hum", "temp"), title="Indoor Air Quality - Study - Winter")

#linear model for study data between humidity and gas variables
fit_1 <- lm(hum ~ gas, data = study)
summary(fit_1)
confint(fit_1)

#kitchen scatterplot matrix
ggpairs(data=kitchen, columns=c("gas", "hum", "temp"), title="Indoor Air Quality - Kitchen - Winter")

#linear model for kitchen data between humidity and gas variables
fit_1k <- lm(hum ~ gas, data = kitchen)
summary(fit_1k)
confint(fit_1k)

#histogram for study model resisduals. Residuals in a statistical or machine learning model are the differences between
#observed and predicted values of data. They are a diagnostic measure used when assessing the quality of a model. 
#They are also known as errors.
ggplot(data=study, aes(fit_1$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals - Study")

#histogram for kitchen model resisduals
ggplot(data=kitchen, aes(fit_1k$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals - Kitchen")

#linear model fitted to the study data to show or predict the relationship between gas and humidity
ggplot(data = study, aes(x = gas, y = hum)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Study Data ")

#predicting humidity for a given gas value
predict(fit_1, data.frame(gas = 427))

#linear model fitted to the study data to show or predict the relationship between gas and humidity
ggplot(data = kitchen, aes(x = gas, y = hum)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Kitchen Data ")

#predicting humidity for a given gas value
predict(fit_1k, data.frame(gas = 500))

#multi linear model for study data between humidity, temperature and gas variables
fit_2 <- lm(gas ~ temp + hum , data = study)
summary(fit_2)
confint(fit_2)

#removing any NA values from study data
study<-study[complete.cases(study),]

#creating 3d scatter plot for the multi linear study data model for predicting gas
temp <- seq(16,26, by=2) #temp vector
hum<- seq(36,92, by=4) ## hum vector
pred_grid <- expand.grid( hum=hum,temp=temp)

#predicitng gas and inserting into predicted grid
pred_grid$gas2 <-predict(fit_2, pred_grid)
fit_2_sp <- scatterplot3d(pred_grid$gas2,pred_grid$temp, pred_grid$hum, angle = 20, color = "dodgerblue", pch = 1, zlab = "Humidity(%)", ylab = "Temperature(c)", xlab = "Gas Resistance (ohm)" )
fit_2_sp$points3d(study$gas, study$temp, study$hum, pch=1)

#multi linear model for kitchen data between humidity, temperature and gas variables
fit_3 <- lm(gas ~ temp + hum , data = kitchen)
summary(fit_3)
confint(fit_3)

#creating 3d scatter plot for the multi linear kitchen data model for predicting gas
temp <- seq(15,26, by=2) ## make a temp vector
hum<- seq(36,92, by=4) ## make a hum vector
pred_grid <- expand.grid( hum=hum,temp=temp)

#predicitng gas and inserting into predicted grid
pred_grid$gas3 <-predict(fit_3, pred_grid)
fit_3_sp <- scatterplot3d( pred_grid$gas3,pred_grid$temp, pred_grid$hum, angle = 20, color = "dodgerblue", pch = 1, zlab = "Humidity(%)", ylab = "Temperature(c)", xlab = "Gas Resistance (ohm)" )
fit_3_sp$points3d(kitchen$gas, kitchen$temp, kitchen$hum, pch=1)


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





stdGasPlot <- ggplot(study, aes(x=time, y=stdStudGas)) +
  geom_line()

kitGasPlot <-  ggplot(kitchen, aes(x=time, y=stdKitGas)) +
  geom_line() 
kitGasPlot

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

#------EXPERIMENT DATA------#
#data exploration with time series and mean of study sensor data
fig15 <-  plot_ly(exp, x = ~time, y = ~gas, type = 'scatter', mode = 'lines')
fig15 <- fig15 %>% layout(title = 'Experimental IAQ Data',
                      xaxis = list(title = 'Time'),
                      yaxis = list (title = 'Gas Resistance'))




