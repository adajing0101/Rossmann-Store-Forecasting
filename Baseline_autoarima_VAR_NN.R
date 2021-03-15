#installing libraries
library("pacman")
p_load(magrittr,forecast,ggplot2,TSA,urca,tseries,Metrics,dplyr,vars,AICcmodavg)
#read in data
train <- read.csv('train.csv')
store_8<-train %>% filter(Store==8) %>% arrange(Date)
# Convert Date column to type "Date", get the first day of our data
store_8$Date = as.Date(store_8$Date, format = "%Y-%m-%d")
# Convert "03-01" to day of the year 
dayOfYear = as.numeric(format(store_8[1,3], "%j"))
#keep the date information for future use  
store_8$Date = anydate(store_8$Date)
store_8$year = year(store_8$Date)
store_8$month = month(store_8$Date)
store_8$week <- week(store_8$Date)
store_8$day = day(store_8$Date)
store_8$StateHoliday<-factor(store_8$StateHoliday)
store_8$SchoolHoliday<-factor(store_8$SchoolHoliday)
store_8$DayOfWeek<-factor(store_8$DayOfWeek)
store_8$Open<-factor(store_8$Open)
store_8$Promo<-factor(store_8$Promo)
# double check 365 dates each year without missing 
store_8 %>% count(year,sort = TRUE)
#take a look at the data: no missing data 
summary(store_8)
#drop store number 
store_8 <- subset(store_8, select=-1)
# adjust the col sequence 
store_8 <- subset(store_8,select= c(2:ncol(store_8),1))
#drop customers
store_8<-subset(store_8,select=-3)
#drop dates
store_8_nodate<-subset(store_8,select=-1)

#convert to time series data
store_8_2 <- ts(store_8,frequency = 7)
store_8_ts_nodate<-ts(store_8_nodate,frequency = 7)
#split train test data
train_7<-window(store_8_2, start = c(1,1), end = c(132,4))
test_7<-window(store_8_2, start = c(132,5))
train_nodate<-window(store_8_ts_nodate, start = c(1,1), end = c(132,4))
test_nodate<-window(store_8_ts_nodate, start = c(132,5) )
#getting only sales data
testsales7<-test_7[,2]
trainsales7<-train_7[,2]
#Naive Forecasting#
#Mean Forecast
fit_mean<-meanf(trainsales7,
                h=21,
                lambda="auto",
                biasadj = TRUE,
                level=c(80, 95))
plot(forecast(fit_mean,h=21)$mean,
     main="Baseline Forecast for Sales",
     ylab="Sales",
     xlab="Time",
     include=100)
lines(testsales7,col="red")
checkresiduals(fit_mean)#p-value < 2.2e-16
RMSE_mean<-rmse(forecast(fit_mean, h=21)$mean,
              testsales7) #2992

# Naive Forecast
fit_naive<-naive(trainsales7,
                h=21,
                lambda="auto",
                level=c(80,95))
plot(forecast(fit_naive,h=21)$mean,
     main="Naive Forecast for Sales",
     ylab="Sales",
     xlab="Time",
     include=100)
lines(testsales7,col="red")
checkresiduals(fit_naive)#p-value < 2.2e-16
RMSE_naive<-rmse(forecast(fit_naive, h=21)$mean,
              testsales7)#2931
#Seasonal Naive
fit_snaive<-snaive(trainsales7,
                 h=21,
                 lambda="auto",
                 level=c(80, 95))
plot(forecast(fit_snaive,h=21),
     main="Seasonal Naive Forecast for Sales",
     ylab="Sales",
     xlab="Time",
     include=100)
lines(testsales7,col="red")
checkresiduals(fit_snaive)#p-value < 2.2e-16
RMSE_snaive<-rmse(forecast(fit_snaive, h=21)$mean,
               testsales7)#1696

#graph
plot(forecast(fit_mean,h=21),
     main="Baseline Forecast for Sales",
     ylab="Sales",
     xlab="Time",
     include=100)
lines(testsales7,col="red")
lines(forecast(fit_snaive_7,h=21),col="black")
legend("topleft",
       legend = c("Actual", "Forecasted"), col = c("red","blue"),
       lty=c(1,1))
par(bg=NA)
autoplot(window(trainsales7, start = c(120,1), end = c(132,4))) +
        autolayer(forecast(fit_mean)$mean, series="Mean Forecast") +
        autolayer(forecast(fit_naive)$mean, series="Naïve") +
        autolayer(forecast(fit_snaive)$mean, series="Seasonal Naïve") +
        autolayer(forecast(fit_hw)$mean, series="H-W with Drift") +
        autolayer(testsales7, series = "Actual Sales")+
        xlab("Time") + ylab("Sales")+
        theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
)

#Holt-Winter
fit_hw_add<-hw(trainsales7,
           h=21,
           lambda="auto",
           level=c(80, 95),
           drift=TRUE,
           seasonal="additive")
summary(fit_hw_add)
fit_hw_damped<-hw(trainsales7,
               h=21,
               lambda="auto",
               level=c(80, 95),
               drift=TRUE,
               seasonal="additive",
               damped = TRUE)
#damping showed higher training accuracy
summary(fit_hw_damped)
#check residuals
checkresiduals(fit_hw_damped)#p-value  9.794e-10
#calculate training rmse
RMSE_hw_add<-rmse(forecast(fit_hw_add, h=21)$mean,
                  testsales7)#1500
RMSE_hw_damped<-rmse(forecast(fit_hw_damped, h=21)$mean,
                  testsales7)#2138

#plotting to investigate why Damping performed better in train
autoplot(window(trainsales7, start = c(120,1), end = c(132,4))) +
        autolayer(forecast(fit_hw_damped, h=21)$mean, series="H-W with Damping") +
        autolayer(forecast(fit_hw)$mean, series="H-W with Drift") +
        autolayer(testsales7, series = "Actual Sales")+
        xlab("Time") + ylab("Sales")+
        theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA)
        )

#plotting residuals
ggAcf(fit_hw$residuals)+
theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)
)

#Arima Forecasting#
fit_auto<-auto.arima(trainsales7,lambda = "auto",seasonal = TRUE)
summary(fit_auto)
plot(forecast(fit_auto,h=189),
     main="Auto.Arima Forecast for Sales",
     ylab="Sales",
     xlab="Time",
     include=100)
ggAcf(fit_auto$residuals)
checkresiduals(fit_auto)
lines(testsales,col="red")
RMSE_auto<-rmse(forecast(fit_auto, h=21)$mean,
              testsales7)

###VAR###
#log transform sales data and bind with other variables
train_log<-cbind(BoxCox(train_nodate[,1],lambda="auto"),
                 train_nodate[,2:6])
#initial data without kpss test is non stationary
kpss.test(train_log[,1])
#differencing = 1 makes the series stationary
train_log[,1]%>% diff(differences=1)%>%kpss.test()
#apply differencing
train_diff<-diff(train_log,differences=1)
#log lambda for back transformation
Lambda<-BoxCox.lambda(train_nodate[,1])
colnames(train_diff)<-colnames(train_nodate)
#fit differentiated and log data
fit_var<-VAR(y=train_diff)
summary_fit_var<-summary(fit_var)
forecast_var_raw<-forecast(fit_var, h=21,level=c(95, 80))
###creating VAR forecast###
forecast_var<-forecast(fit_var, h=21,level=c(95, 80))$forecast$Sales$mean
#combining the last actual value in trained with VAR differenced forecast
last_train<-c(train_log[921,1],forecast_var)
#calculat cumsum and take out the first in the list (last number in train)
last_train_cumsum<-ts(cumsum(last_train)[-1],f=7,start = c(132,5))
#transform log back
forecast_var_back<-InvBoxCox(last_train_cumsum,lambda = Lambda)
#calculate RMSE
RMSE_var<-rmse(forecast_var_back,testsales7)
#aic var
AIc_var<-AIC(forecast_var_raw$model)
#residuals
var_residuals<-forecast_var_back-testsales7
var_residuals_train<-residuals(fit_var)
checkresiduals(var_residuals_train[,1])
Box.test(var_residuals, type ="Ljung-Box",lag=14)

##plotting VAR###
autoplot(window(trainsales7, start = c(120,1), end = c(132,4))) +
        autolayer(forecast_var_back, series="VAR Forecast", CI=TRUE) +
        autolayer(testsales7, series = "Actual Sales")+
        xlab("Time") + ylab("Sales")+
        theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA)
        )

RMSE_var<-rmse(forecast_var_back,testsales7)

##neutral network
fit_nn<-nnetar(trainsales7,lambda = "auto",seasonal = TRUE, xreg=train_7[,3:7],h=21)
forecast_nn<-forecast(fit_nn,h=21,xreg=test_7[,3:7],level=c(95, 80))
autoplot(forecast(fit_nn,h=21,xreg=test_7[,3:7],level=c(95, 80)),CI=TRUE)
autoplot(window(trainsales7, start = c(120,1), end = c(132,4))) +
        autolayer(forecast_nn, series="NN Forecast", PI=TRUE) +
        autolayer(testsales7, series = "Actual Sales")+
        autolayer(nn_residuals,series="residuals")+
        xlab("Time") + ylab("Sales")+
        theme(
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA)
        )
RMSE_nn<-rmse(forecast_nn$mean,testsales7)
nn_residuals<-testsales7-forecast_nn$mean
AIC_nn<-AIC(fit_nn$model)
summary(fit_nn)
Box.test(fit_nn$residuals,type="Ljung-Box")
checkresiduals(fit_nn)