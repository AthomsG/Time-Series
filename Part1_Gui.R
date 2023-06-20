library(readxl)
library(imputeTS)
library(extremogram)
library(tseries)
library(forecast)
library(psych)
library(lmtest)
library(TSclust)
library(dtwclust)
library(ggdendro)
library(gridExtra)

O3<-as.data.frame(Quantidade_Ozono)
O3 <- na_interpolation(QualidadeARO3, option = "linear")
summary(O3)
describe(O3)


par(mfrow=c(1,1))

#######################################
########## Alfragide-Amadora ##########
#######################################

# Missing values
statsNA(O3[,1])
ggplot_na_distribution(O3[,1])

# Time series
Alfragide_TS=ts(O3[,1], start=c(1, 1), end=c(365, 24), frequency=24)
plot(Alfragide_TS, main = "Alfragide", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu * m^2 / m^3))


# Box Cox transformation -> w = (y^lambda-1)/lambda
Alfragide_lambda=BoxCox.lambda(Alfragide_TS) #Calculates the best lambda
print(Alfragide_lambda)
Alfragide_BC=BoxCox(Alfragide_TS, lambda = Alfragide_lambda) #Makes the BOXCOX transformation
plot(Alfragide_BC, main = "Alfragide_BC", xlab = "", ylab = "") # no trend but seasonality


ggtsdisplay(Alfragide_BC)#Displays the time series, the ACF and PACF


# Model 1 without seasonal differences
Alfragide_fit1 <- Arima(Alfragide_TS, order = c(1,0,2), 
                        seasonal = c(2,0,0), lambda = Alfragide_lambda)#model_Sarima model(1,0,2)(2,0,0)

coeftest(Alfragide_fit1)#z-test of estimated coefficients
checkresiduals(Alfragide_fit1)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Alfragide_fit1$residuals)

summary(Alfragide_fit1) # AIC=64870.57   AICc=64870.58   BIC=64920.12   RMSE = 7.725324  MAE=5.355125

autoplot(Alfragide_TS)+autolayer(Alfragide_fit1$fitted,series='Fit')

Alfragide_Prediction1<-forecast(Alfragide_fit1, h=5, level=95)
print(Alfragide_Prediction1$mean)#point prediction for the next five time points
print(Alfragide_Prediction1$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Alfragide_Prediction1$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Alfragide_Prediction1, Alfragide_Prediction1$upper[1], Alfragide_Prediction1$lower[1])#prediction plot


#Model 2 without seasonal differences
Alfragide_fit2 <- Arima(Alfragide_TS, order = c(1,0,1), 
                        seasonal = c(1,0,1), lambda = Alfragide_lambda)

coeftest(Alfragide_fit2)#z-test of estimated coefficients
checkresiduals(Alfragide_fit2)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Alfragide_fit2$residuals)

summary(Alfragide_fit2) # AIC=64133.92   AICc=64133.93   BIC=64176.39 RMSE=7.397969 MAE=5.092091

autoplot(Alfragide_TS)+autolayer(Alfragide_fit1$fitted,series='Fit')#Fitted series against original one


Alfragide_Prediction2<-forecast(Alfragide_fit2, h=5, level=95)
print(Alfragide_Prediction2$mean)#point prediction for the next five time points
print(Alfragide_Prediction2$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Alfragide_Prediction2$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Alfragide_Prediction2, Alfragide_Prediction2$upper[1], Alfragide_Prediction2$lower[1])#prediction plot

#Model 3 without season differences
Alfragide_fit3 <- Arima(Alfragide_TS, order = c(1,0,1), 
                        seasonal = c(1,0,0), lambda = Alfragide_lambda)

coeftest(Alfragide_fit3)#z-test of estimated coefficients
checkresiduals(Alfragide_fit3)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Alfragide_fit3$residuals)

summary(Alfragide_fit3) # AIC=65055.5   AICc=65055.5   BIC=65090.89 RMSE=7.80827 MAE=5.423675

autoplot(Alfragide_TS)+autolayer(Alfragide_fit1$fitted,series='Fit')#Fitted series against original one


Alfragide_Prediction3<-forecast(Alfragide_fit3, h=5, level=95)
print(Alfragide_Prediction3$mean)#point prediction for the next five time points
print(Alfragide_Prediction3$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Alfragide_Prediction3$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Alfragide_Prediction3, Alfragide_Prediction3$upper[1], Alfragide_Prediction3$lower[1])#prediction plot


#Model 4 without season differences

Alfragide_fit4 <- Arima(Alfragide_TS, order = c(1,0,1), 
                        seasonal = c(0,0,1), lambda = Alfragide_lambda)

coeftest(Alfragide_fit4)#z-test of estimated coefficients
checkresiduals(Alfragide_fit4)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Alfragide_fit4$residuals)

summary(Alfragide_fit4) # AIC=65181.48   AICc=65181.49   BIC=65216.87 RMSE=7.864246 MAE=5.454775

autoplot(Alfragide_TS)+autolayer(Alfragide_fit1$fitted,series='Fit')#Fitted series against original one


Alfragide_Prediction3<-forecast(Alfragide_fit3, h=5, level=95)
print(Alfragide_Prediction3$mean)#point prediction for the next five time points
print(Alfragide_Prediction3$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Alfragide_Prediction3$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Alfragide_Prediction3, Alfragide_Prediction3$upper[1], Alfragide_Prediction3$lower[1])#prediction plot


#Model 5 without seasanol differences
Alfragide_fit5 <- Arima(Alfragide_TS, order = c(1,0,1), 
                        seasonal = c(2,0,1), lambda = Alfragide_lambda)

coeftest(Alfragide_fit5)#z-test of estimated coefficients
checkresiduals(Alfragide_fit5)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Alfragide_fit5$residuals)

summary(Alfragide_fit5) # AIC=64094.35   AICc=64094.36   BIC=64143.89 RMSE=7.381181 MAE=5.081754

autoplot(Alfragide_TS)+autolayer(Alfragide_fit1$fitted,series='Fit')#Fitted series against original one


Alfragide_Prediction5<-forecast(Alfragide_fit5, h=5, level=95)
print(Alfragide_Prediction5$mean)#point prediction for the next five time points
print(Alfragide_Prediction5$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Alfragide_Prediction5$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Alfragide_Prediction5, Alfragide_Prediction5$upper[1], Alfragide_Prediction5$lower[1])#prediction plot



#Model 6 without seasonal differences
Alfragide_fit6 <- Arima(Alfragide_TS, order = c(1,0,1), 
                        seasonal = c(1,0,2), lambda = Alfragide_lambda)

coeftest(Alfragide_fit6)#z-test of estimated coefficients
checkresiduals(Alfragide_fit6)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Alfragide_fit6$residuals)

summary(Alfragide_fit6) # AIC=64094.52   AICc=64094.54   BIC=64144.07 RMSE=7.381232 MAE=5.081666

autoplot(Alfragide_TS)+autolayer(Alfragide_fit1$fitted,series='Fit')#Fitted series against original one


Alfragide_Prediction6<-forecast(Alfragide_fit6, h=5, level=95)
print(Alfragide_Prediction6$mean)#point prediction for the next five time points
print(Alfragide_Prediction6$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Alfragide_Prediction6$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Alfragide_Prediction6, Alfragide_Prediction6$upper[1], Alfragide_Prediction6$lower[1])#prediction plot


########################################### Seasonal Diff
Alfragide_BC_diffseasonal=diff(Alfragide_BC,lag=24)
plot(Alfragide_BC_diffseasonal)

ggtsdisplay(Alfragide_BC_diffseasonal)


Best_aic<- auto.arima(Alfragide_BC_diffseasonal, trace = TRUE, ic = "aic", d=0, D=1) #Best (1,0,1)(2,1,0)

Best_aicc <- auto.arima(Alfragide_BC_diffseasonal, trace = TRUE, ic = "aicc", d = 0, D=1)

Best_BIC <- auto.arima(Alfragide_BC_diffseasonal, trace = TRUE, ic = "bic", d = 0, D=1)

#Model 1 with seasonal differences, auto.arima model
Alfragide_BC_fit1 <- Arima(Alfragide_TS, order = c(1,0,1), 
                           seasonal = c(2,1,0), lambda = Alfragide_lambda)

coeftest(Alfragide_BC_fit1)#z-test of estimated coefficients
checkresiduals(Alfragide_BC_fit1)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Alfragide_BC_fit1$residuals)

summary(Alfragide_BC_fit1) # AIC=65921.52   AICc=65921.52   BIC=65956.89 RMSE=8.285256 MAE=5.923435

autoplot(Alfragide_TS)+autolayer(Alfragide_BC_fit1$fitted,series='Fit')#Fitted series against original one


Alfragide_Prediction_BC1<-forecast(Alfragide_BC_fit1, h=5, level=95)
print(Alfragide_Prediction_BC1$mean)#point prediction for the next five time points
print(Alfragide_Prediction_BC1$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Alfragide_Prediction_BC1$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Alfragide_Prediction_BC1, Alfragide_Prediction_BC1$upper[1], Alfragide_Prediction_BC1$lower[1])#prediction plot




###########################
########## Beato ########## TODO
###########################


# Time series
Beato_TS=ts(O3[,2], start=c(1, 1), end=c(365, 24), frequency=24)
plot(Beato_TS, main = "Beato", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu * m^2 / m^3))


# Box Cox transformation -> w = (y^lambda-1)/lambda
Beato_lambda=BoxCox.lambda(Beato_TS) #Calculates the best lambda
print(Beato_lambda)
Beato_BC=BoxCox(Beato_TS, lambda = Beato_lambda) #Makes the BOXCOX transformation
plot(Beato_BC, main = "Beato_BC", xlab = "", ylab = "") # no trend but seasonality

ggtsdisplay(Beato_BC)#Displays the time series, the ACF and PACF

# Model 1 without seasonal differences
Beato_fit1 <- Arima(Beato_TS, order = c(1,0,2), 
                    seasonal = c(2,0,0), lambda = Beato_lambda) # model_Sarima model(1,0,2)(2,0,0)

coeftest(Beato_fit1)#z-test of estimated coefficients
checkresiduals(Beato_fit1)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Beato_fit1$residuals)

summary(Beato_fit1) # AIC=67315.2   AICc=67315.22   BIC=67364.75   RMSE = 6.78989  MAE=4.715054

autoplot(Beato_TS)+autolayer(Beato_fit1$fitted,series='Fit')

Beato_Prediction1<-forecast(Beato_fit1, h=5, level=95)
print(Beato_Prediction1$mean)#point prediction for the next five time points
print(Beato_Prediction1$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Beato_Prediction1$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Beato_Prediction1, Beato_Prediction1$upper[1], Beato_Prediction1$lower[1])#prediction plot


#Model 2 without seasonal differences
Beato_fit2 <- Arima(Beato_TS, order = c(1,0,1), 
                    seasonal = c(1,0,1), lambda = Beato_lambda)

coeftest(Beato_fit2)#z-test of estimated coefficients
checkresiduals(Beato_fit2)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Beato_fit2$residuals)

summary(Beato_fit2) # AIC=66529.47   AICc=66529.48   BIC=66571.94 RMSE=6.483627 MAE=4.475702

autoplot(Beato_TS)+autolayer(Beato_fit1$fitted,series='Fit')#Fitted series against original one


Beato_Prediction2<-forecast(Beato_fit2, h=5, level=95)
print(Beato_Prediction2$mean)#point prediction for the next five time points
print(Beato_Prediction2$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Beato_Prediction2$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Beato_Prediction2, Beato_Prediction2$upper[1], Beato_Prediction2$lower[1])#prediction plot

#Model 3 without season differences
Beato_fit3 <- Arima(Beato_TS, order = c(1,0,1), 
                    seasonal = c(1,0,0), lambda = Beato_lambda)

coeftest(Beato_fit3)#z-test of estimated coefficients
checkresiduals(Beato_fit3)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Beato_fit3$residuals)

summary(Beato_fit3) # AIC=67559.69   AICc=67559.7   BIC=67595.08 RMSE=6.887997 MAE=4.788798

autoplot(Beato_TS)+autolayer(Beato_fit1$fitted,series='Fit')#Fitted series against original one


Beato_Prediction3<-forecast(Beato_fit3, h=5, level=95)
print(Beato_Prediction3$mean)#point prediction for the next five time points
print(Beato_Prediction3$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Beato_Prediction3$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Beato_Prediction3, Beato_Prediction3$upper[1], Beato_Prediction3$lower[1])#prediction plot


#Model 4 without season differences

Beato_fit4 <- Arima(Beato_TS, order = c(1,0,1), 
                    seasonal = c(0,0,1), lambda = Beato_lambda)

coeftest(Beato_fit4)#z-test of estimated coefficients
checkresiduals(Beato_fit4)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Beato_fit4$residuals)

summary(Beato_fit4) # AIC=67726.57   AICc=67726.58   BIC=67761.96 RMSE=6.956726 MAE=4.827848

autoplot(Beato_TS)+autolayer(Beato_fit1$fitted,series='Fit')#Fitted series against original one


Beato_Prediction3<-forecast(Beato_fit3, h=5, level=95)
print(Beato_Prediction3$mean)#point prediction for the next five time points
print(Beato_Prediction3$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Beato_Prediction3$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Beato_Prediction3, Beato_Prediction3$upper[1], Beato_Prediction3$lower[1])#prediction plot


#Model 5 without seasanol differences
Beato_fit5 <- Arima(Beato_TS, order = c(1,0,1), 
                    seasonal = c(2,0,1), lambda = Beato_lambda)

coeftest(Beato_fit5)#z-test of estimated coefficients
checkresiduals(Beato_fit5)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Beato_fit5$residuals)

summary(Beato_fit5) # AIC=66481.29   AICc=66481.3   BIC=66530.84 RMSE=6.465221 MAE=4.46572

autoplot(Beato_TS)+autolayer(Beato_fit1$fitted,series='Fit')#Fitted series against original one


Beato_Prediction5<-forecast(Beato_fit5, h=5, level=95)
print(Beato_Prediction5$mean)#point prediction for the next five time points
print(Beato_Prediction5$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Beato_Prediction5$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Beato_Prediction5, Beato_Prediction5$upper[1], Beato_Prediction5$lower[1])#prediction plot



#Model 6 without seasonal differences
Beato_fit6 <- Arima(Beato_TS, order = c(1,0,1), 
                    seasonal = c(1,0,2), lambda = Beato_lambda)

coeftest(Beato_fit6)#z-test of estimated coefficients
checkresiduals(Beato_fit6)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Beato_fit6$residuals)

summary(Beato_fit6) # AIC=66483.52   AICc=66483.54   BIC=66533.07 RMSE=6.466091 MAE=4.465544

autoplot(Beato_TS)+autolayer(Beato_fit1$fitted,series='Fit')#Fitted series against original one


Beato_Prediction6<-forecast(Beato_fit6, h=5, level=95)
print(Beato_Prediction6$mean)#point prediction for the next five time points
print(Beato_Prediction6$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Beato_Prediction6$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Beato_Prediction6, Beato_Prediction6$upper[1], Beato_Prediction6$lower[1])#prediction plot


########################################### Seasonal Diff
Beato_BC_diffseasonal=diff(Beato_BC,lag=24)
plot(Beato_BC_diffseasonal)

ggtsdisplay(Beato_BC_diffseasonal)


Best_aic<- auto.arima(Beato_BC_diffseasonal, trace = TRUE, ic = "aic", d=0, D=1) #Best (1,0,1)(2,1,0)

Best_aicc <- auto.arima(Beato_BC_diffseasonal, trace = TRUE, ic = "aicc", d = 0, D=1)

Best_BIC <- auto.arima(Beato_BC_diffseasonal, trace = TRUE, ic = "bic", d = 0, D=1)

#Model 1 with seasonal differences, auto.arima model
Beato_BC_fit1 <- Arima(Beato_TS, order = c(1,0,1), 
                       seasonal = c(2,1,0), lambda = Beato_lambda)

coeftest(Beato_BC_fit1)#z-test of estimated coefficients
checkresiduals(Beato_BC_fit1)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Beato_BC_fit1$residuals)

summary(Beato_BC_fit1) # AIC=65921.52   AICc=65921.52   BIC=65956.89 RMSE=8.285256 MAE=5.923435

autoplot(Beato_TS)+autolayer(Beato_BC_fit1$fitted,series='Fit')#Fitted series against original one


Beato_Prediction_BC1<-forecast(Beato_BC_fit1, h=5, level=95)
print(Beato_Prediction_BC1$mean)#point prediction for the next five time points
print(Beato_Prediction_BC1$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Beato_Prediction_BC1$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Beato_Prediction_BC1, Beato_Prediction_BC1$upper[1], Beato_Prediction_BC1$lower[1])#prediction plot




#############################
########## Olivais ##########
#############################


# Time series
Olivais_TS=ts(O3[,4], start=c(1, 1), end=c(365, 24), frequency=24)
plot(Olivais_TS, main = "Olivais", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu * m^2 / m^3))


# Box Cox transformation -> w = (y^lambda-1)/lambda
Olivais_lambda=BoxCox.lambda(Olivais_TS) #Calculates the best lambda
print(Olivais_lambda)
Olivais_BC=BoxCox(Olivais_TS, lambda = Olivais_lambda) #Makes the BOXCOX transformation
plot(Olivais_BC, main = "Olivais_BC", xlab = "", ylab = "") # no trend but seasonality

ggtsdisplay(Olivais_BC)#Displays the time series, the ACF and PACF

# Model 1 without seasonal differences
Olivais_fit1 <- Arima(Olivais_TS, order = c(1,0,2), 
                      seasonal = c(2,0,0), lambda = Olivais_lambda) # model_Sarima model(1,0,2)(2,0,0)

coeftest(Olivais_fit1)#z-test of estimated coefficients
checkresiduals(Olivais_fit1)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Olivais_fit1$residuals)

summary(Olivais_fit1) # AIC=67441.2   AICc=67441.21   BIC=67490.74   RMSE = 9.044625  MAE=6.456063

autoplot(Olivais_TS)+autolayer(Olivais_fit1$fitted,series='Fit')

Olivais_Prediction1<-forecast(Olivais_fit1, h=5, level=95)
print(Olivais_Prediction1$mean)#point prediction for the next five time points
print(Olivais_Prediction1$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Olivais_Prediction1$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Olivais_Prediction1, Olivais_Prediction1$upper[1], Olivais_Prediction1$lower[1])#prediction plot


#Model 2 without seasonal differences
Olivais_fit2 <- Arima(Olivais_TS, order = c(1,0,1), 
                      seasonal = c(1,0,1), lambda = Olivais_lambda)

coeftest(Olivais_fit2)#z-test of estimated coefficients
checkresiduals(Olivais_fit2)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Olivais_fit2$residuals)

summary(Olivais_fit2) # AIC=64484.08   AICc=64484.09   BIC=64526.55 RMSE=7.700016 MAE=5.320585

autoplot(Olivais_TS)+autolayer(Olivais_fit1$fitted,series='Fit')#Fitted series against original one


Olivais_Prediction2<-forecast(Olivais_fit2, h=5, level=95)
print(Olivais_Prediction2$mean)#point prediction for the next five time points
print(Olivais_Prediction2$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Olivais_Prediction2$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Olivais_Prediction2, Olivais_Prediction2$upper[1], Olivais_Prediction2$lower[1])#prediction plot

#Model 3 without season differences
Olivais_fit3 <- Arima(Olivais_TS, order = c(1,0,1), 
                      seasonal = c(1,0,0), lambda = Olivais_lambda)

coeftest(Olivais_fit3)#z-test of estimated coefficients
checkresiduals(Olivais_fit3)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Olivais_fit3$residuals)

summary(Olivais_fit3) # AIC=65654.04   AICc=65654.05   BIC=65689.43 RMSE=8.244666 MAE=5.716403

autoplot(Olivais_TS)+autolayer(Olivais_fit1$fitted,series='Fit')#Fitted series against original one


Olivais_Prediction3<-forecast(Olivais_fit3, h=5, level=95)
print(Olivais_Prediction3$mean)#point prediction for the next five time points
print(Olivais_Prediction3$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Olivais_Prediction3$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Olivais_Prediction3, Olivais_Prediction3$upper[1], Olivais_Prediction3$lower[1])#prediction plot


#Model 4 without season differences

Olivais_fit4 <- Arima(Olivais_TS, order = c(1,0,1), 
                      seasonal = c(0,0,1), lambda = Olivais_lambda)

coeftest(Olivais_fit4)#z-test of estimated coefficients
checkresiduals(Olivais_fit4)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Olivais_fit4$residuals)

summary(Olivais_fit4) # AIC=65825.33   AICc=65825.33   BIC=65860.72 RMSE=8.325535 MAE=5.762994

autoplot(Olivais_TS)+autolayer(Olivais_fit1$fitted,series='Fit')#Fitted series against original one


Olivais_Prediction3<-forecast(Olivais_fit3, h=5, level=95)
print(Olivais_Prediction3$mean)#point prediction for the next five time points
print(Olivais_Prediction3$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Olivais_Prediction3$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Olivais_Prediction3, Olivais_Prediction3$upper[1], Olivais_Prediction3$lower[1])#prediction plot


#Model 5 without seasanol differences
Olivais_fit5 <- Arima(Olivais_TS, order = c(1,0,1), 
                      seasonal = c(2,0,1), lambda = Olivais_lambda)

coeftest(Olivais_fit5)#z-test of estimated coefficients
checkresiduals(Olivais_fit5)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Olivais_fit5$residuals)

summary(Olivais_fit5) # AIC=65825.33   AICc=65825.33   BIC=65860.72 RMSE=8.325535 MAE=5.762994

autoplot(Olivais_TS)+autolayer(Olivais_fit1$fitted,series='Fit')#Fitted series against original one


Olivais_Prediction5<-forecast(Olivais_fit5, h=5, level=95)
print(Olivais_Prediction5$mean)#point prediction for the next five time points
print(Olivais_Prediction5$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Olivais_Prediction5$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Olivais_Prediction5, Olivais_Prediction5$upper[1], Olivais_Prediction5$lower[1])#prediction plot



#Model 6 without seasonal differences
Olivais_fit6 <- Arima(Olivais_TS, order = c(1,0,1), 
                      seasonal = c(1,0,2), lambda = Olivais_lambda)

coeftest(Olivais_fit6)#z-test of estimated coefficients
checkresiduals(Olivais_fit6)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Olivais_fit6$residuals)

summary(Olivais_fit6) # AIC=64457.5   AICc=64457.52   BIC=64507.05 RMSE=7.687633 MAE=5.309933

autoplot(Olivais_TS)+autolayer(Olivais_fit1$fitted,series='Fit')#Fitted series against original one


Olivais_Prediction6<-forecast(Olivais_fit6, h=5, level=95)
print(Olivais_Prediction6$mean)#point prediction for the next five time points
print(Olivais_Prediction6$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Olivais_Prediction6$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Olivais_Prediction6, Olivais_Prediction6$upper[1], Olivais_Prediction6$lower[1])#prediction plot


########################################### Seasonal Diff
Olivais_BC_diffseasonal=diff(Olivais_BC,lag=24)
plot(Olivais_BC_diffseasonal)

ggtsdisplay(Olivais_BC_diffseasonal)


Best_aic<- auto.arima(Olivais_BC_diffseasonal, trace = TRUE, ic = "aic", d=0, D=1) #Best (1,0,1)(2,1,0)

Best_aicc <- auto.arima(Olivais_BC_diffseasonal, trace = TRUE, ic = "aicc", d = 0, D=1)

Best_BIC <- auto.arima(Olivais_BC_diffseasonal, trace = TRUE, ic = "bic", d = 0, D=1)

#Model 1 with seasonal differences, auto.arima model
Olivais_BC_fit1 <- Arima(Olivais_TS, order = c(1,0,1), 
                         seasonal = c(2,1,0), lambda = Olivais_lambda)

coeftest(Olivais_BC_fit1)#z-test of estimated coefficients
checkresiduals(Olivais_BC_fit1)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Olivais_BC_fit1$residuals)

summary(Olivais_BC_fit1) # AIC=65921.52   AICc=65921.52   BIC=65956.89 RMSE=8.285256 MAE=5.923435

autoplot(Olivais_TS)+autolayer(Olivais_BC_fit1$fitted,series='Fit')#Fitted series against original one


Olivais_Prediction_BC1<-forecast(Olivais_BC_fit1, h=5, level=95)
print(Olivais_Prediction_BC1$mean)#point prediction for the next five time points
print(Olivais_Prediction_BC1$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Olivais_Prediction_BC1$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Olivais_Prediction_BC1, Olivais_Prediction_BC1$upper[1], Olivais_Prediction_BC1$lower[1])#prediction plot




###############################
########## Reboleira ##########
###############################


# Time series
Reboleira_TS=ts(O3[,5], start=c(1, 1), end=c(365, 24), frequency=24)
plot(Reboleira_TS, main = "Reboleira", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu * m^2 / m^3))


# Box Cox transformation -> w = (y^lambda-1)/lambda
Reboleira_lambda=BoxCox.lambda(Reboleira_TS) #Calculates the best lambda
print(Reboleira_lambda)
Reboleira_BC=BoxCox(Reboleira_TS, lambda = Reboleira_lambda) #Makes the BOXCOX transformation
plot(Reboleira_BC, main = "Reboleira_BC", xlab = "", ylab = "") # no trend but seasonality

ggtsdisplay(Reboleira_BC)#Displays the time series, the ACF and PACF

# Model 1 without seasonal differences
Reboleira_fit1 <- Arima(Reboleira_TS, order = c(1,0,2), 
                        seasonal = c(2,0,0), lambda = Reboleira_lambda) # model_Sarima model(1,0,2)(2,0,0)

coeftest(Reboleira_fit1)#z-test of estimated coefficients
checkresiduals(Reboleira_fit1)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Reboleira_fit1$residuals)

summary(Reboleira_fit1) # AIC=63583.33   AICc=63583.34   BIC=63632.87   RMSE = 6.631063  MAE=4.528615

autoplot(Reboleira_TS)+autolayer(Reboleira_fit1$fitted,series='Fit')

Reboleira_Prediction1<-forecast(Reboleira_fit1, h=5, level=95)
print(Reboleira_Prediction1$mean)#point prediction for the next five time points
print(Reboleira_Prediction1$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Reboleira_Prediction1$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Reboleira_Prediction1, Reboleira_Prediction1$upper[1], Reboleira_Prediction1$lower[1])#prediction plot


#Model 2 without seasonal differences
Reboleira_fit2 <- Arima(Reboleira_TS, order = c(1,0,1), 
                        seasonal = c(1,0,1), lambda = Reboleira_lambda)

coeftest(Reboleira_fit2)#z-test of estimated coefficients
checkresiduals(Reboleira_fit2)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Reboleira_fit2$residuals)

summary(Reboleira_fit2) # AIC=62811.16   AICc=62811.17   BIC=62853.63 RMSE=6.335111 MAE=4.334792

autoplot(Reboleira_TS)+autolayer(Reboleira_fit1$fitted,series='Fit')#Fitted series against original one


Reboleira_Prediction2<-forecast(Reboleira_fit2, h=5, level=95)
print(Reboleira_Prediction2$mean)#point prediction for the next five time points
print(Reboleira_Prediction2$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Reboleira_Prediction2$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Reboleira_Prediction2, Reboleira_Prediction2$upper[1], Reboleira_Prediction2$lower[1])#prediction plot

#Model 3 without season differences
Reboleira_fit3 <- Arima(Reboleira_TS, order = c(1,0,1), 
                        seasonal = c(1,0,0), lambda = Reboleira_lambda)

coeftest(Reboleira_fit3)#z-test of estimated coefficients
checkresiduals(Reboleira_fit3)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Reboleira_fit3$residuals)

summary(Reboleira_fit3) # AIC=63767.22   AICc=63767.23   BIC=63802.61 RMSE=6.702778 MAE=4.57943

autoplot(Reboleira_TS)+autolayer(Reboleira_fit1$fitted,series='Fit')#Fitted series against original one


Reboleira_Prediction3<-forecast(Reboleira_fit3, h=5, level=95)
print(Reboleira_Prediction3$mean)#point prediction for the next five time points
print(Reboleira_Prediction3$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Reboleira_Prediction3$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Reboleira_Prediction3, Reboleira_Prediction3$upper[1], Reboleira_Prediction3$lower[1])#prediction plot


#Model 4 without season differences

Reboleira_fit4 <- Arima(Reboleira_TS, order = c(1,0,1), 
                        seasonal = c(0,0,1), lambda = Reboleira_lambda)

coeftest(Reboleira_fit4)#z-test of estimated coefficients
checkresiduals(Reboleira_fit4)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Reboleira_fit4$residuals)

summary(Reboleira_fit4) # AIC=63882.57   AICc=63882.58   BIC=63917.96 RMSE=6.750416 MAE=4.607075

autoplot(Reboleira_TS)+autolayer(Reboleira_fit1$fitted,series='Fit')#Fitted series against original one


Reboleira_Prediction3<-forecast(Reboleira_fit3, h=5, level=95)
print(Reboleira_Prediction3$mean)#point prediction for the next five time points
print(Reboleira_Prediction3$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Reboleira_Prediction3$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Reboleira_Prediction3, Reboleira_Prediction3$upper[1], Reboleira_Prediction3$lower[1])#prediction plot


#Model 5 without seasanol differences
Reboleira_fit5 <- Arima(Reboleira_TS, order = c(1,0,1), 
                        seasonal = c(2,0,1), lambda = Reboleira_lambda)

coeftest(Reboleira_fit5)#z-test of estimated coefficients
checkresiduals(Reboleira_fit5)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Reboleira_fit5$residuals)

summary(Reboleira_fit5) # AIC=62780.98   AICc=62780.99   BIC=62830.52 RMSE=6.321983 MAE=4.320024

autoplot(Reboleira_TS)+autolayer(Reboleira_fit1$fitted,series='Fit')#Fitted series against original one


Reboleira_Prediction5<-forecast(Reboleira_fit5, h=5, level=95)
print(Reboleira_Prediction5$mean)#point prediction for the next five time points
print(Reboleira_Prediction5$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Reboleira_Prediction5$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Reboleira_Prediction5, Reboleira_Prediction5$upper[1], Reboleira_Prediction5$lower[1])#prediction plot



#Model 6 without seasonal differences
Reboleira_fit6 <- Arima(Reboleira_TS, order = c(1,0,1), 
                        seasonal = c(1,0,2), lambda = Reboleira_lambda)

coeftest(Reboleira_fit6)#z-test of estimated coefficients
checkresiduals(Reboleira_fit6)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Reboleira_fit6$residuals)

summary(Reboleira_fit6) # AIC=62781.14   AICc=62781.16   BIC=62830.69 RMSE=6.3219 MAE=6.3219

autoplot(Reboleira_TS)+autolayer(Reboleira_fit1$fitted,series='Fit')#Fitted series against original one


Reboleira_Prediction6<-forecast(Reboleira_fit6, h=5, level=95)
print(Reboleira_Prediction6$mean)#point prediction for the next five time points
print(Reboleira_Prediction6$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Reboleira_Prediction6$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Reboleira_Prediction6, Reboleira_Prediction6$upper[1], Reboleira_Prediction6$lower[1])#prediction plot


########################################### Seasonal Diff
Reboleira_BC_diffseasonal=diff(Reboleira_BC,lag=24)
plot(Reboleira_BC_diffseasonal)

ggtsdisplay(Reboleira_BC_diffseasonal)


Best_aic<- auto.arima(Reboleira_BC_diffseasonal, trace = TRUE, ic = "aic", d=0, D=1) #Best (1,0,1)(2,1,0)

Best_aicc <- auto.arima(Reboleira_BC_diffseasonal, trace = TRUE, ic = "aicc", d = 0, D=1)

Best_BIC <- auto.arima(Reboleira_BC_diffseasonal, trace = TRUE, ic = "bic", d = 0, D=1)

#Model 1 with seasonal differences, auto.arima model
Reboleira_BC_fit1 <- Arima(Reboleira_TS, order = c(1,0,1), 
                           seasonal = c(2,1,0), lambda = Reboleira_lambda)

coeftest(Reboleira_BC_fit1)#z-test of estimated coefficients
checkresiduals(Reboleira_BC_fit1)#check if the residuals are white noise using Ljung-Box text
ggtsdisplay(Reboleira_BC_fit1$residuals)

summary(Reboleira_BC_fit1) # AIC=65921.52   AICc=65921.52   BIC=65956.89 RMSE=8.285256 MAE=5.923435

autoplot(Reboleira_TS)+autolayer(Reboleira_BC_fit1$fitted,series='Fit')#Fitted series against original one


Reboleira_Prediction_BC1<-forecast(Reboleira_BC_fit1, h=5, level=95)
print(Reboleira_Prediction_BC1$mean)#point prediction for the next five time points
print(Reboleira_Prediction_BC1$lower)#prediction of low bound of 95% confidence interval for the next five time points
print(Reboleira_Prediction_BC1$upper)#prediction of upper bound of 95% confidence interval for the next five time 
plot(Reboleira_Prediction_BC1, Reboleira_Prediction_BC1$upper[1], Reboleira_Prediction_BC1$lower[1])#prediction plot
