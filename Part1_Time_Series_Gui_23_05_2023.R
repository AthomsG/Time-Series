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

########## Alfragide-Amadora ##########

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

#Best_aic<- auto.arima(Alfragide_BC, trace = TRUE, ic = "aic", stepwise = FALSE, approximation = FALSE, d=0) #Best ARIMA(5,1,0)(2,0,0)

#Best_aicc <- auto.arima(Alfragide_BC, trace = TRUE, ic = "aicc", stepwise = FALSE, approximation = FALSE, d = 0)

#Best_BIC <- auto.arima(Alfragide_BC, trace = TRUE, ic = "bic", stepwise = FALSE, approximation = FALSE, d = 0)
#cat("Alfragide orders:", Alfragide_fit$arma,"\n")


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
