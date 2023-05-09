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
QualidadeARO3<-as.data.frame(Quantidade_Ozono)
QualidadeARO3 <- na_interpolation(QualidadeARO3, option = "linear")
summary(QualidadeARO3)
describe(QualidadeARO3)


par(mfrow=c(1,1))

########## Alfragide-Amadora ##########

# Missing values
statsNA(QualidadeARO3[,1])
ggplot_na_distribution(QualidadeARO3[,1])

# Time series
Alfragide_TS=ts(QualidadeARO3[,1], start=c(1, 1), end=c(365, 24), frequency=24)
plot(Alfragide_TS, main = "Alfragide", xlab = "Day", mgp=c(1.5,0.5,0), ylab = ~ "Ozone values " (mu * m^2 / m^3)) #non-constant variability

# Box Cox transformation -> w = (y^lambda-1)/lambda
Alfragide_lambda=BoxCox.lambda(Alfragide_TS) #Calculates the best lambda
print(Alfragide_lambda)
Alfragide_BC=BoxCox(Alfragide_TS, lambda = Alfragide_lambda) #Makes the BOXCOX transformation
plot(Alfragide_BC, main = "Alfragide_BC", xlab = "", ylab = "") # no trend but seasonality


ggtsdisplay(Alfragide_BC)#Displays the time series, the ACF and PACF

Best_aic<- auto.arima(Alfragide_BC, trace = TRUE, ic = "aic", stepwise = FALSE, approximation = FALSE, d=0) #Best ARIMA(5,1,0)(2,0,0)

Best_aicc <- auto.arima(Alfragide_BC, trace = TRUE, ic = "aicc", stepwise = FALSE, approximation = FALSE, d = 0)

Best_BIC <- auto.arima(Alfragide_BC, trace = TRUE, ic = "bic", stepwise = FALSE, approximation = FALSE, d = 0)
cat("Alfragide orders:", Alfragide_fit$arma,"\n")


# Model
Alfragide_fit1 <- Arima(Alfragide_TS, order = c(1,0,2), 
                          seasonal = c(2,0,0), lambda = Alfragide_lambda)

coeftest(Alfragide_fit1)
checkresiduals(Alfragide_fit1)
ggtsdisplay(Alfragide_fit1$residuals)

summary(Alfragide_fit1) # AIC=44134.2   AICc=44134.21   BIC=44169.6

autoplot(Alfragide_TS)+autolayer(Alfragide_fit1$fitted,series='Fit')

Alfragide_Prediction<-forecast(Alfragide_fit1, h=5, level=95)
print(Alfragide_Prediction$mean)
print(Alfragide_Prediction$lower)
print(Alfragide_Prediction$upper)
plot(Alfragide_Prediction, Alfragide_Prediction$upper, Alfragide_Prediction$lower)


Alfragide_fit2 <- Arima(Alfragide_TS, order = c(1,0,1), 
                          seasonal = c(1,1,1), lambda = Alfragide_lambda)

coeftest(Alfragide_fit2)
checkresiduals(Alfragide_fit2)
ggtsdisplay(Alfragide_fit2$residuals)

summary(Alfragide_fit2) # AIC=44104.24   AICc=44104.25   BIC=44139.65


########################################### Seasonal Diff
Alfragide_BC_diffseasonal=diff(Alfragide_BC,lag=24)
plot(Alfragide_BC_diffseasonal)

ggtsdisplay(Alfragide_BC_diffseasonal)


Best_aic<- auto.arima(Alfragide_BC_diffseasonal, trace = TRUE, ic = "aic", stepwise = FALSE, approximation = FALSE, d=0) #Best ARIMA(5,1,0)(2,0,0)

Best_aicc <- auto.arima(Alfragide_BC_diffseasonal, trace = TRUE, ic = "aicc", stepwise = FALSE, approximation = FALSE, d = 0)

Best_BIC <- auto.arima(Alfragide_BC_diffseasonal, trace = TRUE, ic = "bic", stepwise = FALSE, approximation = FALSE, d = 0)

