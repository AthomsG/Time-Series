# BEST MODELS PARAMETERS

# ALFRAGIDE: (1,0,1)(1,1,1)
# BEATO: (2,0,1)(1,1,1)
# ENTRECAMPOS: (1,0,1)(1,1,1)
# OLIVAIS: (2,0,1)(1,1,1)
# REBOLEIRA: (1,0,1)(1,1,1)

##############################################################################################

# Olivais:
Olivais = ts_data$Olivais
Olivais_lambda = guerrero_lambda=BoxCox.lambda(Olivais)

# Fit best model
Olivais_fit <- Arima(Olivais, 
                       order = c(2,0,1), 
                       seasonal = c(1,1,1), 
                       lambda = Olivais_lambda)

# Compute the Ljung-Box test statistic
ljung_box_test_Olivais <- Box.test(Olivais_fit$residuals, lag = 8760/4, type = "Ljung-Box")
ljung_box_test_Olivais
checkresiduals(Olivais_fit$residuals, lag = 8760/4)

##############################################################################################

# Alfragide:
alfragide = ts_data$`Alfragide-Amadora`
alfragide_lambda = guerrero_lambda=BoxCox.lambda(alfragide)

# Fit best model
alfragide_fit <- Arima(alfragide, 
                    order = c(1,0,1), 
                    seasonal = c(1,1,1), 
                    lambda = alfragide_lambda)

# Compute the Ljung-Box test statistic
ljung_box_test_alfragide <- Box.test(alfragide_fit$residuals, lag = 8760/4, type = "Ljung-Box")
ljung_box_test_alfragide
#checkresiduals(alfragide_fit$residuals, lag = 8760/4)

##############################################################################################

# Beato:
beato = ts_data$Beato
beato_lambda = guerrero_lambda=BoxCox.lambda(beato)

# Fit best model
beato_fit <- Arima(beato, 
                       order = c(2,0,1), 
                       seasonal = c(1,1,1), 
                       lambda = beato_lambda)

# Compute the Ljung-Box test statistic
ljung_box_test_beato <- Box.test(beato_fit$residuals, lag = 8760/4, type = "Ljung-Box")
ljung_box_test_beato
#checkresiduals(beato_fit$residuals, lag = 8760/4)

##############################################################################################

# Entrecampos:
entrecampos = ts_data$Entrecampos
entrecampos_lambda = guerrero_lambda=BoxCox.lambda(entrecampos)

# Fit best model
entrecampos_fit <- Arima(entrecampos, 
                   order = c(1,0,1), 
                   seasonal = c(1,1,1), 
                   lambda = entrecampos_lambda)

# Compute the Ljung-Box test statistic
ljung_box_test_entrecampos <- Box.test(entrecampos_fit$residuals, lag = 8760/4, type = "Ljung-Box")
ljung_box_test_entrecampos
#checkresiduals(entrecampos_fit$residuals, lag = 8760/4)

##############################################################################################

# Reboleira:
Reboleira = ts_data$Reboleira
Reboleira_lambda = guerrero_lambda=BoxCox.lambda(Reboleira)

# Fit best model
Reboleira_fit <- Arima(Reboleira, 
                         order = c(1,0,1), 
                         seasonal = c(1,1,1), 
                         lambda = Reboleira_lambda) # FAILS

Reboleira_fit <- Arima(Reboleira, 
                       order = c(2,0,1), 
                       seasonal = c(1,1,2),
                       lambda = Reboleira_lambda) # COMPLEX MODEL FAILS

# Compute the Ljung-Box test statistic
ljung_box_test_Reboleira <- Box.test(Reboleira_fit$residuals, lag = 8760/4, type = "Ljung-Box")
ljung_box_test_Reboleira
checkresiduals(Reboleira_fit$residuals, lag = 8760/4)
