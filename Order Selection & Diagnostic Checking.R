# BEST MODELS PARAMETERS

# ALFRAGIDE: (1,0,1)(1,1,1)
# BEATO: (2,0,1)(1,1,1)
# ENTRECAMPOS: (1,0,1)(1,1,1)
# OLIVAIS: (2,0,1)(1,1,1)
# REBOLEIRA: (1,0,1)(1,1,1)

# Forecast plot function
# Forecast plot function
forecast_plot <- function(forecast_obj, x, title = "", n_points = 10) {
  
  x_subset <- tail(x, n_points)
  
  forecast_df <- data.frame(
    Time = c(time(x_subset), time(forecast_obj$mean)), 
    Value = c(as.numeric(x_subset), as.numeric(forecast_obj$mean)),
    Type = c(rep("Historical", length(x_subset)), rep("Forecast", length(forecast_obj$mean))),
    Lo80 = c(rep(NA, length(x_subset)), as.numeric(forecast_obj$lower[,1])),
    Hi80 = c(rep(NA, length(x_subset)), as.numeric(forecast_obj$upper[,1]))
  )
  
  p <- ggplot(forecast_df, aes(x = Time, y = Value)) +
    geom_hline(yintercept = 0) +
    geom_line(aes(color = Type), size = 1.2) +
    geom_ribbon(aes(ymin = Lo80, ymax = Hi80), fill = "grey80", alpha = 0.3) +
    scale_color_manual(values = c("Historical" = "black", "Forecast" = "red")) +
    ggtitle(title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 24),  # Increased font size to 24
      legend.title = element_blank(),
      legend.position = "bottom",
      axis.title = element_text(size = 18),  # Increased font size to 18
      axis.text = element_text(size = 16),  # Increased font size to 16
      legend.text = element_text(size = 16)  # Increased font size to 16
    ) +
    ylab(expression(paste("Ozone (", mu, "g/m"^3, ")"))) + 
    xlab("Day")
  
  # Save plot as PDF with a filename based on the city name
  if (!is.null(title)) {
    city_name <- tolower(gsub(" Forecast", "", title))
    filename <- paste0(city_name, "_forecast.pdf")
    ggsave(filename, p, device = "pdf", width = 6, height = 5)
  }
  
  return(p)
}


##############################################################################################

# Olivais: PASS
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
#checkresiduals(Olivais_fit$residuals, lag = 8760/4)

# Forcast
olivais_forecast<-forecast(Olivais_fit, h=5, level=95)
forecast_plot(olivais_forecast, Olivais, title = "Olivais Forecast", n_points = 14)

# Coefficients
coef(Olivais_fit)

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

# Forcast
alfragide_forecast<-forecast(alfragide_fit, h=5, level=95)
forecast_plot(alfragide_forecast, alfragide, title = "Alfragide Forecast", n_points = 14)

# Coefficients
coef(Alfragide_fit)
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

# Forcast
beato_forecast<-forecast(beato_fit, h=5, level=95)
forecast_plot(beato_forecast, beato, title = "Beato Forecast", n_points = 14)

# Coefficients
coef(beato_fit)
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

# Forcast
entrecampos_forecast<-forecast(entrecampos_fit, h=5, level=95)
forecast_plot(entrecampos_forecast, entrecampos, title = "Entrecampos Forecast", n_points = 14)

# Coefficients
coef(entrecampos_fit)
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
#checkresiduals(Reboleira_fit$residuals, lag = 8760/4)

# Forcast
Reboleira_forecast<-forecast(Reboleira_fit, h=5, level=95)
forecast_plot(Reboleira_forecast, Reboleira, title = "Reboleira Forecast", n_points = 14)

# Coefficients
coef(Reboleira_fit)