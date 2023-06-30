# PLOT ACF AND PACF FUNCTION
save_residual_plots_as_pdf <- function(residual, filename, ymin=-0.5, ymax=0.81) {
  # Compute the ACF and PACF of the residuals up to lag 72*24
  acf_resid <- acf(residual, lag.max = 72 * 24, plot = FALSE)
  pacf_resid <- pacf(residual, lag.max = 72 * 24, plot = FALSE)
  
  # Exclude the lag-0 value -> correlation 1
  acf_resid$lag <- acf_resid$lag[-1]
  acf_resid$acf <- acf_resid$acf[-1]
  
  # Compute the confidence intervals
  conf_level <- qnorm((1 + 0.95) / 2) / sqrt(sum(!is.na(residual)))
  
  # Create a PDF file for the plots
  pdf(filename, width = 9, height = 5)
  
  par(mfrow = c(1, 2))  # Set the plot layout to one row and two columns
  
  # Create new plots with adjusted x-axis and y-axis range
  plot(acf_resid$lag * 24, acf_resid$acf, type = "h", xlab = "Lag", ylab = "ACF",
       ylim = c(ymin, ymax), xlim = c(0, 72), xaxt = 'n', cex.lab = 1.2, cex.axis = 1.2)
  axis(1, at = seq(0, 72, by = 12), labels = seq(0, 72, by = 12), cex.axis = 1.2)  # Add x-axis labels
  abline(h = 0, col = "black")  # Add a line at y = 0
  abline(h = c(-1, 1) * conf_level, lty = 2, col = "blue")  # Add confidence intervals
  abline(v = c(24, 48, 72), col = "grey", lty = 3)  # Add vertical grid lines
  title("ACF of Residuals", cex.main = 1.4)
  
  plot(pacf_resid$lag * 24, pacf_resid$acf, type = "h", xlab = "Lag", ylab = "PACF",
       ylim = c(ymin, ymax), xlim = c(0, 72), xaxt = 'n', cex.lab = 1.2, cex.axis = 1.2)
  axis(1, at = seq(0, 72, by = 12), labels = seq(0, 72, by = 12), cex.axis = 1.2)  # Add x-axis labels
  abline(h = 0, col = "black")  # Add a line at y = 0
  abline(h = c(-1, 1) * conf_level, lty = 2, col = "blue")  # Add confidence intervals
  abline(v = c(24, 48, 72), col = "grey", lty = 3)  # Add vertical grid lines
  title("PACF of Residuals", cex.main = 1.4)
  
  dev.off()
  
  par(mfrow = c(1, 1))  # Reset the plot layout to the default
}














# Assuming you have a time series data stored in a variable called 'ts_data' with missing values
ts_data$Olivais_interpolated <- na.approx(ts_data$Olivais)

# Plot the interpolated time series
plot(ts_data$Olivais[1:72], main = "Interpolated Time Series Plot", xlab = "Time", ylab = "Value")

lines(ts_data$Olivais[1:72])

# Add vertical dashed line at time=24
abline(v = 24, col = "red", lty = "dashed")
abline(v = 48, col = "red", lty = "dashed")

# PLOT DECOMPOSED SERIES

# Assuming you have a time series data stored in a variable called 'ts_data'
decomposed <- decompose(ts_data$Olivais)

# Access the decomposed components
trend <- decomposed$trend
seasonal <- decomposed$seasonal
residual <- na.omit(decomposed$random)

# Plot the decomposed components
par(mfrow = c(3, 1))  # Set the plot layout to three rows and one column
ts.plot(trend, main = "Trend Component", ylab = "Value")
plot(seasonal, main = "Seasonal Component", ylab = "Value")
ts.plot(residual, main = "Residual Component", ylab = "Value")

serie = ts_data$Olivais

decomposed <- decompose(serie)
residual <- decomposed$random

# Esquerdo Q e q pequeno
# Direito P grande e p pequeno
ggtsdisplay(residual)

# Como vejo que há picos perto da origem no ACF, aumento p
# Como vejo que há pico grande perto do PACF, aumento q
# Como vejo que há um picos com lag da sasonalidade ACF aumento P
# Como vejo que há picos com lag de sasonalidade PACF aumento Q

test_lambda = guerrero_lambda=BoxCox.lambda(serie)

# WITHOUT SEASONAL DIFFERENCING

# Model 1
# ordeer = (p, d, q)(P, D, Q)s
serie_fit1 <- Arima(serie, 
                    order = c(1,0,0), 
                    seasonal = c(1,0,0), 
                    lambda = test_lambda)

residual1 <- serie_fit1$residuals
save_residual_plots_as_pdf(residual1, "fit_1_residual_plots_olivais.pdf")

# Model 2
serie_fit2 <- Arima(serie, 
                    order = c(1,0,1), 
                    seasonal = c(1,0,0), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit2$residuals, "fit_2_residual_plots_olivais.pdf", -0.2, 0.3)

# Model 3
serie_fit3 <- Arima(serie, 
                    order = c(2,0,0), 
                    seasonal = c(1,0,0), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit3$residuals, "fit_3_residual_plots_olivais.pdf", -0.2, 0.3)

# Model 4
serie_fit4 <- Arima(serie, 
                    order = c(1,0,0), 
                    seasonal = c(2,0,0), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit4$residuals, "fit_4_residual_plots_olivais.pdf", -0.2, 0.3)

# Model 5
serie_fit5 <- Arima(serie, 
                    order = c(1,0,1), 
                    seasonal = c(1,0,1), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit5$residuals, "fit_5_residual_plots_olivais.pdf", -0.1, 0.1)

# Model 6
serie_fit6 <- Arima(serie, 
                    order = c(2,0,0), 
                    seasonal = c(1,0,1), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit6$residuals, "fit_6_residual_plots_olivais.pdf", -0.1, 0.1)

# Model 7
serie_fit1 <- Arima(serie, 
                    order = c(2,0,1), 
                    seasonal = c(1,0,2), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit1$residuals, "residual_plots_olivais_fit7.pdf", -0.1, 0.1)

# Decomposed Series Residuals with seasonal differencing
serie_fit1 <- Arima(serie, 
                    order = c(0,0,0), 
                    seasonal = c(0,1,0), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit1$residuals, "residual_plot_olivais_sdiff.pdf", -0.4, 1)

# Model 8
# order = (p, d, q)(P, D, Q)s
serie_fit8 <- Arima(serie, 
                    order = c(1,0,0), 
                    seasonal = c(1,1,0), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit8$residuals, "residual_plots_olivais_fit8.pdf", -0.35, 0.3)

# Model 9
serie_fit9 <- Arima(serie, 
                    order = c(1,0,1), 
                    seasonal = c(1,1,0), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit9$residuals, "residual_plots_olivais_fit9.pdf", -0.35, 0.3)

# Model 10
serie_fit10 <- Arima(serie, 
                    order = c(2,0,0), 
                    seasonal = c(1,1,0), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit10$residuals, "residual_plots_olivais_fit10.pdf", -0.35, 0.1)

# Model 11
serie_fit11 <- Arima(serie, 
                    order = c(1,0,0), 
                    seasonal = c(2,1,0), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit11$residuals, "residual_plots_olivais_fit11.pdf", -0.30, 0.17)

# Model 12
serie_fit12 <- Arima(serie, 
                    order = c(1,0,1), 
                    seasonal = c(1,1,1), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit12$residuals, "residual_plots_olivais_fit12.pdf", -0.05, 0.05)

# Model 13
serie_fit13 <- Arima(serie, 
                    order = c(2,0,0), 
                    seasonal = c(1,1,1), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit13$residuals, "residual_plots_olivais_fit13.pdf", -0.08, 0.05)

# Model 14
serie_fit14 <- Arima(serie, 
                    order = c(2,0,1), 
                    seasonal = c(1,1,2), 
                    lambda = test_lambda)

save_residual_plots_as_pdf(serie_fit14$residuals, "residual_plots_olivais_fit14.pdf", -0.05, 0.05)

