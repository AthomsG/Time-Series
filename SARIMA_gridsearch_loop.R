# data management
library(dplyr)
# analysis
library(readxl) # read excel
library(imputeTS) # missing value imputation
library(TSclust) # time series clustering
library(extremogram)
library(forecast)
library(zoo)
library(psych)
# graphics
library(ggplot2) # plots
library(ggdendro) # dendrograms
library(gtable) # plot organisation
library(grid) # plot organisation
library(gridExtra) # plot organisation

# This script uses a 'grid search' like approach to SARIMA parameter selection.

# GET TIME SERIES
file_list <- paste("DATA/", list.files(path = "DATA", pattern = "\\.xlsx$"), sep = "")
# list that stores all time series
ts_data <- list()

for (file in file_list) {
  # load data into data frame
  df <- read_excel(file)
  # Extract the name of the file without the path and extension
  filename <- gsub("^DATA/2021\\s+(.*)\\.xlsx$", "\\1", file)
  
  # interpolate missing values
  df <- na_interpolation(df, option = "linear")
  # extract the time series data
  ts <- ts(df$`Ozono (µg/m3)`, start = c(1, 1), frequency = 24, end = c(365, 24))
  # add to the list
  ts_data[[filename]] <- ts
}

# SARIMA FIT LOOP
model_results <- list()

fit_model <- function(order, seasonal_order, model_name) {
  tryCatch({
    serie_fit <- Arima(serie,
                       order = order,
                       seasonal = seasonal_order,
                       lambda = test_lambda)
    
    print(paste("Success in", model_name, "for", city))
    
    # Return the model results
    list(
      model = model_name,
      AIC = AIC(serie_fit),
      BIC = BIC(serie_fit)
    )
    
  }, error = function(e) {
    print(paste("Error in", model_name, "for", city))
    # Return an empty list in case of an error
    list()
  })
}

# Model configurations
models <- list(
  # No Seasonal Differencing
  list(order = c(1,0,0), seasonal_order = c(0,0,0), model_name = "Model 1"),
  list(order = c(1,0,0), seasonal_order = c(1,0,0), model_name = "Model 2"),
  list(order = c(1,0,0), seasonal_order = c(0,0,1), model_name = "Model 3"),
  list(order = c(1,0,0), seasonal_order = c(1,0,1), model_name = "Model 4"),
  
  list(order = c(1,0,1), seasonal_order = c(0,0,0), model_name = "Model 5"),
  list(order = c(1,0,1), seasonal_order = c(1,0,0), model_name = "Model 6"),
  list(order = c(1,0,1), seasonal_order = c(0,0,1), model_name = "Model 7"),
  list(order = c(1,0,1), seasonal_order = c(1,0,1), model_name = "Model 8"),
  
  list(order = c(2,0,0), seasonal_order = c(0,0,0), model_name = "Model 9"),
  list(order = c(2,0,0), seasonal_order = c(1,0,0), model_name = "Model 10"),
  list(order = c(2,0,0), seasonal_order = c(0,0,1), model_name = "Model 11"),
  list(order = c(2,0,0), seasonal_order = c(1,0,1), model_name = "Model 12"),
  
  list(order = c(2,0,1), seasonal_order = c(0,0,0), model_name = "Model 13"),
  list(order = c(2,0,1), seasonal_order = c(1,0,0), model_name = "Model 14"),
  list(order = c(2,0,1), seasonal_order = c(0,0,1), model_name = "Model 15"),
  list(order = c(2,0,1), seasonal_order = c(1,0,1), model_name = "Model 16"),
  
  # Seasonal Differencing
  list(order = c(1,0,0), seasonal_order = c(0,1,0), model_name = "Model 17"),
  list(order = c(1,0,0), seasonal_order = c(1,1,0), model_name = "Model 18"),
  list(order = c(1,0,0), seasonal_order = c(0,1,1), model_name = "Model 19"),
  list(order = c(1,0,0), seasonal_order = c(1,1,1), model_name = "Model 20"),
  
  list(order = c(1,0,1), seasonal_order = c(0,1,0), model_name = "Model 21"),
  list(order = c(1,0,1), seasonal_order = c(1,1,0), model_name = "Model 22"),
  list(order = c(1,0,1), seasonal_order = c(0,1,1), model_name = "Model 23"),
  list(order = c(1,0,1), seasonal_order = c(1,1,1), model_name = "Model 24"),
  
  list(order = c(2,0,0), seasonal_order = c(0,1,0), model_name = "Model 25"),
  list(order = c(2,0,0), seasonal_order = c(1,1,0), model_name = "Model 26"),
  list(order = c(2,0,0), seasonal_order = c(0,1,1), model_name = "Model 27"),
  list(order = c(2,0,0), seasonal_order = c(1,1,1), model_name = "Model 28"),
  
  list(order = c(2,0,1), seasonal_order = c(0,1,0), model_name = "Model 29"),
  list(order = c(2,0,1), seasonal_order = c(1,1,0), model_name = "Model 30"),
  list(order = c(2,0,1), seasonal_order = c(0,1,1), model_name = "Model 31"),
  list(order = c(2,0,1), seasonal_order = c(1,1,1), model_name = "Model 32"),
  
  
  list(order = c(1,0,1), seasonal_order = c(0,1,2), model_name = "Model 33"),
  list(order = c(2,0,0), seasonal_order = c(0,1,2), model_name = "Model 34")
)

# Specify the file path and name
file_path <- "SARIMA_model_results.txt"

# Open the file in write mode and save the model results
file_conn <- file(file_path, "w")

previous_city <- NULL  # Variable to track the previous city

for (city in names(ts_data)) {
  # Check if the city has changed
  if (!is.null(previous_city) && previous_city != city) {
    cat("\n", file = file_conn)
  }
  
  cat("City:", city, "\n", file = file_conn)  # Write the city name to file
  
  serie <- ts_data[[city]]
  test_lambda <- BoxCox.lambda(serie)
  
  model_results[[city]] <- lapply(models, function(model) {
    fit_model(model$order, model$seasonal_order, model$model_name)
  })
  
  model_list <- model_results[[city]]
  for (model_result in model_list) {
    cat("Model:", model_result$model, "\n", file = file_conn)  # Write the model name to file
    cat("AIC:", model_result$AIC, "\n", file = file_conn)  # Write the AIC to file
    cat("BIC:", model_result$BIC, "\n", file = file_conn)  # Write the BIC to file
    cat("\n", file = file_conn)  # Add an empty line
  }
  
  previous_city <- city  # Update the previous city
}

close(file_conn)  # Close the file connection

# Confirmation message
cat("Model results saved to", file_path, "\n")