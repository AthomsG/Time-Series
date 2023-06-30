library(xts)
library(tseries)
library(readxl)
library(ggplot2)
library(MASS)
library(PerformanceAnalytics)


file_names <- c("ALTRI_SGPS.xls", "MOTA_ENGIL.xls", "REN.xls", "SONAE.xls", "THE_NAVIGATOR_COMP.xls")
company_names <- c("ALTRI_SGPS", "MOTA_ENGIL", "REN", "SONAE", "THE_NAVIGATOR_COMP")

# Initialize empty data frames
data_prices <- data.frame(matrix(ncol = length(company_names), nrow = 199))
lreturns_prices <- data.frame(matrix(ncol = length(company_names), nrow = 198))

# Set the column names
colnames(data_prices) <- company_names
colnames(lreturns_prices) <- company_names

# Read the data for each company
for (i in seq_along(file_names)) {
  data <- read_excel(file_names[i], skip = 3)
  reversed_data <- as.data.frame(lapply(data, rev))
  
  close_prices <- ts(reversed_data$Close, start = 1, end = 199)
  
  # Calculate log returns using diff() function
  log_returns <- ts(diff(log(close_prices)))
  
  # Add the data to the respective data frames
  data_prices[,i] <- close_prices
  lreturns_prices[,i] <- log_returns
  
  # Create data frames for plotting
  plot_data_prices <- data.frame(Time = 1:199, Close_price = data_prices[[company_names[i]]])
  plot_lreturns_prices <- data.frame(Time = 2:199, Log_returns = lreturns_prices[[company_names[i]]])
  
  # plot time series
  plot1 <- ggplot(plot_data_prices, aes(x = Time, y = Close_price)) +
    geom_line(color = "#0072B2",
              size = 0.2) +  # line color
    labs(title = paste("Close", company_names[i]),
         x = "Time",
         y = "Close_price") +  # axis labels with symbols
    theme_minimal() + 
    theme(plot.title = element_text(size = 23),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          axis.line = element_line(size = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.3) +
    guides(color = "none")
  
  file1 <- gsub("DATA", "PLOTS", paste("close",file_names[i]))
  suppressMessages(ggsave(gsub("\\.xls", ".pdf", file1), plot1, width = 7, height = 3))
  
  
  
  plot2 <- ggplot(plot_lreturns_prices, aes(x = Time, y = Log_returns)) +
    geom_line(color = "#0072B2",
              size = 0.2) +  # line color
    labs(title = paste("Log returns", company_names[i]),
         x = "Time",
         y = "Log_returns") +  # axis labels with symbols
    theme_minimal() + 
    theme(plot.title = element_text(size = 23),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          axis.line = element_line(size = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.3) +
    guides(color = "none")
  
  file2 <- gsub("DATA", "PLOTS", paste("log_returns",file_names[i]))
  suppressMessages(ggsave(gsub("\\.xls", ".pdf", file2), plot2, width = 7, height = 3))
  
 
  
}

########Cluster analysis######

# Compute the dissimilarity matrix using DTWARP method
dist_ts_logret <- TSclust::diss(SERIES = t(lreturns_prices), METHOD = "DTWARP")

# Perform hierarchical clustering using complete linkage
hc_c_logret <- stats::hclust(dist_ts_logret, method = "complete")

# Save dendrogram to PDF
pdf("dendrogram_complete.pdf")
plot(hc_c_logret, main = "Dendrogram - Complete Linkage", xlab = "Companies", ylab = "Distance")
dev.off()

# Perform hierarchical clustering using average linkage
hc_a_logret <- stats::hclust(dist_ts_logret, method = "average")

# Save dendrogram to PDF
pdf("dendrogram_average.pdf")
plot(hc_a_logret, main = "Dendrogram - Average Linkage", xlab = "Companies", ylab = "Distance")
dev.off()





##selection of assumed distribution##
for (i in seq_along(file_names)) {
  
  # Generate histogram without normal overlay
  file3 <- gsub("DATA", "PLOTS", paste("histogram_n", file_names[i]))
  file3 <- gsub("\\.xls", ".pdf", file3)
  pdf(file3, width = 5, height = 5)
  chart.Histogram(lreturns_prices[, i], 
                  main = paste("Histogram of log returns:", company_names[i]), 
                  xlab = 'Log Returns',
                  method = 'add.density', 
                  colorset = c('cornflowerblue', 'red'), 
                  mgp = c(2, 0.5, 0))
  legend("topleft", 
         legend = c("frequency", "empirical distribution"), 
         fill = c('cornflowerblue', 'red'))
  dev.off()
  
  # Generate boxplot
  file_boxplot <- gsub("DATA", "PLOTS", paste("boxplot", file_names[i]))
  file_boxplot <- gsub("\\.xls", ".pdf", file_boxplot)
  pdf(file_boxplot, width = 5, height = 5)  # Adjust width and height as desired
  boxplot(lreturns_prices[, i], 
          main = paste("Boxplot of log returns:", company_names[i]), 
          ylab = 'Log Returns',
          col = 'cornflowerblue')
  dev.off()
}

##Model fitting###
library(rugarch)

# models
models <- c('sGARCH', 'iGARCH', 'gjrGARCH', 'eGARCH', 'apARCH', 'csGARCH')

# Loop over each company and each model with assumed Student's t-distribution
for (i in seq_along(company_names)) {
  for (model in models) {
    
    # Extract the log returns
    log_returns <- lreturns_prices[[company_names[i]]]
    
    # Select distribution model based on the company name
    if (company_names[i] == "ALTRI_SGPS") {
      dist_model <- "std"  # Standard t-distribution for ALTIR
    } else {
      dist_model <- "sstd"  # Skewed t-distribution for other companies
    }
    
    # Specify the model
    spec <- ugarchspec(variance.model = list(model = model, garchOrder = c(1, 1)),
                       mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                       distribution.model = dist_model)
    
    # Fit the model
    fit <- ugarchfit(spec, log_returns)
    
    # Write the results to a file
    file_name <- paste('output_', company_names[i], '_', model, '.txt', sep = '')
    capture.output(print(show(fit)), file = file_name)
  }
}




library(readxl)
library(forecast)
library(ggplot2)


for (i in seq_along(file_names)) {
  
  # Plot ACF
  acf_plot <- ggAcf(lreturns_prices[[company_names[i]]], main = paste("ACF of", company_names[i]))
  ggsave(filename = paste0("ACF_", company_names[i], ".pdf"), plot = acf_plot, device = "pdf")
  
  # Plot PACF
  pacf_plot <- ggPacf(lreturns_prices[[company_names[i]]], main = paste("PACF of", company_names[i]))
  ggsave(filename = paste0("PACF_", company_names[i], ".pdf"), plot = pacf_plot, device = "pdf")
  
}





  
  




