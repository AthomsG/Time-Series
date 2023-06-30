# data management
library(dplyr)
library(lubridate)
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

# get the list of Excel files in the directory
file_list <- paste("DATA/", list.files(path = "DATA", pattern = "\\.xlsx$"), sep = "")
# list that stores all time series
ts_data <- list()

# ===================================================
#                    Auxiliary Functions
# ===================================================

# Box Cox transformation -> w = (y^lambda-1)/lambda
BoxCox <- function(x, lambda) {
  if(lambda == 0) {
    return(log(x))
  } else {
    return((x^lambda-1)/lambda)
  }
}

# ===================================================
#                    EDA - Exploratory Data Analysis
# ===================================================

# loop through each file and create a plot
for (file in file_list) {
  # load data into data frame
  df <- read_excel(file)
  # count the missing values
  num_missing <- sum(is.na(df))
  # calculate the percentage
  percent_missing <- 100*num_missing/nrow(df)
  # Extract the name of the file without the path and extension
  filename <- gsub("^DATA/2021\\s+(.*)\\.xlsx$", "\\1", file)
  # print the result
  cat(paste("The percentage of missing values in", filename, "is", round(percent_missing, 2), "%.\n"))
  # interpolate missing values
  df <- na_interpolation(df, option="linear")
  # extract the time series data
  ts <- ts(df$`Ozono (µg/m3)`, start=c(1, 1), frequency=24, end=c(365, 24))
  # add to the list
  ts_data[[filename]] <- ts
  # get the name of x variable
  x_var <- names(df)[1]
  
  # convert x variable to date-time format
  df[[x_var]] <- as.POSIXct(df[[x_var]], format = "%Y-%m-%d %H:%M:%S")
  
  # plot time series
  plot <- ggplot(df, aes(x = !!sym(x_var), y = `Ozono (µg/m3)`)) +
    geom_line(color = "#0072B2",
              linewidth = 0.2) +  # line color
    labs(title = filename,
         x = "Measurement Date",
         y = expression(Ozone~(µg/m^{3}))) +  # axis labels with symbols
    scale_x_datetime(date_labels = "%B", 
                     date_breaks = "3 month", 
                     date_minor_breaks = "1 month") +  # date format and breaks
    theme_minimal() + 
    theme(plot.title = element_text(size = 23),# face = "bold"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          axis.line = element_line(linewidth = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.3) +
    guides(color = FALSE)  # hide legend
  
  # save plot as a PDF file with the same name as Excel file
  file <- gsub("DATA", "PLOTS", file)
  suppressMessages(ggsave(gsub("\\.xlsx", ".pdf", file), plot, width = 7, height = 3))
  
  # plot rolling mean and standard deviation
  roll_mean <- rollapply(ts, width = 200, FUN = mean, fill = NA, align = "right")
  roll_sd <- rollapply(ts, width = 200, FUN = sd, fill = NA, align = "right")
  df_ts <- data.frame(x = df[[x_var]], y = ts, mean = roll_mean, sd = roll_sd)
  
  plot_ts <- ggplot(df_ts, aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2) +
    geom_line(aes(y = mean, color = "Rolling Mean")) +
    labs(title = "Rolling Mean & Standard Deviation",
         x = "Measurement Date",
         y = expression(Ozone~(µg/m^{3})),
         subtitle = "Window width = 200 observations") +
    scale_color_manual(name = "Lines", 
                       values = c("Rolling Mean" = "#0072B2", "Original Time Series" = "red")) +
    scale_x_datetime(date_labels = "%B", 
                     date_breaks = "3 month", 
                     date_minor_breaks = "1 month") +
    theme_minimal() + 
    theme(plot.title = element_text(size = 20),# face = "bold"),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          axis.line = element_line(linewidth = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.4) +
    guides(color = FALSE) 
  
  # save plot as a PDF file with the same name as Excel file
  file_ts <- gsub("DATA", "PLOTS", file)
  suppressMessages(ggsave(gsub("\\.xlsx", "_sp.pdf", file_ts), plot_ts, width = 6, height = 3)) #statistical properties
  
  # Box-Cox Transform    ----->    FIQUEI AQUI
  guerrero_lambda=BoxCox.lambda(ts)
  df_BC=BoxCox(ts, lambda=guerrero_lambda)
  
  print(guerrero_lambda)
  
  df_BC<-data.frame(df[x_var], df_BC)

  plot <- ggplot(df_BC, aes(x = !!sym(names(df_BC)[1]), y = `df_BC`)) +
    geom_line(color = "#0072B2",
              linewidth = 0.2) +  # line color
    labs(title = filename,
         x = "Measurement Date",
         y = expression(Ozone~(µg/m^{3}))) +  # axis labels with symbols
    scale_x_datetime(date_labels = "%B", 
                     date_breaks = "3 month", 
                     date_minor_breaks = "1 month") +  # date format and breaks
    theme_minimal() + 
    theme(plot.title = element_text(size = 23),# face = "bold"),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 16),
          axis.line = element_line(linewidth = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.3) +
    guides(color = FALSE)  # hide legend
  
  # save plot as a PDF file with the same name as Excel file
  suppressMessages(ggsave(gsub("\\.xlsx", "_bc.pdf", file), plot, width = 7, height = 3))
  
  # plot histogram
  hist <- ggplot(df, aes(x = `Ozono (µg/m3)`)) +
    geom_histogram(binwidth = 2, fill = "#0072B2", color = "white") +
    labs(title = filename,
         x = expression(Ozone~(µg/m^{3})),
         y = "Frequency") +
    scale_x_continuous(breaks = seq(0, 150, by = 25)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 20),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          axis.line = element_line(linewidth = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.4)
  
  # save plot as a PDF file with the same name as Excel file
  suppressMessages(ggsave(gsub("\\.xlsx", "_hist.pdf", file), hist, width = 6, height = 3))
  
  print(gsub("^PLOTS/2021\\s+(.*)\\.xlsx", "\\1", file))
  print(summary(df$`Ozono (µg/m3)`))
}

# ===================================================
#                       CLUSTERING
# ===================================================

ts_data <- as.data.frame(ts_data)

# Compute dissimilarity matrix using Dynamic Time Warping method
dist_ts <- TSclust::diss(SERIES = t(ts_data), METHOD = "DTWARP") 

# Hierarchical clustering using complete linkage
hc_c <- stats::hclust(dist_ts, method="complete") 

# Convert the hclust object to dendro_data object for use with ggdendro
dendro_data <- ggdendro::dendro_data(hc_c)

# Create dendrogram plot
p1_c <- ggdendro::ggdendrogram(dendro_data, rotate = FALSE)

# Rotate x-axis text and increase font size
p1_c <- p1_c +
  theme(axis.text.x = element_text(angle = 90, size = 17, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        plot.margin = margin(12, 0, 12, 44, "pt"))  # Increased left margin to make room for y-axis title

# Add y-axis title
p1_c <- p1_c + 
  labs(y = "Dissimilarity") +
  theme(axis.title.y = element_text(size = 14, angle = 90, vjust = 10))

# Plot
p1_c
