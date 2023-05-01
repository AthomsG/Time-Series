library(readxl)
library(ggplot2)
library(imputeTS)

# get the list of Excel files in the directory
file_list <- paste("DATA/", list.files(path = "DATA", pattern = "\\.xlsx$"), sep = "")

# ===================================================
#                    EDA - Exploratory Data Analysis
# ===================================================

# loop through each file and create a plot
for (file in file_list) {
  # load data into data frame
  df <- read_excel(file)
  # interpolate missing values
  df <- na_interpolation(df, option = "linear")
  
  # get the name of x variable
  x_var <- names(df)[1]
  
  # convert x variable to date-time format
  df[[x_var]] <- as.POSIXct(df[[x_var]], format = "%Y-%m-%d %H:%M:%S")
  
  # plot time series
  plot <- ggplot(df, aes(x = !!sym(x_var), y = `Ozono (µg/m3)`)) +
    geom_line(color = "#0072B2",
              linewidth = 0.2) +  # line color
    labs(title = gsub("^DATA/2021\\s+(.*)\\.xlsx$", "\\1", file),
         x = "Date",
         y = expression(Ozono~(µg/m^{3}))) +  # axis labels with symbols
    scale_x_datetime(date_labels = "%Y-%m-%d", 
                     date_breaks = "2 month", 
                     date_minor_breaks = "15 day") +  # date format and breaks
    theme_minimal() + 
    theme(plot.title = element_text(size = 14),# face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.line = element_line(linewidth = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.5) +
    guides(color = FALSE)  # hide legend
  
  # save plot as a PDF file with the same name as Excel file
  file <- gsub("DATA", "PLOTS", file)
  suppressMessages(ggsave(gsub("\\.xlsx", ".pdf", file), plot))
  
  # plot histogram
  hist <- ggplot(df, aes(x = `Ozono (µg/m3)`)) +
    geom_histogram(binwidth = 2, fill = "#0072B2", color = "white") +
    labs(title = gsub("^PLOTS/2021\\s+(.*)\\.xlsx", "\\1", file),
         x = expression(Ozono~(µg/m^{3})),
         y = "Frequency") +
    scale_x_continuous(breaks = seq(0, 150, by = 25)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.line = element_line(linewidth = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.5)
  
  # save plot as a PDF file with the same name as Excel file
  suppressMessages(ggsave(gsub("\\.xlsx", "_hist.pdf", file), hist))
  
  print(gsub("^PLOTS/2021\\s+(.*)\\.xlsx", "\\1", file))
  print(summary(df$`Ozono (µg/m3)`))
}