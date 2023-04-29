library(readxl)
library(ggplot2)

# get the list of Excel files in the directory
file_list <- paste("DATA/", list.files(path = "DATA", pattern = "\\.xlsx$"), sep = "")

# loop through each file and create a plot
for (file in file_list) {
  # load the data into a data frame
  df <- read_excel(file)
  
  # get the name of the x variable
  x_var <- names(df)[1]
  
  # convert the x variable to date-time format
  df[[x_var]] <- as.POSIXct(df[[x_var]], format = "%Y-%m-%d %H:%M:%S")
  
  # plot the time series with custom theme and color palette
  plot <- ggplot(df, aes(x = !!sym(x_var), y = `Ozono (µg/m3)`)) +
    geom_line(color = "#0072B2",
              linewidth = 0.2) +  # custom line color
    labs(title = gsub("^DATA/2021\\s+(.*)\\.xlsx$", "\\1", file),
         x = "Date",
         y = expression(Ozono~(µg/m^{3}))) +  # custom axis labels with symbols
    scale_x_datetime(date_labels = "%Y-%m-%d", 
                     date_breaks = "2 month", 
                     date_minor_breaks = "15 day") +  # custom date format and breaks
    theme_minimal() +  # custom theme
    theme(plot.title = element_text(size = 14),# face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.line = element_line(linewidth = 1),
          panel.grid.major = element_line(color = "#DDDDDD"),
          aspect.ratio = 0.3) +
    guides(color = FALSE)  # hide legend
  
  # save the plot as a PDF file with the same name as the Excel file
  file <- gsub("DATA", "PLOTS", file)
  ggsave(gsub("\\.xlsx", ".pdf", file), plot)
}

