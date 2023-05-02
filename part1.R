# data management
library(dplyr)
# analysis
library(readxl) # read excel
library(imputeTS) # missing value imputation
library(TSclust) # time series clustering
library(extremogram)
library(forecast)
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
#                    EDA - Exploratory Data Analysis
# ===================================================

# loop through each file and create a plot
for (file in file_list) {
  # load data into data frame
  df <- read_excel(file)
  # interpolate missing values
  df <- na_interpolation(df, option = "linear")
  # extract the time series data
  ts <- df$`Ozono (µg/m3)`
  # add to the list
  ts_data[[gsub("^DATA/2021\\s+(.*)\\.xlsx$", "\\1", file)]] <- ts
  
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
                     date_minor_breaks = "1 month") +  # date format and breaks
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

# ===================================================
#                       CLUSTERING
# ===================================================

ts_data <- as.data.frame(ts_data)

# Clusters
dist_ts <- TSclust::diss(SERIES = t(ts_data), METHOD = "DTWARP") 

# Complete Linkage
hc_c <- stats::hclust(dist_ts, method="complete") 
plot(hc_c)

hclus_c <- stats::cutree(hc_c, k = 5) %>%
  as.data.frame(.) %>%
  dplyr::rename(.,cluster_group = .) %>%
  tibble::rownames_to_column("type_col")

hcdata_c <- ggdendro::dendro_data(hc_c)
names_order_c <- hcdata_c$labels$label

p1_c <- hcdata_c %>%
  ggdendro::ggdendrogram(., rotate=TRUE, leaf_labels=FALSE) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p2_c <- ts_data %>%
  dplyr::mutate(index = df$Reboleira) %>%
  tidyr::gather(key = type_col, value = value, -index) %>%
  dplyr::full_join(., hclus_c, by = "type_col") %>% 
  mutate(type_col = factor(type_col, levels = rev(as.character(names_order_c)))) %>% 
  ggplot(aes(x = index,
             y = value, 
             color = factor(cluster_group))) +
  geom_line(color = "#0072B2",
            linewidth = 0.2) +
  facet_wrap(~type_col, 
             ncol = 1, 
             strip.position="left") + 
  labs(x = "Date", 
       y = expression(Ozono~(µg/m^{3}))) +
  scale_x_datetime(date_labels = "%B", 
                   date_breaks = "3 month", 
                   date_minor_breaks = "1 month") +  # date format and breaks
  theme_bw() + 
  theme(strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.line = element_line(linewidth = 1),
        panel.grid.major = element_line(color = "#DDDDDD"),
        legend.position = "none",
        plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text =  element_text(size = 10))

gp1_c<-ggplotGrob(p1_c)
gp2_c<-ggplotGrob(p2_c) 

cplot <- grid.arrange(gp2_c, gp1_c, ncol=2, widths=c(4,2))

ggsave("PLOTS/part1_cluster.pdf", cplot)