library(readxl)
library(ggplot2)
# Initial Data Examination

# load the data into a data frame
df <- read_excel('Data/2021 Alfragide-Amadora.xlsx')

# plot the time series

ggplot(df, aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Time Series 1 plot")
