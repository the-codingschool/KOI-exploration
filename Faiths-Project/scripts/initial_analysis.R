library(readr)
library(ggplot2)
library(dplyr)
cumulative_koi_data <- read_csv("data/cumulative_koi_data.csv")
View(cumulative_koi_data)

#Rename
koi_data <- cumulative_koi_data

#Sample data
koi_data.sample <- sample_n(koi_data, 100, replace = TRUE)


ggplot(data = koi_data.sample, aes(x = koi_period, y = koi_duration), 
       fill = koi_disposition) +
  geom_point()

