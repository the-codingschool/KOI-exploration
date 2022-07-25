# This is an R Script file for cleaning and processing the KOI dataset

# Load required libraries
library(dplyr)
library(ggplot2)

# Load dataset
koi_data <- read.csv("data/cumulative_koi_data.csv")
View(koi_data)




na_srad_koi_data <- filter(koi_data, !is.na(koi_srad))
View(na_srad_koi_data)

confirmed_koi <- filter(koi_data, koi_disposition == "CONFIRMED")
confirmed_koi2 <- filter(confirmed_koi, koi_pdisposition != "FALSE POSITIVE")
View(confirmed_koi2)

candidate_koi <- filter(koi_data, koi_disposition == "CANDIDATE")
View(candidate_koi)

false_koi <- filter(koi_data, koi_disposition == "FALSE POSITIVE")
false_koi2 <- filter (false_koi, koi_pdisposition != "CANDIDATE")
View(false_koi2)
