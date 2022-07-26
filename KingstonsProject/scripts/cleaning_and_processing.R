# This is an R Script file for cleaning and processing the KOI dataset

# Load required libraries
library(dplyr)
library(ggplot2)

# Load dataset
koi_data <- read.csv("data/cumulative_koi_data.csv")
View(koi_data)


## Removing all err columns
clean_koi_data <- select(koi_data, !contains("err"))


## Filtering dataset to remove all rows with NA values in the srad column, which also happens to be the 
## same rows that most other information columns have NA values on.
na_koi_data <- filter(clean_koi_data, !is.na(koi_srad))


## Filtering dataset into KOI dispositions of "CONFIRMED", "CANDIDATE", and "FALSE POSITIVE"s only
### There is 1 KOI with a Kepler data disposition of CANDIDATE but an archive disposition of FALSE POSITIVE
### There are also a few KOIs with a Kepler data disposition of FALSE POSITIVE but an archive disposition of CONFIRMED
confirmed_koi <- filter(clean_koi_data, koi_disposition == "CONFIRMED")
confirmed_koi2 <- filter(confirmed_koi, koi_pdisposition != "FALSE POSITIVE")

candidate_koi <- filter(clean_koi_data, koi_disposition == "CANDIDATE")

false_koi <- filter(clean_koi_data, koi_disposition == "FALSE POSITIVE")
false_koi2 <- filter (false_koi, koi_pdisposition != "CANDIDATE")



