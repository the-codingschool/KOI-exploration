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
saveRDS(na_koi_data, "KingstonsProject/data/na_koi_data.RDS")

## Filtering dataset into KOI dispositions of "CONFIRMED", "CANDIDATE", and "FALSE POSITIVE"s only
# There is 1 KOI with a Kepler data disposition of CANDIDATE but an archive disposition of FALSE POSITIVE
# There are also a few KOIs with a Kepler data disposition of FALSE POSITIVE but an archive disposition of CONFIRMED
confirmed_koi <- filter(clean_koi_data, koi_disposition == "CONFIRMED")
confirmed_koi2 <- filter(confirmed_koi, koi_pdisposition != "FALSE POSITIVE")
na_confirmed_koi <- filter(confirmed_koi2, !is.na(koi_srad))
saveRDS(na_confirmed_koi, "KingstonsProject/data/na_confirmed_koi.RDS")

candidate_koi <- filter(clean_koi_data, koi_disposition == "CANDIDATE")
na_candidate_koi <- filter(candidate_koi, !is.na(koi_srad))
saveRDS(na_candidate_koi, "KingstonsProject/data/na_candidate_koi.RDS")

false_koi <- filter(clean_koi_data, koi_disposition == "FALSE POSITIVE")
false_koi2 <- filter (false_koi, koi_pdisposition != "CANDIDATE")
na_false_koi <- filter(false_koi2, !is.na(koi_srad))
saveRDS(na_false_koi, "KingstonsProject/data/na_false_koi.RDS")

## Arranging na_koi_data by koi_disposition
sorted_na_koi_data <-  arrange(na_koi_data, koi_disposition)

