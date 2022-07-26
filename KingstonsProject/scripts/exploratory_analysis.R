# Load required libraries
library(dplyr)
library(ggplot2)

# Load dataset
koi_data <- read.csv("data/cumulative_koi_data.csv")
View(koi_data)



## Tried to sort by date but name doesn't actually correspond to date discovered
#name_koi_data <- arrange(koi_data, desc(kepoi_name))
#View(name_koi_data)



ggplot(clean_koi_data, aes(x = koi_pdisposition, y = koi_score, color = koi_disposition)) +
  geom_boxplot() +
  labs(x = "KOI Exoplanet Archive disposition",
       y = "Disposition score",
       title = "Mean disposition scores across Exoplanet Archive dispositions and Kepler data dispositions")
### There is 1 KOI with a Kepler data disposition of CANDIDATE but an archive disposition of FALSE POSITIVE
### There are also a few KOIs with a Kepler data disposition of FALSE POSITIVE but an archive disposition of CONFIRMED


ggplot(na_srad_koi_data, aes(x = koi_prad, y = koi_srad, color = koi_disposition)) +
  geom_point() +
  labs(x = "Planetary radius (Earth radii)",
       y = "Stellar radius (Solar radii)")

## Multiplying stellar radius by 109 to get approximately what it'd be like in Earth radii
ggplot(na_srad_koi_data, aes(x = koi_prad, y = koi_srad*109, color = koi_disposition)) +
  geom_point() +
  labs(x = "Planetary radius (Earth radii)",
       y = "Stellar radius (Earth radii)")



## How does transit duration differ for different dispositions and disposition scores?
ggplot(candidate_koi, aes(x = koi_score, y = koi_duration)) +
  geom_point(stat = "summary",
           fun = "mean",
           color = "blue") +
  labs(title = 'Average transit duration of "CANDIDATE" KOIs by disposition score',
       x = "Disposition score",
       y = "Transit duration (hours)")

ggplot(confirmed_koi2, aes(x = koi_score, y = koi_duration)) +
  geom_point(stat = "summary",
           fun = "mean",
           color = "springgreen3") +
  labs(title = 'Average transit duration of "CONFIRMED" KOIs by disposition score',
       x = "Disposition score",
       y = "Transit duration (hours)")

ggplot(false_koi2, aes(x = koi_score, y = koi_duration)) +
  geom_point(stat = "summary",
           fun = "mean",
           color = "red") +
  labs(title = 'Average transit duration of "FALSE POSITIVE" KOIs by disposition score',
       x = "Disposition score",
       y = "Transit duration (hours)")

## Generally how long is transit duration for different dispositions?
ggplot(candidate_koi, aes(x = koi_duration)) +
  geom_histogram(fill = "blue")

ggplot(confirmed_koi2, aes(x = koi_duration)) +
  geom_histogram(fill = "springgreen3")

ggplot(false_koi2, aes(x = koi_duration)) +
  geom_histogram(fill = "red")

ggplot(koi_data, aes(x = koi_duration)) +
  geom_histogram(aes(fill = koi_disposition)) +
  labs(x = "Transit duration (hours)")
### Colors don't match up to above graphs for this one which makes it a bit confusing

ggplot(koi_data, aes(x = koi_disposition, y = koi_duration)) +
  geom_histogram(stat = "summary",
                 fun = "mean",
                 fill = "thistle")








