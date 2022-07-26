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


ggplot(na_koi_data, aes(x = koi_prad, y = koi_srad, color = koi_disposition)) +
  geom_point() +
  labs(x = "Planetary radius (Earth radii)",
       y = "Stellar radius (Solar radii)")

## Multiplying stellar radius by 109 to get approximately what it'd be like in Earth radii
ggplot(na_koi_data, aes(x = koi_prad, y = koi_srad*109, color = koi_disposition)) +
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





# RESEARCH QUESTION: How might the orbital periods of planets be affected differently
# by the planetary radius and stellar surface gravity depending on its disposition?

## How is orbital period correlated to planetary radius and stellar surface gravity
ggplot(na_koi_data, aes(x = koi_period, y = koi_prad)) +
  geom_point()
cor(na_koi_data$koi_period, na_koi_data$koi_prad)
### Correlation is very low; about 0.51%

ggplot(na_koi_data, aes(x = koi_period, y = koi_slogg)) +
  geom_point()
cor(na_koi_data$koi_period, na_koi_data$koi_slogg)
### Correlation is also very low; about 0.19%
#### This could be in part due to the orbital period outlier (second highest value is 2190.7010, highest value is 129995.7784)


outlier_period_koi_data <- filter(na_koi_data, koi_period < 2200)

ggplot(outlier_period_koi_data, aes(x = koi_period, y = koi_prad)) +
  geom_point()
cor(outlier_period_koi_data$koi_period, outlier_period_koi_data$koi_prad)
### Raised correlation to 5.86%, still quite low

ggplot(outlier_period_koi_data, aes(x = koi_period, y = koi_slogg)) +
  geom_point()
cor(outlier_period_koi_data$koi_period, outlier_period_koi_data$koi_slogg)
### Raised correlation to -4.72%, also quite low



## How does average orbital period differ for different dispositions
ggplot(na_koi_data, aes(x = koi_disposition, y = koi_period)) +
  geom_bar(stat = "summary",
           fun = "mean")

## Comparing orbital period by planetary radius and stellar surface gravity for different dispositions
ggplot(candidate_koi, aes(x = koi_period, y = koi_prad)) +
  geom_point()
ggplot(confirmed_koi2, aes(x = koi_period, y = koi_prad)) +
  geom_point()
ggplot(false_koi2, aes(x = koi_period, y = koi_prad)) +
  geom_point()

ggplot(candidate_koi, aes(x = koi_period, y = koi_slogg)) +
  geom_point()
ggplot(confirmed_koi2, aes(x = koi_period, y = koi_slogg)) +
  geom_point()
ggplot(false_koi2, aes(x = koi_period, y = koi_slogg)) +
  geom_point()


