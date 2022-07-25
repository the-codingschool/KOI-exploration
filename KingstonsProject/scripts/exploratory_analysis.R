# Load required libraries
library(dplyr)
library(ggplot2)

# Load dataset
koi_data <- read.csv("data/cumulative_koi_data.csv")
View(koi_data)



#name_koi_data <- arrange(koi_data, desc(kepoi_name))
#View(name_koi_data)

ggplot(koi_data, aes(x = koi_pdisposition, y = koi_score, color = koi_disposition)) +
  geom_boxplot()
# There is 1 KOI with a Kepler data disposition of CANDIDATE but an archive disposition of FALSE POSITIVE
# There are also a few KOIs with a Kepler data disposition of FALSE POSITIVE but an archive disposition of CONFIRMED


ggplot(na_srad_koi_data, aes(x = koi_prad, y = koi_srad, color = koi_disposition)) +
  geom_point() +
  labs(x = "Planetary radius",
       y = "Stellar radius")

cor(na_srad_koi_data$koi_prad, na_srad_koi_data$koi_srad)




ggplot(candidate_koi, aes(x = koi_score, y = koi_duration)) +
  geom_point(stat = "summary",
           fun = "mean",
           color = "blue") +
  labs(title = 'Average transit duration of "CANDIDATE" KOIs by disposition score',
       x = "Disposition score",
       y = "Transit duration")
ggplot(candidate_koi, aes(x = koi_duration)) +
  geom_histogram()

ggplot(confirmed_koi2, aes(x = koi_score, y = koi_duration)) +
  geom_bar(stat = "summary",
           fun = "mean",
           fill = "springgreen3") +
  labs(title = 'Average transit duration of "CONFIRMED" KOIs by disposition score',
       x = "Disposition score",
       y = "Transit duration")
ggplot(confirmed_koi2, aes(x = koi_duration)) +
  geom_histogram()

ggplot(false_koi2, aes(x = koi_score, y = koi_duration)) +
  geom_bar(stat = "summary",
           fun = "mean",
           fill = "red") +
  labs(title = 'Average transit duration of "FALSE POSITIVE" KOIs by disposition score',
       x = "Disposition score",
       y = "Transit duration")
ggplot(false_koi2, aes(x = koi_duration)) +
  geom_histogram()


ggplot(koi_data, aes(x = koi_duration)) +
  geom_histogram(aes(fill = koi_disposition)) +
  labs(x = "Transit duration (hours)")




ggplot(koi_data, aes(x = koi_disposition, y = koi_duration)) +
  geom_histogram(stat = "summary",
                 fun = "mean",
                 fill = "springgreen4")
