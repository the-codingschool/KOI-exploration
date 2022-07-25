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


ggplot(na_srad_koi_data, aes(x = koi_prad, y = koi_srad)) +
  geom_point()

cor(na_srad_koi_data$koi_prad, na_srad_koi_data$koi_srad)



ggplot(candidate_koi, aes(x = koi_score, y = koi_duration)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  labs(title = 'Average transit duration of "CANDIDATE" KOIs by disposition score',
       x = "Disposition score",
       y = "Transit duration")

