koi_data <- read.csv("data/cumulative_koi_data.csv")
View(koi_data)

library(ggplot2)

ggplot(data = koi_data, aes(x = koi_disposition, y = koi_disposition)) +
  labs (title = "Exoplanet Disposition") +
  geom_bar(stat = "summary", fun = "mean")

ggplot(data = koi_data, aes(x = koi_srad)) +
  labs(title = "Stellar Radius") +
  geom_density()

ggplot(data = koi_data, aes(x = koi_prad)) +
  labs(title = "Exoplanet Radius") +
  geom_density()