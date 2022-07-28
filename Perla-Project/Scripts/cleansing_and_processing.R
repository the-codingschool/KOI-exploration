View(koi_data)
library(dplyr)
library(ggplot2)
koi_data2 <- select(koi_data, kepid, kepoi_name, kepler_name, koi_disposition, koi_time0bk, koi_duration, koi_depth, koi_model_snr)
View(koi_data2)

koi_data2_len <- nrow(koi_data2) 

koi_data2$label <- c(rep("training", ceiling(.6*koi_data2_len)),
                rep("test", ceiling(.4*koi_data2_len)),
                rep("validation", ceiling(.4*koi_data2_len))) %>%
  sample(koi_data2_len, replace = F)

head(koi_data2)

koi_data2_train <- filter(koi_data2, label == "training")
koi_data2_test <- filter(koi_data2, label == "test")
koi_data2_valid <- filter(koi_data2, label == "validation")

koi_lm <- lm(koi_disposition ~ koi_time0bk + koi_duration + koi_depth + koi_model_snr, data = koi_data2_train)

predict(koi_lm, koi_data2_test_x_vals)