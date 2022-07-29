View(koi_data)
library(dplyr)
library(ggplot2)
koi_data2 <- select(koi_data, koi_disposition, koi_time0bk, koi_duration, koi_depth, koi_model_snr)
View(koi_data2)

koi_data2[is.na(koi_data2)] = NA

koi_data2_len <- nrow(koi_data2) 

koi_data2$label <- c(rep("training", ceiling(.6*koi_data2_len)),
                   rep("test", ceiling(.4*koi_data2_len)),
                   rep("validation", ceiling(.4*koi_data2_len))) %>%
  sample(koi_data2_len, replace = F)

head(koi_data2)

koi_data2_train <- filter(koi_data2, label == "training")
koi_data2_test <- filter(koi_data2, label == "test")
koi_data2_valid <- filter(koi_data2, label == "validation")

koi_data2_train_filtered <- filter(koi_data2_train, koi_disposition %in% c("FALSE POSITIVE", "CONFIRMED"))

glm_fit <- glm(as.factor(koi_disposition) ~ koi_time0bk + koi_duration + koi_depth + koi_model_snr, data = koi_data2_train_filtered, na.action = na.exclude, family = binomial(link='logit'))

koi_data2_test_x_vals <- koi_data2_test %>% filter(koi_disposition %in% c("FALSE POSITIVE", "CONFIRMED")) %>%
  select(koi_time0bk, koi_duration, koi_depth, koi_model_snr)

length(predict(glm_fit, koi_data2_test_x_vals))
koi_data2_test_2disp <- filter(koi_data2_test, koi_disposition %in% c("FALSE POSITIVE", "CONFIRMED"))
koi_data2_test_2disp$glm_preds <- predict(glm_fit, koi_data2_test_x_vals)
View(koi_data2_test_2disp)

koi_data2_test_2disp$pred_disp <- ifelse(koi_data2_test_2disp$glm_preds < 0, "CONFIRMED", "FALSE POSITIVE")

na.omit(koi_data2_test_2disp)
koi_data2_test_2disp <- na.omit(koi_data2_test_2disp)

accuracy(koi_data2_test_2disp$koi_disposition, koi_data2_test_2disp$pred_disp)

f1(koi_data2_test$glm_disposition, koi_data2_test$petal_length_cat)

View(koi_data2_test_2disp)


ggplot(data = koi_data2_test_2disp, aes(x = koi_disposition, fill = koi_disposition == pred_disp)) +
  labs (title = "Disposition Comparison") +
  geom_bar()


