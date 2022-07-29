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

koi_data2_test_x_vals <- koi_data2_test %>% filter(koi_disposition %in% c("FALSE POSITIVE", "CONFIRMED")) %>% select(koi_time0bk, koi_duration, koi_depth, koi_model_snr)

predict(glm_fit, koi_data2_test_x_vals)


koi_data2_train_2 <- filter(koi_data2_train, koi_disposition %in% c("FALSE POSITIVE","CONFIRMED"))

koi_data2_glm <- glm(as.factor(koi_disposition) ~ koi_time0bk + koi_duration + koi_depth + koi_model_snr, 
                data = koi_data2_train_2,
                family = binomial(link = "logit"))

summary(koi_data2_glm)

koi_data2_test_2 <- koi_data2_test %>% 
  filter(koi_disposition %in% c("FALSE POSITIVE","CONFIRMED")) 

koi_data2_2_preds <- koi_data2_test_2 %>%
  select(-koi_disposition) %>%
  predict(object = koi_data2_glm)

koi_data2_test_2$glm_2_pred <- koi_data2_2_preds

View(koi_data2_test)


true_vals <-sum(koi_data2_test$glm_disposition == koi_data2_test$petal_length_cat)
total_vals <- nrow(koi_data2_test)

accuracy <- true_vals/total_vals 
accuracy

f1(koi_data2_test$glm_disposition, koi_data2_test$petal_length_cat)




