# Load required libraries
library(dplyr)
library(ggplot2)

# Load dataset
koi_data <- read.csv("data/cumulative_koi_data.csv")
View(koi_data)



# Tried to sort by date but name doesn't actually correspond to date discovered
#name_koi_data <- arrange(koi_data, desc(kepoi_name))
#View(name_koi_data)


## Test plots
ggplot(clean_koi_data, aes(x = koi_pdisposition, y = koi_score, color = koi_disposition)) +
  geom_boxplot() +
  labs(x = "KOI Exoplanet Archive disposition",
       y = "Disposition score",
       title = "Mean disposition scores across Exoplanet Archive dispositions and Kepler data dispositions")
# There is 1 KOI with a Kepler data disposition of CANDIDATE but an archive disposition of FALSE POSITIVE
# There are also a few KOIs with a Kepler data disposition of FALSE POSITIVE but an archive disposition of CONFIRMED


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
# Colors don't match up to above graphs for this one which makes it a bit confusing

ggplot(koi_data, aes(x = koi_disposition, y = koi_duration)) +
  geom_histogram(stat = "summary",
                 fun = "mean",
                 fill = "thistle")





### RESEARCH QUESTION: How might the orbital periods of planets be affected differently
### by the planetary radius and stellar surface gravity depending on its disposition?

## How is orbital period correlated to planetary radius and stellar surface gravity
ggplot(na_koi_data, aes(x = koi_period, y = koi_prad)) +
  geom_point()
cor(na_koi_data$koi_period, na_koi_data$koi_prad)
## Correlation is very low; about 0.51%

ggplot(na_koi_data, aes(x = koi_period, y = koi_slogg)) +
  geom_point()
cor(na_koi_data$koi_period, na_koi_data$koi_slogg)
# Correlation is also very low; about 0.19%
# This could be in part due to the orbital period outlier (second highest value is 2190.7010, highest value is 129995.7784)


outlier_period_koi_data <- filter(na_koi_data, koi_period < 2200)
saveRDS(outlier_period_koi_data, "KingstonsProject/data/outlier_period_koi_data.RDS")

ggplot(outlier_period_koi_data, aes(x = koi_period, y = koi_prad)) +
  geom_point()
cor(outlier_period_koi_data$koi_period, outlier_period_koi_data$koi_prad)
# Raised correlation to 5.86%, still quite low

ggplot(outlier_period_koi_data, aes(x = koi_period, y = koi_slogg)) +
  geom_point()
cor(outlier_period_koi_data$koi_period, outlier_period_koi_data$koi_slogg)
# Raised correlation to -4.72%, also quite low



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




## ANOVA test
# does orbital period change when we compare it based on disposition?
koi_period_anova <- aov(data = na_koi_data, koi_period ~ koi_disposition)
summary(koi_period_anova)
anova_results <- TukeyHSD(koi_period_anova)$koi_disposition
# 2 of 3 of the groups had significant difference
ggplot(na_koi_data, aes(x = koi_disposition, y = koi_period, fill = koi_disposition)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  stat_summary(fun.data = 'mean_se',
               geom = 'errorbar',
               width = 0.2) +
  annotate("text", label = "*",
           color = "springgreen4",
           size = 10,
           x = c("CANDIDATE", "CONFIRMED"),
           y = c(235, 32)) +
  annotate("text", label = "*",
           color = "blue",
           size = 10,
           x = c("CANDIDATE", "FALSE POSITIVE"),
           y = c(250, 70)) +
  labs(x = "KOI Disposition",
       y = "Average Orbital Period (days)",
       title = "Average Orbital Period by Disposition")
# Without extreme outlier
View(outlier_period_koi_data)
outlier_koi_period_anova <- aov(data = outlier_period_koi_data, koi_period ~ koi_disposition) # does orbital period change when we compare it based on disposition?
summary(outlier_koi_period_anova)
outlier_anova_results <- TukeyHSD(outlier_koi_period_anova)$koi_disposition
# all 3 of the groups had significant difference
ggplot(outlier_period_koi_data, aes(x = koi_disposition, y = koi_period, fill = koi_disposition)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  stat_summary(fun.data = 'mean_se',
               geom = 'errorbar',
               width = 0.2) +
  annotate("text", label = "*",
           color = "springgreen4",
           size = 10,
           x = c("CANDIDATE", "CONFIRMED"),
           y = c(108, 32)) +
  annotate("text", label = "*",
           color = "blue",
           size = 10,
           x = c("CANDIDATE", "FALSE POSITIVE"),
           y = c(115, 68)) +
  annotate("text", label = "*",
           color = "red",
           size = 10,
           x = c("CONFIRMED", "FALSE POSITIVE"),
           y = c(39, 75)) +
  labs(x = "KOI Disposition",
       y = "Average Orbital Period (days)",
       title = "Average Orbital Period by Disposition Without Extreme Outlier")
  




##  k-means clustering
koi_numerics <- select(na_koi_data, koi_prad, koi_slogg) %>%
  scale()

koi_clusters <- kmeans(koi_numerics, centers = 3)
koi_clusters

koi_clusters$cluster # vector designating a cluster for each row
na_koi_data$cluster <- koi_clusters$cluster
View(na_koi_data)
sorted_na_koi_data2 <- arrange(na_koi_data, cluster)
View(sorted_na_koi_data2)
# Removing the extreme outlier to make colors more visible
sorted_na_koi_data2 <- filter(sorted_na_koi_data2, koi_period < 2200)

ggplot(sorted_na_koi_data2, aes(x = koi_prad, y = koi_slogg)) +
  geom_point(aes(color = as.factor(cluster)))

ggplot(sorted_na_koi_data2, aes(x = koi_prad, y = koi_slogg)) +
  geom_point(aes(color = koi_period))










### Supervised modeling
ggplot(na_koi_data, aes(x = koi_period, y = koi_prad)) +
  geom_point()
ggplot(na_koi_data, aes(x = koi_period, y = koi_slogg)) +
  geom_point()
ggplot(na_koi_data, aes(x = koi_period, y = koi_disposition)) +
  geom_point()

cor(na_koi_data$koi_period, na_koi_data$koi_prad)
cor(na_koi_data$koi_period, na_koi_data$koi_slogg)

# Choose features
# koi_prad, koi_slogg, koi_disposition

# Split into training, test, and validation sets
koi_len <-nrow(na_koi_data)

na_koi_data$label <-c(rep("training", ceiling(.6*koi_len)),
                           rep("test", ceiling(.2*koi_len)),
                           rep("validation", ceiling(.2*koi_len))) %>%
  sample(koi_len, replace = F)

koi_train <- filter(na_koi_data, label == "training")
koi_test <- filter(na_koi_data, label == "test")
koi_valid <- filter(na_koi_data, label == "validation")


## Linear model
koi_lm <- lm(koi_period ~ koi_slogg + koi_prad, data = koi_train) 
koi_lm
summary(koi_lm)

koi_lm_predictions <- select(koi_test, koi_slogg, koi_prad) %>%
  predict(object = koi_lm)
koi_test$lm_pred <- koi_lm_predictions
View(koi_test)


## Logistic model
# create 2 categories
mean(na_koi_data$koi_period)
koi_train_glm <- koi_train %>%
  mutate(koi_period_cat = as.factor(ifelse(koi_period < 74.30795, "below average", "above average")))
head(koi_train_glm)

koi_glm <- glm(koi_period_cat ~ koi_slogg + koi_prad + koi_disposition,
                         data = koi_train_glm,
                         family = binomial(link = "logit"))
summary(koi_glm)

koi_glm_preds <- koi_test %>%
  select(koi_slogg, koi_prad, koi_disposition) %>%
  predict(object = koi_glm)

koi_test$glm_pred <- koi_glm_preds

koi_test <- koi_test %>%
  mutate(koi_period_cat = as.factor(ifelse(koi_period < 74.30795, "below average", "above average")))

View(koi_test)
# Only got a few negative values


## Gradient boosting machine
library(gbm)

# create the model
koi_gbm <- gbm(koi_period ~ koi_slogg + koi_prad + as.factor(koi_disposition),
                data = koi_test,
                n.trees = 500)
summary(koi_gbm)

# select out only the x-values we used from test and predict
koi_gbm_preds <- koi_test %>%
  select(koi_slogg, koi_prad, koi_disposition) %>%
  predict(object = koi_gbm)

# save predictions back into test set
koi_test$gbm_pred <- koi_gbm_preds
View(koi_test)

library(Metrics)

# calculate rmse between predictions and true values
lm_rmse <- rmse(koi_test$koi_period, koi_test$lm_pred)
gbm_rmse <- rmse(koi_test$koi_period, koi_test$gbm_pred) # wins, smaller error

# calculate mae between predictions and true values
lm_mae <- mae(koi_test$koi_period, koi_test$lm_pred)
gbm_mae <- mae(koi_test$koi_period, koi_test$gbm_pred) # wins, smaller error


## Accuracy
koi_test <- koi_test %>%
  mutate(glm_period_cat = ifelse(glm_pred > 0, "below average", "above average"))
View(koi_test)

true_vals <- sum(koi_test$glm_period_cat == koi_test$koi_period_cat)
total_vals <- nrow(koi_test)

accuracy <- true_vals/total_vals
accuracy


## Saving dataset as RDS file
saveRDS(koi_test, "KingstonsProject/data/koi_test.RDS")


# Linear model plots
ggplot(koi_test, aes(x = koi_period, y = koi_slogg)) +
  geom_point() +
  geom_line(data = koi_test, aes(x = lm_pred), color = "indianred2") +
  labs(x = "Orbital Period (days)",
       y= "Stellar Surface Gravity",
       title = "All KOIs: Predictions of Orbital Period by Stellar Surface Gravity")
ggplot(koi_test, aes(x = koi_period, y = lm_pred)) +
  geom_point() +
  geom_abline(color = "blue") +
  labs(x = "Actual Orbital Period",
       y= "Predicted Values",
       title = "All KOIs: Actual Orbital Period Compared to Predicted Orbital Period")

# Logistic model plot
ggplot(koi_test, aes(x = koi_period_cat, fill = glm_period_cat)) +
  geom_bar() +
  labs(x = "Actual Results",
       fill = "Prediction")

# Gradient boosting machine plots
ggplot(koi_test, aes(x = koi_period, y = koi_prad)) +
  geom_point() +
  geom_line(data = koi_test, aes(x = gbm_pred), color = "indianred2") +
  labs(x = "Orbital period (days)",
       y= "Planetary Radius (Earth radii)",
       title = "All KOIs")
ggplot(koi_test, aes(x = koi_period, y = gbm_pred)) +
  geom_point() +
  geom_abline(color = "blue") +
  labs(x = "Actual Orbital Period",
       y= "Predicted Values",
       title = "All KOIs: Actual Orbital Period Compared to Predicted Orbital Period")
summary(koi_gbm)


# Comparing model accuracy between lm and gbm
model_accuracy <- data.frame(model_type = "", rmse = "", mae = "")
model_accuracy <- model_accuracy[-1,]
model_accuracy <- rbind(model_accuracy, data.frame(model_type = "lm", rmse = lm_rmse, mae = lm_mae))
model_accuracy <- rbind(model_accuracy, data.frame(model_type = "gbm", rmse = gbm_rmse, mae = gbm_mae))
model_accuracy
ggplot(data = model_accuracy, aes(x = model_type)) +
  geom_point(aes(y = rmse), color = "red", size = 5) +
  geom_point(aes(y = mae), color = "blue", size = 5) +
  annotate("text", label = "RMSE",
           color = "red",
           size = 7,
           x = 0.55,
           y = 120) +
  annotate("text", label = "MAE",
           color = "blue",
           size = 7,
           x = 0.55,
           y = 116) +
  labs(x = "Model Type",
       y = "Score",
       title = "All KOIs: RMSE and MAE Scores for Gradient Boosting Machine and Linear Model")











### Supervised modeling (CONFIRMED KOI)
ggplot(na_confirmed_koi, aes(x = koi_period, y = koi_prad)) +
  geom_point()
ggplot(na_confirmed_koi, aes(x = koi_period, y = koi_slogg)) +
  geom_point()

cor(na_confirmed_koi$koi_period, na_confirmed_koi$koi_prad)
cor(na_confirmed_koi$koi_period, na_confirmed_koi$koi_slogg)

# Choose features
# koi_prad, koi_slogg, koi_disposition

# Split into training, test, and validation sets
confirmed_koi_len <-nrow(na_confirmed_koi)

na_confirmed_koi$label <-c(rep("training", ceiling(.6*confirmed_koi_len)),
                      rep("test", ceiling(.2*confirmed_koi_len)),
                      rep("validation", ceiling(.2*confirmed_koi_len))) %>%
  sample(confirmed_koi_len, replace = F)

confirmed_koi_train <- filter(na_confirmed_koi, label == "training")
confirmed_koi_test <- filter(na_confirmed_koi, label == "test")
confirmed_koi_valid <- filter(na_confirmed_koi, label == "validation")


## Linear model
confirmed_koi_lm <- lm(koi_period ~ koi_prad + koi_slogg, data = confirmed_koi_train) 
confirmed_koi_lm
summary(confirmed_koi_lm)

confirmed_koi_lm_predictions <- select(confirmed_koi_test, koi_slogg, koi_prad) %>%
  predict(object = confirmed_koi_lm)
confirmed_koi_test$lm_pred <- confirmed_koi_lm_predictions
View(confirmed_koi_test)


## Logistic model
# create 2 categories
mean(na_confirmed_koi$koi_period)
confirmed_koi_train_glm <- confirmed_koi_train %>%
  mutate(confirmed_koi_period_cat = as.factor(ifelse(koi_period < 27.626, "below average", "above average")))
head(confirmed_koi_train_glm)

confirmed_koi_glm <- glm(confirmed_koi_period_cat ~ koi_prad + koi_slogg,
               data = confirmed_koi_train_glm,
               family = binomial(link = "logit")) # can't use koi_disposition as there is only one unique value
summary(confirmed_koi_glm)

confirmed_koi_glm_preds <- confirmed_koi_test %>%
  select(koi_slogg, koi_prad) %>%
  predict(object = confirmed_koi_glm)

confirmed_koi_test$glm_pred <- confirmed_koi_glm_preds
confirmed_koi_test <- confirmed_koi_test %>%
  mutate(confirmed_koi_period_cat = as.factor(ifelse(koi_period < 27.626, "below average", "above average")))

View(confirmed_koi_test)


## Gradient boosting machine
library(gbm)

# create the model
confirmed_koi_gbm <- gbm(koi_period ~ koi_prad + koi_slogg,
               data = confirmed_koi_test,
               n.trees = 500) # shouldn't use koi_disposition as there is only one unique value
summary(confirmed_koi_gbm)

# select out only the x-values we used from test and predict
confirmed_koi_gbm_preds <- confirmed_koi_test %>%
  select(koi_slogg, koi_prad) %>%
  predict(object = confirmed_koi_gbm)

# save predictions back into test set
confirmed_koi_test$gbm_pred <- confirmed_koi_gbm_preds
View(confirmed_koi_test)

library(Metrics)

# calculate rmse between predictions and true values
confirmed_lm_rmse <- rmse(confirmed_koi_test$koi_period, confirmed_koi_test$lm_pred)
confirmed_gbm_rmse <- rmse(confirmed_koi_test$koi_period, confirmed_koi_test$gbm_pred) # wins, smaller error

# calculate mae between predictions and true values
confirmed_lm_mae <- mae(confirmed_koi_test$koi_period, confirmed_koi_test$lm_pred)
confirmed_gbm_mae <- mae(confirmed_koi_test$koi_period, confirmed_koi_test$gbm_pred) # wins, smaller error


## Accuracy
confirmed_koi_test <- confirmed_koi_test %>%
  mutate(glm_period_cat = ifelse(glm_pred < 0, "below average", "above average"))
View(confirmed_koi_test)

confirmed_true_vals <- sum(confirmed_koi_test$glm_period_cat == confirmed_koi_test$confirmed_koi_period_cat)
confirmed_total_vals <- nrow(confirmed_koi_test)

confirmed_accuracy <- confirmed_true_vals/confirmed_total_vals
confirmed_accuracy


## Saving dataset as RDS file
saveRDS(confirmed_koi_test, "KingstonsProject/data/confirmed_koi_test.RDS")


# Linear model plots
ggplot(confirmed_koi_test, aes(x = koi_period, y = koi_prad)) +
  geom_point() +
  geom_line(data = confirmed_koi_test, aes(x = lm_pred, color = "red"))
ggplot(confirmed_koi_test, aes(x = koi_period, y = lm_pred)) +
  geom_point() +
  geom_abline(color = "blue")

# Logistic model plot
ggplot(confirmed_koi_test, aes(x = confirmed_koi_period_cat, fill = glm_period_cat)) +
  geom_bar() +
  labs(x = "Actual Results",
       fill = "Prediction")

# Gradient boosting machine plots
ggplot(confirmed_koi_test, aes(x = koi_period, y = koi_prad)) +
  geom_point() +
  geom_line(data = confirmed_koi_test, aes(x = gbm_pred), color = "indianred2") +
  labs(x = "Orbital Period (days)",
       y = "Planetary Radius (Earth radii)")
ggplot(confirmed_koi_test, aes(x = koi_period, y = gbm_pred)) +
  geom_point() +
  geom_abline(color = "blue")
summary(confirmed_koi_gbm)


# Comparing model accuracy between lm and gbm
confirmed_model_accuracy <- data.frame(model_type = "", rmse = "", mae = "")
confirmed_model_accuracy <- confirmed_model_accuracy[-1,]
confirmed_model_accuracy <- rbind(confirmed_model_accuracy, data.frame(model_type = "lm", rmse = confirmed_lm_rmse, mae = confirmed_lm_mae))
confirmed_model_accuracy <- rbind(confirmed_model_accuracy, data.frame(model_type = "gbm", rmse = confirmed_gbm_rmse, mae = confirmed_gbm_mae))
confirmed_model_accuracy
ggplot(data = confirmed_model_accuracy, aes(x = model_type)) +
  geom_point(aes(y = rmse), color = "red", size = 5) +
  geom_point(aes(y = mae), color = "blue", size = 5) +
  annotate("text", label = "RMSE",
           color = "red",
           size = 7,
           x = 0.55,
           y = 45) +
  annotate("text", label = "MAE",
           color = "blue",
           size = 7,
           x = 0.55,
           y = 43) +
  labs(x = "Model type",
       y = "Score",
       title = "Confirmed KOIs")










### Supervised modeling (CANDIDATE KOI)
ggplot(na_candidate_koi, aes(x = koi_period, y = koi_prad)) +
  geom_point()
ggplot(na_candidate_koi, aes(x = koi_period, y = koi_slogg)) +
  geom_point()
ggplot(na_candidate_koi, aes(x = koi_period, y = koi_disposition)) +
  geom_point()

cor(na_candidate_koi$koi_period, na_candidate_koi$koi_prad)
cor(na_candidate_koi$koi_period, na_candidate_koi$koi_slogg)


candidate_koi_len <-nrow(na_candidate_koi)

na_candidate_koi$label <-c(rep("training", ceiling(.6*candidate_koi_len)),
                           rep("test", ceiling(.2*candidate_koi_len)),
                           rep("validation", ceiling(.2*candidate_koi_len))) %>%
  sample(candidate_koi_len, replace = F)

candidate_koi_train <- filter(na_candidate_koi, label == "training")
candidate_koi_test <- filter(na_candidate_koi, label == "test")
candidate_koi_valid <- filter(na_candidate_koi, label == "validation")


## Linear model
candidate_koi_lm <- lm(koi_period ~ koi_slogg + koi_prad, data = candidate_koi_train) 
candidate_koi_lm
summary(candidate_koi_lm)

candidate_koi_lm_predictions <- select(candidate_koi_test, koi_slogg, koi_prad) %>%
  predict(object = candidate_koi_lm)
candidate_koi_test$lm_pred <- candidate_koi_lm_predictions
View(candidate_koi_test)


## Logistic model
mean(na_candidate_koi$koi_period)
candidate_koi_train_glm <- candidate_koi_train %>%
  mutate(koi_period_cat = as.factor(ifelse(koi_period < 164.9336, "below average", "above average")))
head(candidate_koi_train_glm)

candidate_koi_glm <- glm(koi_period_cat ~ koi_slogg + koi_prad,
                data = candidate_koi_train_glm,
                family = binomial(link = "logit"))
summary(candidate_koi_glm)

candidate_koi_glm_preds <- candidate_koi_test %>%
  select(koi_slogg, koi_prad) %>%
  predict(object = candidate_koi_glm)

candidate_koi_test$glm_pred <- candidate_koi_glm_preds

candidate_koi_test <- candidate_koi_test %>%
  mutate(candidate_koi_period_cat = as.factor(ifelse(koi_period < 164.9336, "below average", "above average")))

View(candidate_koi_test)


## Gradient boosting machine
library(gbm)

# create the model
candidate_koi_gbm <- gbm(koi_period ~ koi_prad + koi_slogg,
                         data = candidate_koi_test,
                         n.trees = 500) # shouldn't use koi_disposition as there is only one unique value
summary(candidate_koi_gbm)

# select out only the x-values we used from test and predict
candidate_koi_gbm_preds <- candidate_koi_test %>%
  select(koi_slogg, koi_prad) %>%
  predict(object = candidate_koi_gbm)

# save predictions back into test set
candidate_koi_test$gbm_pred <- candidate_koi_gbm_preds
View(candidate_koi_test)

library(Metrics)

# calculate rmse between predictions and true values
candidate_lm_rmse <- rmse(candidate_koi_test$koi_period, candidate_koi_test$lm_pred)
candidate_gbm_rmse <- rmse(candidate_koi_test$koi_period, candidate_koi_test$gbm_pred) # wins, smaller error

# calculate mae between predictions and true values
candidate_lm_mae <- mae(candidate_koi_test$koi_period, candidate_koi_test$lm_pred) # wins, smaller error
candidate_gbm_mae <- mae(candidate_koi_test$koi_period, candidate_koi_test$gbm_pred)


## Accuracy
candidate_koi_test <- candidate_koi_test %>%
  mutate(glm_period_cat = ifelse(glm_pred < 0, "below average", "above average"))
View(candidate_koi_test)

candidate_true_vals <- sum(candidate_koi_test$glm_period_cat == candidate_koi_test$candidate_koi_period_cat)
candidate_total_vals <- nrow(candidate_koi_test)

candidate_accuracy <- candidate_true_vals/candidate_total_vals
candidate_accuracy


# Linear model plots
ggplot(candidate_koi_test, aes(x = koi_period, y = koi_slogg)) +
  geom_point() +
  geom_line(data = candidate_koi_test, aes(x = lm_pred, color = "red"))
ggplot(candidate_koi_test, aes(x = koi_period, y = lm_pred)) +
  geom_point() +
  geom_abline(color = "blue")

# Logistic model plot
ggplot(candidate_koi_test, aes(x = candidate_koi_period_cat, fill = glm_period_cat)) +
  geom_bar() +
  labs(x = "Actual results",
       fill = "Prediction")

# Gradient boosting machine plots
ggplot(candidate_koi_test, aes(x = koi_period, y = koi_slogg)) +
  geom_point() +
  geom_line(data = candidate_koi_test, aes(x = gbm_pred), color = "indianred2") +
  labs(title = "Candidate KOIs",
       x = "Orbital period (days)",
       y = "Stellar surface gravity")
ggplot(candidate_koi_test, aes(x = koi_period, y = gbm_pred)) +
  geom_point() +
  geom_abline(color = "blue")
summary(candidate_koi_gbm)


# Comparing model accuracy between lm and gbm
candidate_model_accuracy <- data.frame(model_type = "", rmse = "", mae = "")
# candidate_model_accuracy <- candidate_model_accuracy[-1,]
candidate_model_accuracy <- rbind(candidate_model_accuracy, data.frame(model_type = "lm", rmse = candidate_lm_rmse, mae = candidate_lm_mae))
candidate_model_accuracy <- rbind(candidate_model_accuracy, data.frame(model_type = "gbm", rmse = candidate_gbm_rmse, mae = candidate_gbm_mae))
candidate_model_accuracy
ggplot(data = candidate_model_accuracy, aes(x = model_type)) +
  geom_point(aes(y = rmse), color = "red", size = 5) +
  geom_point(aes(y = mae), color = "blue", size = 5) +
  annotate("text", label = "RMSE",
           color = "red",
           size = 7,
           x = 0.55,
           y = 144) +
  annotate("text", label = "MAE",
           color = "blue",
           size = 7,
           x = 0.55,
           y = 140) +
  labs(x = "Model type",
       y = "Score",
       title = "Candidate KOIs")









### Supervised modeling (FALSE POSITIVE KOI)
ggplot(na_false_koi, aes(x = koi_period, y = koi_prad)) +
  geom_point()
ggplot(na_false_koi, aes(x = koi_period, y = koi_slogg)) +
  geom_point()

cor(na_false_koi$koi_period, na_false_koi$koi_prad)
cor(na_false_koi$koi_period, na_false_koi$koi_slogg)

# Choose features
# koi_prad, koi_slogg, koi_disposition

# Split into training, test, and validation sets
false_koi_len <-nrow(na_false_koi)

na_false_koi$label <-c(rep("training", ceiling(.6*false_koi_len)),
                           rep("test", ceiling(.2*false_koi_len)),
                           rep("validation", ceiling(.2*false_koi_len))) %>%
  sample(false_koi_len, replace = F)

false_koi_train <- filter(na_false_koi, label == "training")
false_koi_test <- filter(na_false_koi, label == "test")
false_koi_valid <- filter(na_false_koi, label == "validation")


## Linear model
false_koi_lm <- lm(koi_period ~ koi_prad + koi_slogg, data = false_koi_train) 
false_koi_lm
summary(false_koi_lm)

false_koi_lm_predictions <- select(false_koi_test, koi_slogg, koi_prad) %>%
  predict(object = false_koi_lm)
false_koi_test$lm_pred <- false_koi_lm_predictions
View(false_koi_test)


## Logistic model
# create 2 categories
mean(na_false_koi$koi_period)
false_koi_train_glm <- false_koi_train %>%
  mutate(false_koi_period_cat = as.factor(ifelse(koi_period < 62.93709, "below average", "above average")))
head(false_koi_train_glm)

false_koi_glm <- glm(false_koi_period_cat ~ koi_prad + koi_slogg,
                         data = false_koi_train_glm,
                         family = binomial(link = "logit")) # can't use koi_disposition as there is only one unique value
summary(false_koi_glm)

false_koi_glm_preds <- false_koi_test %>%
  select(koi_slogg, koi_prad) %>%
  predict(object = false_koi_glm)

false_koi_test$glm_pred <- false_koi_glm_preds
false_koi_test <- false_koi_test %>%
  mutate(false_koi_period_cat = as.factor(ifelse(koi_period < 62.93709, "below average", "above average")))

View(false_koi_test)


## Gradient boosting machine
library(gbm)

# create the model
false_koi_gbm <- gbm(koi_period ~ koi_prad + koi_slogg,
                         data = false_koi_test,
                         n.trees = 500) # shouldn't use koi_disposition as there is only one unique value
summary(false_koi_gbm)

# select out only the x-values we used from test and predict
false_koi_gbm_preds <- false_koi_test %>%
  select(koi_slogg, koi_prad) %>%
  predict(object = false_koi_gbm)

# save predictions back into test set
false_koi_test$gbm_pred <- false_koi_gbm_preds
View(false_koi_test)

library(Metrics)

# calculate rmse between predictions and true values
false_lm_rmse <- rmse(false_koi_test$koi_period, false_koi_test$lm_pred)
false_gbm_rmse <- rmse(false_koi_test$koi_period, false_koi_test$gbm_pred) # wins, smaller error

# calculate mae between predictions and true values
false_lm_mae <- mae(false_koi_test$koi_period, false_koi_test$lm_pred)
false_gbm_mae <- mae(false_koi_test$koi_period, false_koi_test$gbm_pred) # wins, smaller error


## Accuracy
false_koi_test <- false_koi_test %>%
  mutate(glm_period_cat = ifelse(glm_pred < 0, "below average", "above average"))
View(false_koi_test)

false_true_vals <- sum(false_koi_test$glm_period_cat == false_koi_test$false_koi_period_cat)
false_total_vals <- nrow(false_koi_test)

false_accuracy <- false_true_vals/false_total_vals
false_accuracy


# Linear model plots
ggplot(false_koi_test, aes(x = koi_period, y = koi_prad)) +
  geom_point() +
  geom_line(data = false_koi_test, aes(x = lm_pred, color = "red"))
ggplot(false_koi_test, aes(x = koi_period, y = lm_pred)) +
  geom_point() +
  geom_abline(color = "blue")

# Logistic model plot
ggplot(false_koi_test, aes(x = false_koi_period_cat, fill = glm_period_cat)) +
  geom_bar() +
  labs(x = "Actual results",
       fill = "Prediction")

# Gradient boosting machine plots
ggplot(false_koi_test, aes(x = koi_period, y = koi_slogg)) +
  geom_point() +
  geom_line(data = false_koi_test, aes(x = gbm_pred), color = "indianred2") +
  labs(x = "Orbital period (days)",
       y = "Stellar surface gravity",
       title = "False positive KOIs (Gradient boosting machine)")
ggplot(false_koi_test, aes(x = koi_period, y = gbm_pred)) +
  geom_point() +
  geom_abline(color = "blue")
summary(false_koi_gbm)


# Comparing model accuracy between lm and gbm
false_model_accuracy <- data.frame(model_type = "", rmse = "", mae = "")
false_model_accuracy <- false_model_accuracy[-1,]
false_model_accuracy <- rbind(false_model_accuracy, data.frame(model_type = "lm", rmse = false_lm_rmse, mae = false_lm_mae))
false_model_accuracy <- rbind(false_model_accuracy, data.frame(model_type = "gbm", rmse = false_gbm_rmse, mae = false_gbm_mae))
false_model_accuracy
ggplot(data = false_model_accuracy, aes(x = model_type)) +
  geom_point(aes(y = rmse), color = "red", size = 5) +
  geom_point(aes(y = mae), color = "blue", size = 5) +
  annotate("text", label = "RMSE",
           color = "red",
           size = 7,
           x = 0.55,
           y = 134) +
  annotate("text", label = "MAE",
           color = "blue",
           size = 7,
           x = 0.55,
           y = 130) +
  labs(x = "Model type",
       y = "Score",
       title = "False Positive KOIs")



