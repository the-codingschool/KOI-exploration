# Load required libraries
library(dplyr)
library(ggplot2)

# Load dataset
koi_data <- read.csv("data/cumulative_koi_data.csv")
View(koi_data)



# Tried to sort by date but name doesn't actually correspond to date discovered
#name_koi_data <- arrange(koi_data, desc(kepoi_name))
#View(name_koi_data)



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
koi_period_anova <- aov(data = na_koi_data, koi_period ~ koi_disposition) # does orbital period change when we compare it based on disposition?
summary(koi_period_anova)
TukeyHSD(koi_period_anova)
# 2 of 3 of the groups had significant difference



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
# Very, very off


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
summary(koi_glm)
View(koi_test)


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


## Evaluate performance of models
View(koi_test)

library(Metrics)

# calculate rmse between predictions and true values
rmse(koi_test$koi_period, koi_test$lm_pred)
rmse(koi_test$koi_period, koi_test$gbm_pred) # wins, smaller error

# calculate mae between predictions and true values
mae(koi_test$koi_period, koi_test$lm_pred) # wins, smaller error
mae(koi_test$koi_period, koi_test$gbm_pred) 


## Accuracy
koi_test <- koi_test %>%
  mutate(glm_period_cat = ifelse(glm_pred < 0, "below average", "above average"))
View(koi_test)

true_vals <- sum(koi_test$glm_period_cat == koi_test$koi_period_cat)
total_vals <- nrow(koi_test)

accuracy <- true_vals/total_vals
accuracy

















### Supervised modeling (candidate KOI)
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
head(candidate_koi_test)
# Very, very off


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
  mutate(koi_period_cat = as.factor(ifelse(koi_period < 164.9336, "below average", "above average")))
View(candidate_koi_test)

iris_train_2species <- filter(iris_train, Species %in% c("setosa", "virginica"))

# create the model
iris_glm <- glm(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length,
                data = iris_train_2species,
                family = binomial(link = "logit"))

