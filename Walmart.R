

# Libraries ---------------------------------------------------------------

library(vroom)
library(dplyr)
library(corrplot)
library(recipes)
library(embed)
library(discrim)
library(tidyverse)
library(workflows)
library(tidymodels)
library(prophet)
library(patchwork)
# Reading In --------------------------------------------------------------

setwd("~/GitHub/Walmart-Recuiting")
Test <- vroom("test.csv")
Train <- vroom("train.csv")
Store_list <- vroom("stores.csv")
Features <- vroom("features.csv")


# 0 Method ----------------------------------------------------------------

Features[is.na(Features)] <- 0
Features$MarkdownTotal <- rowSums(Features[,c("MarkDown1", "MarkDown2",
                                              "MarkDown3","MarkDown4",
                                              "MarkDown5")])
Features$MarkdownFlag <- as.integer(!Features$MarkdownTotal == 0)
Features <- Features[, -c(5,6,7,8,9)]

joined_train <- left_join(
  Train,
  Features,
  by = c("Store", "Date", "IsHoliday")
)

joined_test <- left_join(
  Test,
  Features,
  by = c("Store", "Date", "IsHoliday")
)

head(joined_train) 
head(joined_test)



# 3 ML Tests --------------------------------------------------------------

# Recipe

Walmart_Recipe <- recipe(Weekly_Sales ~ ., data = joined_train) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_lencode(all_nominal_predictors(), outcome = vars(Weekly_Sales), smooth = FALSE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())



# Specific Samples ----------------------------------------------------------

Sample1 <- filter(joined_train, Store == 2 & Dept == 1)
Sample2 <- filter(joined_train, Store == 19 & Dept == 33)
Sample3 <- filter(joined_train, Store == 41 & Dept == 60)

# Random Forests

rf_mod <- rand_forest(
  mtry  = tune(),
  min_n = tune(),
  trees = 100
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

rf_wf <- workflow() %>%
  add_recipe(Walmart_Recipe) %>%
  add_model(rf_mod)

folds <- vfold_cv(Sample3, v = 5)

rf_tuned <- tune_grid(
  rf_wf,
  resamples = folds,
  metrics = metric_set(rmse)
)

rf_best <- select_best(rf_tuned, metric = "rmse")

cv_results <- collect_metrics(rf_tuned) %>%
  filter(mtry == rf_best$mtry, min_n == rf_best$min_n)

cv_results %>%
  select(.metric, mean, std_err)



# Linear Reg --------------------------------------------------------------

lin_mod <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

lin_wf <- workflow() %>%
  add_recipe(Walmart_Recipe) %>%
  add_model(lin_mod)

folds <- vfold_cv(Sample3, v = 5)

lin_cv <- fit_resamples(
  lin_wf,
  resamples = folds,
  metrics = metric_set(rmse),
  control = control_resamples(save_pred = TRUE)
)

lin_results <- collect_metrics(lin_cv)

lin_results


# KNN ---------------------------------------------

knn_mod <- nearest_neighbor(
  neighbors = tune(),
  weight_func = tune()
) %>%
  set_engine("kknn") %>%
  set_mode("regression")

knn_wf <- workflow() %>%
  add_recipe(Walmart_Recipe) %>%
  add_model(knn_mod)

knn_tuned <- tune_grid(
  knn_wf,
  resamples = folds,
  grid = 20,
  metrics = metric_set(rmse)
)

collect_metrics(knn_tuned)



# Choose Store and Dept
store <- 19   
dept  <- 33

# Filter and rename columns to Prophet format
sd_train <- joined_train %>%
  filter(Store == store, Dept == dept) %>%
  rename(y = Weekly_Sales, ds = Date)

sd_test <- joined_test %>%
  filter(Store == store, Dept == dept) %>%
  rename(ds = Date)

# Fit Prophet model
prophet_model <- prophet() %>%
  add_regressor("Fuel_Price") %>%
  add_regressor("Temperature") %>%
  add_regressor("MarkdownTotal") %>%
  fit.prophet(df = sd_train)

# Predict on train and test
fitted_vals <- predict(prophet_model, sd_train)   # Fitted values
test_preds  <- predict(prophet_model, sd_test)    # Forecast values

# Plot fitted + forecast
H <- ggplot() +
  geom_line(data = sd_train,
            aes(x = ds, y = y, color = "Data")) +
  
  geom_line(data = fitted_vals,
            aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  
  geom_line(data = test_preds,
            aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  
  scale_color_manual(values = c(
    "Data" = "black",
    "Fitted" = "blue",
    "Forecast" = "red"
  )) +
  
  labs(color = "",
       x = "Date",
       y = "Weekly Sales",
       title = paste0("Prophet Model: Store ", store,
                      ", Dept ", dept)) +
  
  theme_minimal()

# Choose Store and Dept
store <- 41   
dept  <- 60

# Filter and rename columns to Prophet format
sd_train <- joined_train %>%
  filter(Store == store, Dept == dept) %>%
  rename(y = Weekly_Sales, ds = Date)

sd_test <- joined_test %>%
  filter(Store == store, Dept == dept) %>%
  rename(ds = Date)

# Fit Prophet model
prophet_model <- prophet() %>%
  add_regressor("Fuel_Price") %>%
  add_regressor("Temperature") %>%
  add_regressor("MarkdownTotal") %>%
  fit.prophet(df = sd_train)

# Predict on train and test
fitted_vals <- predict(prophet_model, sd_train)   # Fitted values
test_preds  <- predict(prophet_model, sd_test)    # Forecast values

# Plot fitted + forecast
I <- ggplot() +
  geom_line(data = sd_train,
            aes(x = ds, y = y, color = "Data")) +
  
  geom_line(data = fitted_vals,
            aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
  
  geom_line(data = test_preds,
            aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
  
  scale_color_manual(values = c(
    "Data" = "black",
    "Fitted" = "blue",
    "Forecast" = "red"
  )) +
  
  labs(color = "",
       x = "Date",
       y = "Weekly Sales",
       title = paste0("Prophet Model: Store ", store,
                      ", Dept ", dept)) +
  
  theme_minimal()

H+I
# EDA -------------------------------------------------------------

colnames(Train)
colnames(Store_list)
colnames(Features)


Train$Size <- Store_list$Size[ match(Train$Store, Store_list$Store) ]
Train$Type <- Store_list$Type[ match(Train$Store, Store_list$Store) ]

unique(pull(Test,Store))
unique(pull(Train,Store))

table(pull(Train,Store))
table(pull(Test,Store))

head(Features)

numeric_cols <- Features %>% select(where(is.numeric))
cor_matrix <- cor(numeric_cols, use = "complete.obs")
corrplot(cor_matrix)

