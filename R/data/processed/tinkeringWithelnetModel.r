
library(tidyverse)
library(ggplot2)
library(VIM)
library(reshape2)
library(dplyr)
library(ggplot2)    # used for plotting
library(tidymodels) #used for models
library(glmnet) #used for lasso
library(recipes)


# 1. Read Data
model_data <- readRDS("../test/data/interim/target_data_.RDS")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

model_data %>%
  select(-ends_with("_flag"), -ends_with("cilow"), -ends_with("cihigh"),
         -ends_with("_numerator"), -ends_with("denominator"),
         - c("county_ranked", "statecode", "countycode", "fipscode")) %>%
  filter(state != "US")

data_raw_only = model_data %>% select(-contains("_race"), - county, -contains("_other_data")) %>%
  mutate(across(where(is.character), as.factor))

set.seed(1)
split = initial_split(data_raw_only, prop=0.7, strata="v005_rawvalue", breaks=5)
train_data = training(split)
test_data = testing(split)

# Define a recipe for the training data without imputation
rec1 <- recipe(v005_rawvalue ~ ., data = train_data) %>%
  prep(data = train_data)

# Define a recipe for the testing data with imputation
rec2 <- recipe(v005_rawvalue ~ ., data = test_data) %>%
  step_impute_median(all_numeric(), -all_outcomes()) %>%
  step_impute_mode(all_nominal(), -all_outcomes()) %>%
  prep(data = test_data)

# Apply the preprocessing to training and testing data
train_data <- bake(rec1, new_data = train_data)
test_data <- bake(rec2, new_data = test_data)


folds <- vfold_cv(train_data, v = 10)

elastic_net_spec <- linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = tune(),
  mixture = tune()
)

library(doParallel)
cl <- makeCluster(parallel::detectCores())
registerDoParallel(cl)


elnet_grid <- grid_regular(penalty() %>% range_set(c(0.1, 4)) , mixture(),  levels = 10) 

elnet_resample <- tune_grid(
  elastic_net_spec,
  v005_rawvalue ~ .,
  resamples = folds,
  grid = elnet_grid
)

