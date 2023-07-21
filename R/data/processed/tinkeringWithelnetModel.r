
library(tidyverse)
library(ggplot2)
library(VIM)
library(reshape2)
library(dplyr)
library(ggplot2)    # used for plotting
library(tidymodels) #used for models
library(glmnet) #used for lasso
library(recipes)

# Boosting
library(bonsai)
library(lightgbm)


# 1. Read Data
model_data <- readRDS("../test/data/interim/target_data_.RDS")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

model_data = model_data %>%
  select(-ends_with("_flag"), -ends_with("cilow"), -ends_with("cihigh"),
         -ends_with("_numerator"), -ends_with("denominator"),
         - c("county_ranked", "statecode", "countycode", "fipscode")) %>%
  filter(state != "US") %>% drop_na("v005_rawvalue")

data_raw_only = model_data %>% select(-contains("_race"), - county, -contains("_other_data")) %>%
  mutate(across(where(is.character), as.factor))

set.seed(1)
split = initial_split(data_raw_only, prop=0.7, strata="v005_rawvalue", breaks=5)
train_data = training(split)
test_data = testing(split)

# Define a recipe for the training data without imputation
rec1 <- recipe(v005_rawvalue ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors(), one_hot=TRUE)

# Define a recipe for the testing data with imputation
rec2 <- recipe(v005_rawvalue ~ ., data = test_data) %>%
#  step_dummy(all_nominal_predictors(), one_hot=TRUE)  %>%
  step_impute_median(all_numeric_predictors()) #%>%
#  step_impute_mode(all_nominal(), -all_outcomes()) %>%
#  prep(data = test_data)



# Apply the preprocessing to training and testing data
#train_data <- bake(rec1, new_data = train_data)
#test_data <- bake(rec2, new_data = test_data)


folds <- vfold_cv(train_data, v = 10)

elastic_net_spec <- linear_reg(
  mode = "regression",
  engine = "glmnet",
  penalty = tune(),
  mixture = tune()
)


model_lgbm = boost_tree(
  mtry = tune(),
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  # learn_rate = tune()
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  translate()



workflow = workflow() %>%
  add_model(elastic_net_spec) %>%
  add_recipe(rec1)

workflow_boosting = workflow() %>%
  add_model(model_lgbm) %>%
  add_recipe(rec1)

library(doParallel)
cl <- makeCluster(parallel::detectCores())
registerDoParallel(cl)


elnet_grid <- grid_regular(penalty() %>% range_set(c(0.1, 4)) , mixture(),  levels = 10) 

elnet_resample <- tune_grid(
  workflow,
  resamples = folds,
  grid = elnet_grid, 
  metrics=metric_set(rmse, mae, huber_loss),  
  control = control_grid(allow_par=TRUE, save_pred=TRUE, parallel_over = "resamples")
)


param_grid_boosting <- grid_max_entropy(
  mtry() %>% range_set(c(2, 10)),
  trees(),# %>% range_set(c(500, 1000)), 
  tree_depth(),# %>% range_set(c(10, 1000)), 
  min_n(), #%>% range_set(c(5, 15)),
  size = 20
)


tuned_model = tune_grid(
  workflow_boosting,
  resamples = folds,
  metrics=metric_set(rmse, mae, huber_loss),
  grid = param_grid_boosting,
  control = control_grid(allow_par=TRUE, save_pred=TRUE, parallel_over = "resamples")
)
collect_metrics(tuned_model)
autoplot(tuned_model)

collect_metrics(tuned_model) %>% filter(.metric == "mae")


model_fitted_boosting = workflow_boosting %>% 
  finalize_workflow(select_best(tuned_model, metric="rmse")) %>%
  fit(train_data)

# Evaluation
test_bake = rec2 %>% prep() %>% bake(new_data = test_data)
test_prediction = predict(model_fitted_boosting, new_data = test_bake)

target_name = "v005_rawvalue"
test_prediction = test_prediction %>%
  mutate(truth = test_data[[target_name]])

# Check calibration
with(test_prediction,
     plot(truth, .pred, pch=".", cex=2))
abline(a=0, b=1, col="red")


eval_metric = metric_set(rmse, mae, huber_loss, rsq)
eval_metric(
  data = test_prediction,
  truth = truth,
  estimate = .pred
)

# SHAP
library(shapviz)
train_mat = rec1 %>% prep() %>%
  bake(has_role("predictor"), new_data=train_data, composition = "matrix")
shp = shapviz(model_fitted_boosting %>% extract_fit_engine(),
              X_pred = train_mat)

p = sv_importance(shp, kind = "bar", max_display = 10)
p$data = 
  p$data %>%
  mutate(feature =
           fct_recode(feature,
                      !!!setNames(levels(p$data$feature),
                                  sub(" raw value", "", desc[levels(p$data$feature)] %>% unlist)))) 
p


