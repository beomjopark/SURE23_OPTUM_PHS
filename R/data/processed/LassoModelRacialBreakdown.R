# Modelling with Tidymodels
# Example for LASSO
# Predict PHS_race_hispnaic ~ .
library(tidyverse)
library(ggplot2)
library(VIM)

# 1. Read Data
data_chrs23 <- readRDS("./data/interim/analytic2023_c1.rds")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

# We will only focus on raw values
data_core = data_chrs23 %>%
  select(-ends_with("_flag"), -ends_with("cilow"), -ends_with("cihigh"),
         -ends_with("_numerator"), -ends_with("denominator"),
         - c("county_ranked", "statecode", "countycode", "fipscode")) %>%
  filter(state != "US")

data_race_hispanic_only = data_core %>% select(- contains("black"), - contains("white"), - contains("asian"), - contains("aian"),-county) %>%
  mutate(across(where(is.character), as.factor))

data_race_white_only = data_core %>% select(- contains("black"), - contains("hispanic"), - contains("asian"), - contains("aian"),-county) %>%
  mutate(across(where(is.character), as.factor))

data_race_black_only = data_core %>% select(- contains("white"), - contains("hispanic"), - contains("asian"), - contains("aian"),-county) %>%
  mutate(across(where(is.character), as.factor))

data_race_asian_only = data_core %>% select(- contains("black"), - contains("hispanic"), - contains("white"), - contains("aian"),-county) %>%
  mutate(across(where(is.character), as.factor))

data_race_aian_only = data_core %>% select(- contains("black"), - contains("hispanic"), - contains("asian"), - contains("white"),-county) %>%
  mutate(across(where(is.character), as.factor))

data_by_race <- c(data_race_hispanic_only, data_race_white_only, data_race_black_only, data_race_asian_only, data_race_aian_only)

# 2. Modeling Pipeline
library(tidymodels)
library(glmnet)

# 2.1. Define Receipe specification (saturated model)
target_name = "v005_rawvalue"
for(var in data_by_race){
  
  recipe_sat = 
    recipe(v005_rawvalue~ ., data=) %>%
    step_naomit(all_predictors()) %>%
    #  step_log(all_numeric_predictors(), offset=1) %>%
    step_dummy(all_nominal_predictors(), one_hot=TRUE)
  
}

# 2.2. Define data splits
set.seed(1)
for(var in data_by_race){
  split = initial_split(var, prop=0.7, strata=target_name, breaks=5)
}

train_data = training(split)
test_data = testing(split)

# Check state imbalance
data.frame(test_ratio = round(test_data$state %>% table / nrow(test_data) * 100, 2),
           train_ratio = round(train_data$state %>% table / nrow(train_data) * 100, 2))

# 2.3 Define model engine
engine_lasso = linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression") %>%
  translate()

# 2.4. Bind the Workflow
workflow = workflow() %>%
  add_model(engine_lasso) %>%
  add_recipe(recipe_sat)

# 2.5 Hyperparameter tuning with cross-validation
## Create 5-folds
resampling = vfold_cv(train_data, v=5, strata = target_name)

## Parallel process to lift computation burden
library(doParallel)
cl <- makeCluster(parallel::detectCores())
registerDoParallel(cl)

## Define search grid
param_grid <- grid_regular( 
  penalty(), 
  levels = 40
)

tuned_model = tune_grid(
  workflow,
  resamples = resampling,
  metrics=metric_set(rmse, mae, huber_loss),
  grid = param_grid,
  control = control_grid(allow_par=TRUE, save_pred=TRUE, parallel_over = "resamples")
)
collect_metrics(tuned_model)

## Check the search performance
tuned_model %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme(legend.position = "none")


# Finalize the model and fit the model
model_fitted = workflow %>% 
  finalize_workflow(select_best(tuned_model, metric="huber_loss")) %>%
  fit(train_data)

# Evaluate OOS Performance
test_prediction = predict(model_fitted,
                          new_data = test_data)
test_prediction = test_prediction %>%
  mutate(truth = test_data[[target_name]])

# Check calibration
with(test_prediction,
     plot(truth, .pred, pch=".", cex=2))
abline(a=0, b=1, col="red")

eval_metric = metric_set(rmse, mae, huber_loss)
eval_metric(
  data = test_prediction,
  truth = truth,
  
  estimate = .pred
)

# Assuming 'test_prediction' contains the truth and predicted values

