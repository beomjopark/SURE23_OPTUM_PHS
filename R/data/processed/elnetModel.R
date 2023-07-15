# Modelling with Tidymodels
# Example for LASSO
# Predict PHS_rawvalue ~ .
library(tidyverse)
library(ggplot2)
library(VIM)
library(reshape2)
library(dplyr)
library(ggplot2)    # used for plotting
library(tidymodels) #used for models
library(glmnet) #used for lasso

# 1. Read Data
data_chrs23 <- readRDS("./data/interim/target_data.RDS")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

# We will only focus on raw values
# we want to predict v005_rawvalue
data_core = data_chrs23 %>%
  select(-ends_with("_flag"), -ends_with("cilow"), -ends_with("cihigh"),
         -ends_with("_numerator"), -ends_with("denominator"),
         - c("county_ranked", "statecode", "countycode", "fipscode")) %>%
  filter(state != "US")

data_raw_only = data_core %>% select(-contains("_race"), - county, -contains("_other_data")) %>%
  mutate(across(where(is.character), as.factor))

require(VIM)
check_na = data_raw_only %>% aggr(., plot=FALSE)
rownames(check_na$missings) = sub(" raw value", "", desc[check_na$missings$Variable])
check_na$missings %>% 
  mutate(Rate = round(Count / nrow(data_raw_only),2)) %>%
  arrange(desc(Count))

data_race_black_only = data_core %>% select(- contains("white"), - contains("hispanic"), - contains("asian"), - contains("aian"),-county) %>%
  mutate(across(where(is.character), as.factor))

check_na = data_race_black_only %>% aggr(., plot=FALSE)
rownames(check_na$missings) = sub(" raw value", "", desc[check_na$missings$Variable])
check_na$missings %>% 
  mutate(Rate = round(Count / nrow(data_raw_only),2)) %>%
  arrange(desc(Count))


#
#
#
#
train_data = training(split)
test_data = testing(split)

#Define data splits
set.seed(52)
folds <- vfold_cv(train_data, v = 10)
split = initial_split(data_raw_only, prop=0.7, strata="v005_rawvalue", breaks=5)



rec <- recipe(v005_rawvalue ~ ., data = train_data) %>%
  step_naomit(all_predictors()) %>%
  prep(data = train_data)

#after the recipe, there was still na values in the train and test data
#train_data <- na.omit(train_data)
#test_data <- na.omit(test_data)

#not sure if we should include bake
train_data <- bake(rec, new_data = train_data)
test_data <- bake(rec, new_data = test_data)

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

elnet_resample %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(mixture = factor(mixture)) %>%
  ggplot(aes(x = penalty,
             y = mean,
             color = mixture)) +
  geom_line(size = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  scale_x_log10(labels =
                  scales::label_number()) +
  theme_bw()

best_elnet <- elnet_resample %>%
  select_best("rmse")

final_elnet <- linear_reg(engine = "glmnet", 
                          penalty = best_elnet$penalty, 
                          mixture = best_elnet$mixture) %>%
  fit(v005_rawvalue ~ ., data = train_data)


test_prediction = predict(final_elnet,
                          new_data = test_data)

test_prediction = test_prediction %>%
  mutate(truth = test_data[["v005_rawvalue"]])


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
 

print(nrow(test_prediction))
test_prediction %>% is.na()

print(sum(is.na(test_prediction)))
print(sum(is.na(test_prediction$.pred)))




