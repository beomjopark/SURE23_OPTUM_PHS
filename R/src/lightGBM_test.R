# LightGBM - RAWVALUE ONLY
library(tidyverse)
library(ggplot2)
library(VIM)
data_chrs23 <- readRDS("./data/interim/analytic2023_c1.rds")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")


data_core = data_chrs23 %>%
  select(-ends_with("_flag"), -ends_with("cilow"), -ends_with("cihigh"),
         -ends_with("_numerator"), -ends_with("denominator"),
         - c("county_ranked", "statecode", "countycode", "fipscode")) %>%
  filter(state != "US")

data_raw_only = data_core %>% select(-contains("_race"), - county) %>%
  mutate(across(where(is.character), as.factor))

# missing - Not so much issue for Tree Models
vim_raw = data_raw_only %>% aggr(., plot=FALSE)
rownames(vim_raw$missings) = sub(" raw value", "", desc[vim_raw$missings$Variable])
vim_raw$missings %>% 
  mutate(missing_ratio = round(Count / nrow(vim_raw$missings), 2)) %>%
  arrange(desc(Count))


# tidymodel
library(tidymodels)
library(bonsai)
library(lightgbm)

# Define Receipe and splits
target_name = "v005_rawvalue"
recipe_sat = 
  recipe(v005_rawvalue ~ ., data=data_raw_only) %>%
#  step_log(all_numeric_predictors(), offset=1) %>%
  step_dummy(all_nominal_predictors(), one_hot=TRUE)

set.seed(1)
split = initial_split(data_raw_only, prop=0.7, strata=target_name, breaks=5)

train_data = training(split)
test_data = testing(split)

# Check state imbalance
data.frame(test_ratio = round(test_data$state %>% table / nrow(test_data) * 100, 2),
      train_ratio = round(train_data$state %>% table / nrow(train_data) * 100, 2))

# Define model spec
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

# Workflow
workflow = workflow() %>% add_model(model_lgbm) %>% add_recipe(recipe_sat)

# Tune model
## Create folds
resampling = vfold_cv(train_data, v=5, strata = target_name)

library(doParallel)
cl <- makeCluster(parallel::detectCores())
registerDoParallel(cl)

param_grid <- grid_max_entropy(
  mtry() %>% range_set(c(1, 20)),
  trees() %>% range_set(c(500, 1000)), 
  tree_depth() %>% range_set(c(500, 1000)), 
  min_n(), #%>% range_set(c())
  size = 20
)

tuned_model = tune_grid(
  workflow,
  resamples = resampling,
  metrics=metric_set(rmse, mae, huber_loss),
  grid = param_grid,
  control = control_grid(allow_par=TRUE, save_pred=TRUE, parallel_over = "resamples")
)
collect_metrics(tuned_model)

# Fit the model
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

#.metric    .estimator .estimate
#<chr>      <chr>          <dbl>
#  1 rmse       standard        905.
#2 mae        standard        657.
#3 huber_loss standard        657.


## EXAMINE SHAP
library(shapviz)
train_mat = recipe_sat %>% prep() %>%
  bake(has_role("predictor"), new_data=test_data, composition = "matrix")
shp = shapviz(model_fitted %>% extract_fit_engine(),
        X_pred = train_mat)

p = sv_importance(shp, kind = "bar", max_display = 5)
p$data = 
  p$data %>%
  mutate(feature =
           fct_recode(feature,
                      !!!setNames(levels(p$data$feature),
                                  sub(" raw value", "", desc[levels(p$data$feature)] %>% unlist)))) 
p

p = sv_force(shp, row_id = 119, max_display = 8)
new_label = sapply(regmatches(p$data$label, regexpr("^[^=]+", p$data$label)),
       function(x) ifelse(is.null(desc[[x]]), x,
                          sub(" raw value", "", desc[[x]])))
p$data$label =
  mapply(function(x, y) gsub("^[^=]+", x, y), new_label, p$data$label)
p

p = sv_waterfall(shp, row_id = 119, max_display = 20)
new_label = sapply(regmatches(p$data$label, regexpr("^[^= ]+", p$data$label)),
                   function(x) ifelse(is.null(desc[[x]]), x,
                                      sub(" raw value", "", desc[[x]])))
p$data$label =
  mapply(function(x, y) gsub("^[^=]+", x, y), new_label, p$data$label)
p

p = sv_importance(shp, kind="beeswarm", alpha=0.3)
p$data = 
p$data %>%
  mutate(Var2 =
           fct_recode(Var2,
                      !!!setNames(levels(p$data$Var2),
                                  sub(" raw value", "", desc[levels(p$data$Var2)] %>% unlist))),
         feature =
           fct_recode(feature,
                      !!!setNames(levels(p$data$feature),
                                  sub(" raw value", "", desc[levels(p$data$feature)] %>% unlist)))
           ) 
p

p = sv_importance(shp, kind = "bar", max_display = 5)
query_str = levels(p$data$feature)
for(query in query_str) {
  p = sv_dependence(shp, v= query, alpha=0.2) 
  p$labels$x = sub(" raw value", "", desc[[p$labels$x]])
  p$labels$colour = sub(" raw value", "", desc[[p$labels$colour]])
  plot(p)
}
 
p = sv_dependence(shp, v= "v054_rawvalue", alpha=0.2,color_var = NULL) 
p$labels$x = sub(" raw value", "", desc[[p$labels$x]])
p$labels$colour = sub(" raw value", "", desc[[p$labels$colour]])
plot(p)



