# LightGBM - RAWVALUE ONLY
library(tidyverse)
library(ggplot2)
library(VIM)
data_chrs23 <- readRDS("./data/processed/data_core_long_30p.rds")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

race_vars = data_chrs23 %>% select(starts_with("v"), -ends_with("_rawvalue"))  %>% colnames 
desc[sub("_race", "_rawvalue", race_vars)]
sub("_race", "_rawvalue", race_vars) %in% colnames(data_chrs23) # all race subgroup var has rawvalue

data_processed = data_chrs23 %>% select(-ends_with("_race"), "v005_race", -v005_rawvalue, -county) %>%
  mutate(across(where(is.character), as.factor))

# All subgroup variable
data_processed = data_chrs23 %>% select(-sub("_race", "_rawvalue", race_vars), -county) %>%
  mutate(across(where(is.character), as.factor))

# tidymodel
library(tidymodels)
library(bonsai)
library(lightgbm)

# Define Receipe and splits
target_name = "v005_race"

set.seed(1)
split = initial_split(data_processed, prop=0.7, strata=target_name, breaks=5)

train_data = training(split)
test_data = testing(split)

recipe_sat = 
  recipe(v005_race ~ ., data=train_data) %>%
  #  step_log(all_numeric_predictors(), offset=1) %>%
  step_relevel(race, ref_level = "white") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# Check state imbalance
data.frame(test_ratio = round(test_data$state %>% table / nrow(test_data) * 100, 2),
      train_ratio = round(train_data$state %>% table / nrow(train_data) * 100, 2))

# Define model spec
model_lgbm = boost_tree(
  mtry = tune(),
  trees = tune(),
  learn_rate = 0.05,
#  tree_depth = tune(),
#  min_n = 0
#  min_n = tune(),
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
  mtry() %>% range_set(c(5, 75)),
  trees() %>% range_set(c(40, 100)), 
  #  tree_depth() %>% range_set(c(500, 1000)), 
  #  min_n(), #%>% range_set(c())
  size = 30
)

# Raw only
param_grid <- grid_max_entropy(
  mtry() %>% range_set(c(5, 75)),
  trees() %>% range_set(c(40, 100)), 
  #  tree_depth() %>% range_set(c(500, 1000)), 
  #  min_n(), #%>% range_set(c())
  size = 30
)

# Race only
param_grid <- grid_max_entropy(
  mtry() %>% range_set(c(5, 75)),
  trees() %>% range_set(c(60, 200)), 
  #  tree_depth() %>% range_set(c(500, 1000)), 
  #  min_n(), #%>% range_set(c())
  size = 30
)
tuned_model = tune_grid(
  workflow,
  resamples = resampling,
  metrics=metric_set(rmse, mae, huber_loss),
  grid = param_grid,
  control = control_grid(allow_par=TRUE, save_pred=TRUE, parallel_over = "resamples")
)
collect_metrics(tuned_model)
autoplot(tuned_model)

# Fit the model
model_fitted = workflow %>% 
  finalize_workflow(select_best(tuned_model, metric="huber_loss")) %>%
  fit(train_data)

# Evaluate OOS Performance
rec_test <- recipe(v005_race ~ ., data = test_data) %>%
  step_impute_median(all_numeric_predictors()) #%>%

test_prediction = predict(model_fitted,
                          new_data = rec_test %>% prep() %>% bake(new_data = test_data))
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
  bake(has_role("predictor"), new_data=train_data, composition = "matrix")

train_names = colnames(train_mat)
shp = shapviz(model_fitted %>% extract_fit_engine(),
        X_pred = train_mat)

# Collapse Race
shp_collapse = shp
shp_collapse$S = collapse_shap(S=shp$S, collapse=list(Race = train_names[grep("race", train_names)]))
shp_collapse$X = shp$X %>% 
  mutate(Race = case_when(
    race_white == 1 ~ "White",
    race_black == 1 ~ "Black",
    race_asian == 1 ~ "Asian",
    race_hispanic == 1 ~ "Hispanic",
    race_aian == 1 ~ "AIAN",
    TRUE ~ NA_character_
  )) %>%
  select(-starts_with("race_"))

shp = shp_collapse

p = sv_importance(shp, kind = "bar", max_display = 10)
attribLevels = levels(p$data$feature)
attribLevels = sub("_race", "_rawvalue", attribLevels)
attribLevels[attribLevels %in% colnames(desc)] = 
  sub(" raw value", "",
      desc[attribLevels[attribLevels %in% colnames(desc)]] %>% unlist)
p$data = 
  p$data %>%
  mutate(feature =
           fct_recode(feature,
                      !!!setNames(levels(p$data$feature), attribLevels))) 
p

#p = sv_force(shp, row_id = 120, max_display = 8)
#new_label = sapply(regmatches(p$data$label, regexpr("^[^=]+", p$data$label)),
#       function(x) ifelse(is.null(desc[[x]]), x,
#                          sub(" raw value", "", desc[[x]])))
#p$data$label =
#  mapply(function(x, y) gsub("^[^=]+", x, y), new_label, p$data$label)
#p

#p = sv_waterfall(shp, row_id = 120, max_display = 10)
#new_label = sapply(regmatches(p$data$label, regexpr("^[^= ]+", p$data$label)),
#                   function(x) ifelse(is.null(desc[[x]]), x,
#                                      sub(" raw value", "", desc[[x]])))
#p$data$label =
#  mapply(function(x, y) gsub("^[^=]+", x, y), new_label, p$data$label)
#p

#p = sv_importance(shp, kind="beeswarm", alpha=0.3)
#p$data = 
#p$data %>%
#  mutate(Var2 =
#           fct_recode(Var2,
#                      !!!setNames(levels(p$data$Var2),
#                                  sub(" raw value", "", desc[levels(p$data$Var2)] %>% unlist))),
#         feature =
#           fct_recode(feature,
#                      !!!setNames(levels(p$data$feature),
#                                  sub(" raw value", "", desc[levels(p$data$feature)] %>% unlist)))
#           ) 
#p

p = sv_importance(shp, kind = "bar", max_display = 10)
query_str = levels(p$data$feature)
for(query in query_str) {
  p = sv_dependence(shp, v= query, alpha=0.2) 
  p$labels = lapply(p$labels, function(x) sub("_race", "_rawvalue", x))
  
  if(!is.null(desc[[p$labels$x]])) {
    p$labels$x = sub(" raw value", "", desc[[p$labels$x]])
  }
  if(!is.null(desc[[p$labels$colour]])) {
    p$labels$colour = sub(" raw value", "", desc[[p$labels$colour]])
  }
  if(grepl("race", p$labels$colour)) {
    p + scale_color_manual(p$labels$colour)
  }
  plot(p)
}


# Effect of Race
plotData = data.frame(Race = fct_relevel(shp$X$Race, 
                                         "AIAN", "Black", "White", "Hispanic", "Asian"),
                      shp = shp$S[,"Race"])
ggplot(plotData, aes(x=shp, fill=Race, color=Race)) +
  geom_density(alpha=0.5) +
  scale_y_continuous(expand = c(0, 0))

#library(ggbeeswarm)
ggplot(plotData, aes(x=Race, y=shp, color=Race, fill=Race)) +
  geom_violin(alpha=0.5, draw_quantiles = c(0.25, 0.5, 0.75))
#  geom_quasirandom(alpha=0.3)
  

# Effect of Insufficient sleep 
Hmisc::rcorr(as.matrix(shp$X %>% select(v143_rawvalue, v144_rawvalue, v082_rawvalue)))

p = sv_dependence(shp, v= "v143_rawvalue", alpha=0.3,
                  color_var = "v082_rawvalue",
                  viridis_args = list(begin=0, end=1)) 
p$labels$x = sub(" raw value", "", desc[[p$labels$x]])
p$labels$colour = sub(" raw value", "", desc[[p$labels$colour]])
p + geom_smooth(color="red")

# Actually Physical distress is much stronger
p = sv_dependence(shp, v= "v143_rawvalue", alpha=0.3,
                  color_var = "v144_rawvalue",
                  viridis_args = list(begin=0, end=1)) 
p$labels$x = sub(" raw value", "", desc[[p$labels$x]])
p$labels$colour = sub(" raw value", "", desc[[p$labels$colour]])
p + geom_smooth(color="red")

# For 2D
p = sv_dependence2D(shp, x= "v143_rawvalue", alpha=0.3,
                  y = "v144_rawvalue",
                  viridis_args = list(begin=0, end=1))

p$labels$x = sub(" raw value", "", desc[[p$labels$x]])
p$labels$y = sub(" raw value", "", desc[[p$labels$y]])
p 

p = sv_dependence2D(shp, x= "v143_rawvalue", alpha=0.3,
                    y = "v082_rawvalue",
                    viridis_args = list(begin=0, end=1))

p$labels$x = sub(" raw value", "", desc[[p$labels$x]])
p$labels$y = sub(" raw value", "", desc[[p$labels$y]])
p 