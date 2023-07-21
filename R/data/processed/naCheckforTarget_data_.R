library(tidyverse)
library(ggplot2)
library(VIM)
library(reshape2)
library(dplyr)
library(ggplot2)    # used for plotting
library(tidymodels) #used for models
library(glmnet) #used for lasso

model_data <- readRDS("../test/data/interim/target_data_.RDS")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

model_data = model_data %>%
  select(-ends_with("_cilow"), -ends_with("_cihigh")) %>% filter(state != "US")

#check for na's
check_na = model_data %>% aggr(., plot=FALSE)
rownames(check_na$missings) = sub("", "", desc[check_na$missings$Variable])
check_na$missings %>% 
  mutate(Rate = round(Count / nrow(model_data),2)) %>%
  arrange(desc(Count)) 

#the following table is just for aesthetic view of vars, not good to use
varswithlessthan30percMissing <- tibble(check_na$missings %>% 
         mutate(Rate = round(Count / nrow(model_data),2)) %>%
         arrange(desc(Count)) )
saveRDS(varswithlessthan30percMissing, "../test/data/interim/varswithlessthan30percMissing.rds")