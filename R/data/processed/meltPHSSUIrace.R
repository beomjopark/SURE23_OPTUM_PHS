
#add long form 
# if we have race breakdown, make into long form
#melt or pivot longer
#phs and race melt 
library(tidyverse)
library(ggplot2)
library(VIM)
library(dplyr)
library(reshape2)

# 1. Read Data
data_chrs23 <- readRDS("./data/interim/analytic2023_c1.rds")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

# We will only focus on raw values
data_core = data_chrs23 %>%
  select(-matches("_flag|cilow|cihigh|_numerator|_denominator"),
         - c("county_ranked", "statecode", "countycode", "fipscode")) %>%
  filter(state != "US")

melt_vars = data_core %>% select(starts_with("v005_race")) %>% colnames

data_phs_long <- melt(data_core, 
                  id.vars = setdiff(names(data_core), melt_vars), 
                  measure.vars = melt_vars,
                  variable.name = "race", 
                  value.name = "phs_race")

data_phs_long = data_phs_long %>%
  mutate(race = sub("v005_race_", "", data_phs_long$race))

data_phs_long = data_phs_long %>% filter(!is.na(phs_race))
table(data_phs_long$race)

# 1. receipe formula (phs_race ~.) 
# 2. step_dummy("race")

# Advanced
data_phs_long %>% head

data_core %>% select(starts_with("v001_race")) %>% colnames
data_v001_long = ...

data_phs_long %>% left_join(data_v001_long)