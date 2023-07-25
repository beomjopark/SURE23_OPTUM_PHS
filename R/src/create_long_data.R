library(tidyverse)
library(VIM)
library(dplyr)

# 1. Read Data
data_chrs23 <- readRDS("./data/interim/analytic2023_c1.rds")
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

# 2. Gather all vars: remove stuff we know will not be used
data_core = data_chrs23 %>%
  select(-ends_with("_flag"), -ends_with("cilow"), -ends_with("cihigh"),
         -ends_with("_numerator"), -ends_with("denominator"),
         -contains("_other_data"),
         - c("county_ranked", "statecode", "countycode", "fipscode")) %>%
  filter(state != "US") 

# Check the NA status of target v005
data_core$v005_rawvalue %>% is.na %>% table
data_core %>% select(starts_with("v005_race_")) %>% apply(.,2,is.na) %>% rowSums %>% table

# Make long form
data_core_long <- data_core %>% #select(contains("_race_")) %>%
  pivot_longer(cols = contains("_race_"),
               names_to = c(".value", "race"),  names_pattern = "(v.*_race)_(.*)")

# Drop NAs for v005
data_core_long = data_core_long %>% drop_na("v005_race")

# Now check the NA trends
check_na_target_data = data_core_long %>% aggr(., plot=FALSE)
#rownames(check_na_target_data$missings) = sub("_rawvalue", "", desc[check_na_target_data$missings$Variable])
check_na_target = check_na_target_data$missings%>% 
  mutate(Rate = round(Count / nrow(data_core_long),2)) %>%
  arrange(desc(Count)) 

# Check unsafe variables
unsafeVars = check_na_target %>% filter(Rate > 0.3) %>% pull(Variable)
desc[unsafeVars[unsafeVars %>% endsWith("_rawvalue")]]
desc[sub("_race","_rawvalue", unsafeVars[!(unsafeVars %>% endsWith("_rawvalue"))])]


saveRDS(data_core_long %>% select(-any_of(unsafeVars)), "./data/processed/data_core_long_30p.rds")