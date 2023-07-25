#we need to remove vars that has less than 70% existing in all counties
#July 14th 
library(tidyverse)
library(VIM)
library(dplyr)

# 1. Read Data
data_chrs23 <- readRDS("./data/interim/analytic2023_c1.rds")
# Reading in the description table
desc = readRDS("./data/processed/colDesc_analysis2023.rds")


# 2. Gather all vars: remove stuff we know will not be used
data_core = data_chrs23 %>%
  select(-ends_with("_flag"), -ends_with("cilow"), -ends_with("cihigh"),
         -ends_with("_numerator"), -ends_with("denominator"),
         - c("county_ranked", "statecode", "countycode", "fipscode")) %>%
  filter(state != "US") 

# check all vars in data
check_na = data_core %>% aggr(., plot=FALSE)
rownames(check_na$missings) = sub("_rawvalue", "", desc[check_na$missings$Variable])


check_na$missings %>% 
  mutate(Rate = round(Count / nrow(data_chrs23),2)) %>%
  arrange(desc(Count))
check_na$missings %>% count()


target <- check_na$missings %>% 
  mutate(Rate = Count / nrow(data_chrs23)) %>%
  arrange(desc(Count)) %>% filter(Rate >= 0.70)
target %>% count()

nontarget <- check_na$missings %>% 
  mutate(Rate = Count / nrow(data_chrs23)) %>%
  arrange(desc(Count)) %>% filter(Rate < 0.70)
nontarget %>% count()


#3. Save target data i.e. all vars whose' data exist in at least 70% of counties
#   aka missing na's upto 30%
target_data = data_chrs23 %>% select(-one_of(target$Variable))

check_na_target_data = target_data %>% aggr(., plot=FALSE)
rownames(check_na_target_data$missings) = sub("_rawvalue", "", desc[check_na_target_data$missings$Variable])
check_na_target_data$missings%>% 
  mutate(Rate = round(Count / nrow(target_data),2)) %>%
  arrange(desc(Count)) 


saveRDS(target_data, "../test/data/interim/target_model_data.rds")