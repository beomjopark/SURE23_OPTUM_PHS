#we need to remove vars that has less than 70% existing in all counties
#July 14th 
library(tidyverse)
library(VIM)
library(dplyr)

# 1. Read Data
data_chrs23 <- readRDS("./data/interim/analytic2023_c1.rds")
# Reading in the description table
desc = readRDS("./data/processed/colDesc_analysis2023.rds")

# 2. check all vars in data
check_na = data_chrs23 %>% aggr(., plot=FALSE)
rownames(check_na$missings) = sub("_rawvalue", "", desc[check_na$missings$Variable])

#check for na rate; also gather count of missing na's
check_na$missings %>% 
mutate(Rate = round(Count / nrow(data_chrs23),2)) %>%
  arrange(desc(Count))
check_na$missings %>% count()

#check how many meet our 30% criteria, group those into target var
target <- check_na$missings %>% 
  mutate(Rate = round(Count / nrow(data_chrs23),2)) %>%
  arrange(desc(Count)) %>% filter(Rate >= 0.30)
#gather count of target, i.e. how many are missing >= 30
target %>% count()

# 3. Save target data i.e. all vars whose' data exist in at least 70% of counties
# aka missing Na's up-to 30%
# new data , take original data and remove any vars included in the target vars
target_data <- data_chrs23 %>% select(-one_of(target$Variable))

#we have removed those vars and now we will save this data and work with this
saveRDS(target_data, "../test/data/interim/target_data_.RDS")

#verify
#check for missing Na's of new data frame
check_na_target_data = target_data %>% aggr(., plot=FALSE)
rownames(check_na_target_data$missings) = sub("", "", desc[check_na_target_data$missings$Variable])

#check how many var with na rate over 30 exist, i.e. should be zero
check_na_target_data$missings %>% 
  mutate(Rate = round(Count / nrow(target_data),2)) %>%
  arrange(desc(Count)) %>% filter(Rate >= 0.30) %>%  count()


