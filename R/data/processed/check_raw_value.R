# Read Data and Description
analytic_data2023_0 = readRDS("./data/interim/analytic2023_c1.rds")
#analytic_data2023_0 =read_csv("data/raw/analytic_data2023_0.csv", skip=1)
description = readRDS("./data/processed/colDesc_analysis2023.rds")

# Grab proportion of racial group
prop_racial_enc = 
  c("v055_rawvalue", "v081_rawvalue", "v054_rawvalue", "v056_rawvalue", "v126_rawvalue")
description[which(colnames(description) %in% prop_racial_enc)]

racial_proportion = 
  analytic_data2023_0 %>% select(prop_racial_enc)

# Plot to check whether rawvalue is roughly the weighted average in the formula
# PHS_raw_value = sum of all PHS (PHS_numer) / # total pop (PHS_demon) 
# PHS_race      = PHS_race_all / # pop_race
# PHS_raw_value = sum_{race} (prop_race * PHS_race)
plot(
  (analytic_data2023_0 %>% select(starts_with("v005_race")) %>% 
sapply(.,
       function(x) ifelse(is.na(x), 0, x)) * as.matrix(racial_proportion) ) %>% rowSums,
analytic_data2023_0 %>% pull("v005_rawvalue"))
abline(a=0, b=1, col="red")

# Confirmed the NA are roughly zero.